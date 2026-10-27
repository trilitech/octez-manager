#!/usr/bin/env python3
"""Use the qualified existing AST analyzer; exercise each refusal beyond hash checks."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent.parent
ADAPTER = ROOT / "scripts/check-integration-pipefail.py"
if not os.environ.get("SHELLCHECK_BINARY"):
    print("SHELLCHECK_BINARY must name the qualified, hash-verified checker", file=sys.stderr)
    raise SystemExit(2)
CHECKER = Path(os.environ["SHELLCHECK_BINARY"])


class IntegrationPipefail(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="pipefail-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.sources = self.root / "test/integration"
        self.sources.mkdir(parents=True)
        self.source = self.sources / "lib.sh"
        self.source.write_text("output=$(printf 'ready\\n')\n[[ \"$output\" == ready ]]\n")

    def check(self, expected, checker=CHECKER, lock=None, environment=None):
        command = [sys.executable, str(ADAPTER), "--root", str(self.root), "--shellcheck", str(checker)]
        if lock:
            command += ["--lock", str(lock)]
        result = subprocess.run(command, capture_output=True, text=True,
                                env={**os.environ, **(environment or {})})
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
        return result

    def lock_for(self, checker):
        lock = self.root / "fixture-lock.json"
        lock.write_text(json.dumps({"sha256": hashlib.sha256(checker.read_bytes()).hexdigest()}))
        return lock

    def fake(self, body):
        checker = self.root / "fake-checker"
        checker.write_text("#!" + sys.executable + "\n" + body)
        checker.chmod(0o755)
        return checker

    def test_safe(self):
        self.assertEqual(json.loads(self.check(0).stdout)["files"], 1)

    def test_sourced_library_unsafe_even_without_own_pipefail(self):
        self.source.write_text("printf 'early\\n' | grep -q early\n")
        result = self.check(1, environment={"SHELLCHECK_OPTS": "--exclude=SC2337"})
        report = json.loads(result.stdout)
        self.assertTrue(any(x["code"] == 2337 and x["source"].endswith("lib.sh") for x in report["findings"]))

    def test_multiline_unsafe(self):
        self.source.write_text("printf 'early\\n' | \\\n    grep -q early\n")
        self.check(1)

    def test_unrelated_warning_is_not_a_new_lint_campaign(self):
        self.source.write_text("echo $unquoted\n")
        self.check(0)

    def test_malformed_is_refused_before_rule_filter(self):
        self.source.write_text("if\n")
        self.assertIn("could not analyze", self.check(2).stderr)

    def test_suppressions(self):
        for code in ["SC2337", "2337", "all", "SC2046,SC2337"]:
            with self.subTest(code=code):
                self.source.write_text("# shellcheck disable=" + code + "\nprintf 'x' | grep -q x\n")
                self.assertIn("suppression", self.check(2).stderr)

    def test_inline_suppression(self):
        self.source.write_text("printf x # shellcheck disable=SC2337\n")
        self.assertIn("suppression", self.check(2).stderr)

    def test_unrelated_suppression_is_allowed(self):
        self.source.write_text("# shellcheck disable=SC2086\necho $unquoted\n")
        self.check(0)

    def test_review_boundary_ratchet(self):
        result = subprocess.run([sys.executable, str(ROOT / "test/check_integration_pipefail_boundaries.py"),
                                 "--adapter", str(ADAPTER), "--shellcheck", str(CHECKER)],
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_directive_values_are_not_disable_keys(self):
        self.source.write_text('# shellcheck source="disable=SC2337" disable=SC2086\necho ready\n')
        self.check(0)

    def test_empty(self):
        self.source.unlink()
        self.assertIn("empty", self.check(2).stderr)

    def test_unreadable(self):
        self.source.chmod(0)
        self.assertIn("readable regular", self.check(2).stderr)

    def test_source_symlink(self):
        self.source.unlink()
        self.source.symlink_to(ROOT / "test/integration/cli-tester/tests/lib.sh")
        self.assertIn("regular", self.check(2).stderr)

    def test_directory_symlink(self):
        (self.sources / "linked").symlink_to(ROOT / "test/integration", target_is_directory=True)
        self.assertIn("symlinked", self.check(2).stderr)

    def test_unavailable(self):
        self.check(2, checker=self.root / "missing")

    def test_wrong_hash(self):
        checker = self.fake("print('[]')\n")
        self.assertIn("SHA256", self.check(2, checker=checker).stderr)

    def test_incapable_after_hash_qualification(self):
        checker = self.fake("print('[]')\n")
        self.assertIn("lacks SC2337", self.check(2, checker=checker, lock=self.lock_for(checker)).stderr)

    def test_bad_json_after_hash_qualification(self):
        checker = self.fake("print('not json')\n")
        self.assertIn("JSON", self.check(2, checker=checker, lock=self.lock_for(checker)).stderr)

    def test_tool_failure_after_hash_qualification(self):
        checker = self.fake("raise SystemExit(3)\n")
        self.assertIn("execution failed", self.check(2, checker=checker, lock=self.lock_for(checker)).stderr)

    def test_diagnostic_shape_after_hash_qualification(self):
        checker = self.fake("import json\nprint(json.dumps([{'code':2337}]))\nraise SystemExit(1)\n")
        self.assertIn("malformed", self.check(2, checker=checker, lock=self.lock_for(checker)).stderr)

    def test_inconsistent_exit(self):
        checker = self.fake("print('[]')\nraise SystemExit(1)\n")
        self.assertIn("inconsistent", self.check(2, checker=checker, lock=self.lock_for(checker)).stderr)

    def test_selected_source_not_followed_is_infrastructure(self):
        checker = self.fake("""import json, pathlib, sys
path = pathlib.Path(sys.argv[-1])
text = path.read_text()
if 'unsafe.sh' in text:
    records = [dict(code=2337,line=1,file='unsafe.sh',message='early reader',level='warning')]
elif 'safe.sh' in text:
    records = []
else:
    records = [dict(code=1091,line=2,file=str(path),message='source unavailable',level='info')]
print(json.dumps(records))
raise SystemExit(int(bool(records)))
""")
        self.assertIn("could not analyze", self.check(2, checker=checker, lock=self.lock_for(checker)).stderr)

    def test_host_rc_cannot_disable_rule(self):
        (self.sources / ".shellcheckrc").write_text("disable=SC2337\n")
        self.source.write_text("printf x | grep -q x\n")
        self.check(1)

    def test_stable_checker_is_incapable_even_with_matching_fixture_hash(self):
        stable = shutil.which("shellcheck")
        known_version = subprocess.run([stable, "--version"], capture_output=True, text=True).stdout if stable else ""
        if stable and "version: 0.11.0\n" in known_version and Path(stable).resolve() != CHECKER.resolve():
            self.assertIn("lacks SC2337", self.check(2, checker=Path(stable), lock=self.lock_for(Path(stable))).stderr)
        else:
            self.skipTest("known incapable stable 0.11.0 is not installed; fake capability refusal remains mandatory")


if __name__ == "__main__":
    unittest.main()
