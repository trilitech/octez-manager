#!/usr/bin/env python3
"""Bounded SC2337 policy over regular test/integration/**/*.sh, not dynamic closure."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import stat
import subprocess
import sys
import tempfile


class Refusal(Exception):
    """Input/tool failure; never assertion-backed red evidence."""


def suppresses_2337(directive):
    for token in shlex.split(directive):
        key, separator, value = token.partition("=")
        if key != "disable" or not separator:
            continue
        for code in value.lower().split(","):
            if code == "all":
                return True
            match = re.fullmatch(r"(?:sc)?(\d+)(?:-(?:sc)?(\d+))?", code)
            if match:
                lower = int(match[1])
                upper = int(match[2]) if match[2] else lower
                if lower <= 2337 <= upper:
                    return True
    return False


def inventory(root):
    directory = root / "test/integration"
    if any(path.is_symlink() for path in (root, root / "test", directory)) or not directory.is_dir():
        raise Refusal("integration source directory unavailable or symlinked")
    files = []
    def failed(error):
        raise error
    for parent, directories, names in os.walk(directory, onerror=failed):
        for name in directories:
            if (Path(parent) / name).is_symlink():
                raise Refusal("symlinked integration directory: " + str(Path(parent) / name))
        for name in names:
            if not name.endswith(".sh"):
                continue
            source = Path(parent) / name
            mode = source.lstat().st_mode
            if not stat.S_ISREG(mode) or not mode & 0o444:
                raise Refusal("source must be a readable regular file: " + str(source))
            text = source.read_text(encoding="utf-8")
            for directive in re.findall(r"#\s*shellcheck\s+([^\n]*)", text):
                if suppresses_2337(directive):
                    raise Refusal("SC2337/all suppression is forbidden: " + str(source))
            files.append(source)
    if not files:
        raise Refusal("empty integration shell inventory")
    return sorted(files)


def analyze(checker, source, temporary):
    # Source the actual file with explicit pipefail context, preserving its path/lines.
    driver = temporary / "driver.sh"
    driver.write_text("set -o pipefail\nsource " + shlex.quote(str(source)) + "\n")
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith("SHELLCHECK_") and key not in {"ENV", "BASH_ENV"}}
    result = subprocess.run([str(checker), "--norc", "--shell=bash", "--format=json",
                             "--check-sourced", "--external-sources", str(driver)],
                            capture_output=True, text=True, env=environment, timeout=30)
    if result.returncode not in (0, 1):
        raise Refusal("ShellCheck execution failed: " + str(result.returncode) + " " + result.stderr)
    try:
        records = json.loads(result.stdout)
    except ValueError as error:
        raise Refusal("invalid ShellCheck JSON") from error
    if not isinstance(records, list):
        raise Refusal("ShellCheck JSON must be a diagnostic array")
    for record in records:
        if (not isinstance(record, dict) or type(record.get("code")) is not int
                or type(record.get("line")) is not int or record["line"] < 1
                or not isinstance(record.get("file"), str)
                or not isinstance(record.get("message"), str)
                or record.get("level") not in {"error", "warning", "info", "style"}):
            raise Refusal("malformed ShellCheck diagnostic")
    if bool(records) != (result.returncode == 1):
        raise Refusal("inconsistent ShellCheck exit/diagnostics")
    # Sourced parse failures are downgraded by ShellCheck to warning SC1094.
    # They still mean the selected source was skipped and must refuse.
    if any(record["level"] == "error" or record["code"] == 1094
           or (record["code"] in {1090, 1091} and record["file"] == str(driver))
           for record in records):
        raise Refusal("ShellCheck could not analyze " + str(source) + ": " + result.stdout)
    return records


def qualify(checker, lock, temporary):
    if hashlib.sha256(checker.read_bytes()).hexdigest() != lock["sha256"]:
        raise Refusal("ShellCheck binary SHA256 does not match lock")
    unsafe = temporary / "unsafe.sh"
    unsafe.write_text("printf 'early\\n' | grep -q early\n")
    records = analyze(checker, unsafe, temporary)
    if not any(record["code"] == 2337 for record in records):
        raise Refusal("checker lacks SC2337 in sourced pipefail context")
    safe = temporary / "safe.sh"
    safe.write_text("output=$(printf 'early\\n')\n[[ \"$output\" == early ]]\n")
    if analyze(checker, safe, temporary):
        raise Refusal("safe capability control was not clean")


def run(root, checker, lock):
    files = inventory(root)
    with tempfile.TemporaryDirectory(prefix="integration-pipefail-") as folder:
        temporary = Path(folder)
        qualify(checker, lock, temporary)
        findings = []
        for source in files:
            records = analyze(checker, source, temporary)
            findings.extend({**record, "source": str(source.relative_to(root))}
                            for record in records if record["code"] == 2337)
    print(json.dumps({"files": len(files), "findings": findings}, indent=2))
    return int(bool(findings))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parent.parent)
    parser.add_argument("--shellcheck", type=Path, required=True)
    parser.add_argument("--lock", type=Path, default=Path(__file__).with_name("integration-shellcheck.lock.json"))
    options = parser.parse_args()
    try:
        lock = json.loads(options.lock.read_text())
        if not isinstance(lock, dict) or not isinstance(lock.get("sha256"), str) or not re.fullmatch("[0-9a-f]{64}", lock["sha256"]):
            raise Refusal("invalid checker lock SHA256")
        return run(options.root.absolute(), options.shellcheck.absolute(), lock)
    except (Refusal, OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        print("integration-pipefail: REFUSED: " + str(error), file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
