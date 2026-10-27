#!/usr/bin/env python3
"""Standalone review ratchet: 0 pass, 1 policy assertion, 2 setup/tool failure."""
import argparse
from pathlib import Path
import subprocess
import sys
import tempfile


def check(adapter, checker, case):
    with tempfile.TemporaryDirectory(prefix="pipefail-boundaries-") as folder:
        root = Path(folder) / "repo"
        sources = root / "test/integration"
        sources.mkdir(parents=True)
        source = sources / "safe.sh"
        safe = "output=$(printf ready)\n[[ \"$output\" == ready ]]\n"

        def run(expected, label, setup=False, literal_data=False):
            result = subprocess.run([sys.executable, str(adapter), "--root", str(root),
                                     "--shellcheck", str(checker)], capture_output=True, text=True)
            if result.returncode != expected:
                false_refusal = (literal_data and result.returncode == 2 and
                                 ("suppression is forbidden" in result.stderr or
                                  "No closing quotation" in result.stderr))
                if setup or (result.returncode >= 2 and not false_refusal):
                    raise RuntimeError(label + ": " + result.stderr)
                raise AssertionError(f"{label}: expected {expected}, got {result.returncode}: "
                                     + result.stdout + result.stderr)

        source.write_text(safe)
        run(0, "regular directories and qualified tool", setup=True)
        if case in {"all", "ranges"}:
            for codes in ("SC2330-SC2340", "SC2000-SC3000", "2330-2340",
                          "SC2337-SC2337", "SC2086,SC2330-SC2340"):
                source.write_text("# shellcheck disable=" + codes + "\nprintf x | grep -q x\n")
                run(2, "covering range " + codes)
            for codes in ("SC2330-SC2336", "SC2338-SC2340", "SC2000-SC2200"):
                source.write_text("# shellcheck disable=" + codes + "\n" + safe)
                run(0, "noncovering range " + codes)
                source.write_text("# shellcheck disable=" + codes + "\nprintf x | grep -q x\n")
                run(1, "noncovering range retains unsafe finding " + codes)
        if case in {"all", "comments"}:
            data = [
                "cat <<'EOF'\n# shellcheck disable=SC2337\nEOF\n",
                "cat <<'# shellcheck disable=SC2337'\nhello\n# shellcheck disable=SC2337\n",
                "printf '%s\\n' '# shellcheck disable=SC2086'\n",
                'printf "%s\\n" "# shellcheck disable=SC2337"\n',
                "printf '%s\\n' $'# shellcheck disable=SC2337'\n",
                'printf "%s\\n" $"# shellcheck disable=SC2337"\n',
                r"printf '%s\n' \#\ shellcheck\ disable=SC2337" + "\n",
                "printf '%s\\n' 'first\n# shellcheck disable=SC2337\nlast'\n",
                'printf "%s\\n" "first\n# shellcheck disable=SC2337\nlast"\n',
                "cat <<EOF\n# shellcheck disable=SC2337\nEOF\n",
                "cat <<-EOF\n\t# shellcheck disable=SC2337\n\tEOF\n",
                "cat <<-'EOF'\n\t# shellcheck disable=SC2337\n\tEOF\n",
                'echo "$(printf "%s\\n" "# shellcheck disable=SC2337")"\n',
                "echo `printf '%s\\n' '# shellcheck disable=SC2337'`\n",
                "cat <<EOF\n$(printf '%s\\n' '# shellcheck disable=SC2337')\nEOF\n",
                "printf '%s\\n' $'escaped \\' # shellcheck disable=SC2337'\n",
                "printf '%s\\n' foo# shellcheck disable=SC2337\n",
            ]
            for number, text in enumerate(data):
                source.write_text(text)
                run(0, "literal directive data " + str(number), literal_data=True)
            for text in [
                "# shellcheck disable=SC2337\nprintf x | grep -q x\n",
                "# shellcheck disable=SC2330-SC2340\nprintf x | grep -q x\n",
                "# shellcheck disable=all\nprintf x | grep -q x\n",
                "\t#\tshellcheck\tdisable=SC2337\nprintf x | grep -q x\n",
                "# shellcheck disable=SC1107,SC2337\nprintf x | grep -q x\n",
                'echo "$(\necho "$(\n# shellcheck disable=SC2337\nprintf x | grep -q x\n)"\n)"\n',
                "printf x # shellcheck disable=SC2337\n",
                'echo "$(\n# shellcheck disable=SC2337\nprintf x | grep -q x\n)"\n',
                "echo `\n# shellcheck disable=SC2337\nprintf x | grep -q x\n`\n",
                "cat <<EOF\n$(\n# shellcheck disable=SC2337\nprintf x | grep -q x\n)\nEOF\n",
            ]:
                source.write_text(text)
                run(2, "actual annotation remains forbidden")
        source.write_text(safe)
        if case in {"all", "ancestors"}:
            for component in (root, root / "test", sources):
                target = component.with_name(component.name + "-real")
                component.rename(target)
                component.symlink_to(target, target_is_directory=True)
                try:
                    run(2, "symlinked " + component.name)
                finally:
                    component.unlink()
                    target.rename(component)
        run(0, "restored regular directories")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--adapter", type=Path, required=True)
    parser.add_argument("--shellcheck", type=Path, required=True)
    parser.add_argument("--case", choices=("all", "ranges", "ancestors", "comments"), default="all")
    options = parser.parse_args()
    try:
        if not options.adapter.is_file() or not options.shellcheck.is_file():
            raise RuntimeError("adapter/checker unavailable")
        check(options.adapter.absolute(), options.shellcheck.absolute(), options.case)
    except AssertionError as error:
        print(error, file=sys.stderr)
        return 1
    except (OSError, RuntimeError, subprocess.SubprocessError) as error:
        print(error, file=sys.stderr)
        return 2
    print("suppression ranges and inventory boundaries: PASS")
    return 0


if __name__ == "__main__":
    sys.exit(main())
