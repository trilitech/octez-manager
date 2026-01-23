#!/bin/bash
# Generate coverage report from bisect_ppx data
set -euo pipefail

COVERAGE_DIR="${1:-./_coverage}"
OUTPUT_DIR="${2:-./coverage-report}"

echo "Generating coverage report..."
echo "Coverage data dir: $COVERAGE_DIR"
echo "Output dir: $OUTPUT_DIR"

# Check if bisect-ppx-report is available
if ! command -v bisect-ppx-report &>/dev/null; then
	echo "ERROR: bisect-ppx-report not found"
	echo "Install with: opam install bisect_ppx"
	exit 1
fi

# Check if coverage files exist
if [ ! -d "$COVERAGE_DIR" ] || [ -z "$(ls -A $COVERAGE_DIR/*.coverage 2>/dev/null)" ]; then
	echo "ERROR: No coverage files found in $COVERAGE_DIR"
	echo "Run tests with: BISECT_FILE=$COVERAGE_DIR/bisect dune runtest --instrument-with bisect_ppx --force"
	exit 1
fi

# Count coverage files
COVERAGE_FILES=$(find "$COVERAGE_DIR" -name "*.coverage" | wc -l)
echo "Found $COVERAGE_FILES coverage file(s)"

# Generate HTML report
echo "Generating HTML report..."
mkdir -p "$OUTPUT_DIR"
bisect-ppx-report html \
	--coverage-path "$COVERAGE_DIR" \
	--title "Octez Manager Coverage Report" \
	-o "$OUTPUT_DIR"

echo "HTML report generated at: $OUTPUT_DIR/index.html"

# Generate summary report
echo ""
echo "Generating summary..."
bisect-ppx-report summary \
	--coverage-path "$COVERAGE_DIR"

# Generate detailed text summary for CI
echo ""
echo "Coverage by file:"
bisect-ppx-report summary \
	--coverage-path "$COVERAGE_DIR" \
	--per-file

# Generate Cobertura XML for tools like codecov
echo ""
echo "Generating Cobertura XML..."
bisect-ppx-report cobertura \
	--coverage-path "$COVERAGE_DIR" \
	"$OUTPUT_DIR/coverage.xml"

echo ""
echo "✓ Coverage report generation complete!"
