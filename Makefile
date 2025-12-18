.PHONY: deps deps-ci build test clean coverage

OPAM_EXEC ?= opam exec --
DUNE = $(OPAM_EXEC) dune
BISECT = $(OPAM_EXEC) bisect-ppx-report

deps:
	opam update -y
	opam install --deps-only --with-test --reuse-build -y .

deps-ci:
	@if [ -n "$$MIAOU_GIT_URL" ]; then \
		echo "Pinning miaou from $$MIAOU_GIT_URL"; \
		opam pin add -y miaou "$$MIAOU_GIT_URL" || { echo "ERROR: Failed to pin miaou package" >&2; exit 1; }; \
	fi
	$(MAKE) deps

build:
	$(DUNE) build
	cp -f _build/install/default/bin/octez-manager ./

fmt:
	@$(DUNE) build @fmt || true

fmt-check:
	@$(DUNE) build @fmt >/dev/null
	@git --no-pager diff --exit-code || (echo "Formatting changes required. Run 'make fmt' and commit." && false)

test: fmt-check
	$(DUNE) runtest

clean:
	$(DUNE) clean

coverage: clean
	@echo "[coverage] running instrumented test suite"
	@rm -rf _coverage && mkdir -p _coverage
	@BISECT_FILE=$(CURDIR)/_coverage/bisect $(DUNE) runtest --instrument-with bisect_ppx
	@COVERAGE_FILES=$$(find _coverage -name '*.coverage' -print); \
		$(BISECT) summary --per-file $$COVERAGE_FILES | tee _coverage/summary.txt; \
		$(BISECT) html -o _coverage/html $$COVERAGE_FILES
	@echo "[coverage] summary saved to _coverage/summary.txt"
	@echo "[coverage] html report available under _coverage/html/index.html"
