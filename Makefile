.PHONY: all deps deps-ci build test clean coverage update-miaou arch-index arch-query

OPAM_EXEC ?= opam exec --
DUNE = $(OPAM_EXEC) dune
BISECT = $(OPAM_EXEC) bisect-ppx-report

all: build

update-miaou:
	opam reinstall miaou-core miaou-driver-term miaou-runner miaou-tui miaou-driver-matrix

deps:
	opam update -y
	opam install --deps-only --with-test --reuse-build -y .

MIAOU_GIT_URL ?= https://github.com/trilitech/miaou.git
PPX_FORBID_GIT_URL ?= https://github.com/atacama-dev/ppx_forbid.git

deps-ci:
	@echo "Pinning ppx_forbid from $(PPX_FORBID_GIT_URL)"
	@opam pin add -y ppx_forbid "$(PPX_FORBID_GIT_URL)" || { echo "ERROR: Failed to pin ppx_forbid package" >&2; exit 1; }
	@echo "Pinning miaou from $(MIAOU_GIT_URL)"
	@opam pin add -y miaou "$(MIAOU_GIT_URL)" || { echo "ERROR: Failed to pin miaou package" >&2; exit 1; }
	@opam pin add -y miaou-driver-matrix "$(MIAOU_GIT_URL)" || { echo "ERROR: Failed to pin miaou-driver-matrix package" >&2; exit 1; }
	$(MAKE) deps

build:
	$(DUNE) build
	cp -f _build/install/default/bin/octez-manager ./

fmt:
	@$(DUNE) build @fmt || true

fmt-check:
	@$(DUNE) build @fmt >/dev/null
	@git --no-pager diff --exit-code || (echo "Formatting changes required. Run 'make fmt' and commit." && false)

completions:
	$(DUNE) exec -- octez-manager-gen-completion

completions-check:
	@mkdir -p /tmp/octez-completions-check
	@$(DUNE) exec -- octez-manager-gen-completion --out-dir /tmp/octez-completions-check
	@if ! diff -q completions/octez-manager.bash /tmp/octez-completions-check/octez-manager.bash >/dev/null 2>&1 || \
	    ! diff -q completions/octez-manager.zsh /tmp/octez-completions-check/octez-manager.zsh >/dev/null 2>&1; then \
		echo "ERROR: Shell completion files are out of date."; \
		echo "Run: make completions"; \
		echo ""; \
		echo "=== Bash diff ==="; \
		diff -u completions/octez-manager.bash /tmp/octez-completions-check/octez-manager.bash || true; \
		echo ""; \
		echo "=== Zsh diff ==="; \
		diff -u completions/octez-manager.zsh /tmp/octez-completions-check/octez-manager.zsh || true; \
		rm -rf /tmp/octez-completions-check; \
		exit 1; \
	fi
	@rm -rf /tmp/octez-completions-check
	@echo "Completion files are up to date."

lint-sync-io:
	@./scripts/check-sync-io.sh

lint-indexer:
	@./scripts/check-direct-tzkt.sh

test: fmt-check completions-check lint-sync-io lint-indexer
	$(DUNE) runtest

arch-index:
	$(DUNE) exec -- tools/arch_index.exe

arch-query:
	$(DUNE) exec tools/arch_query.exe -- $(ARGS)

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
