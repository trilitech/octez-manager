.PHONY: all deps build test clean coverage update-miaou arch-index arch-query

OPAM_EXEC ?= opam exec --
DUNE = $(OPAM_EXEC) dune
BISECT = $(OPAM_EXEC) bisect-ppx-report

all: build

update-miaou:
	opam update
	opam upgrade miaou-core miaou-driver-term miaou-driver-matrix miaou-driver-web miaou-runner miaou-registry --yes

deps:
	@opam switch list -s | grep -q "^$(CURDIR)$$" || opam switch create . 5.3.0 --no-install --yes
	@opam install . --deps-only --with-test --yes
	@opam install sqlite3 dune-build-info ocamlformat --yes

build:
	$(DUNE) build src/
	cp -f _build/default/src/main.exe ./octez-manager

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
