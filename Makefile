PREFIX ?= /usr/local/perkelang
SHELL := /usr/bin/env bash

# The following lines allow targets to take arguments from the command line
FILE := $(word 2,$(MAKECMDGOALS))
%:
	@:

# Default target: build the compiler
.PHONY: build
build:
	opam exec dune build

# Clean target: remove build artifacts
.PHONY: clean
clean:
	rm -rf build
	opam exec dune clean
	rm -f test/test.out test/test.c
	rm -fr docs

# Install dependencies
.PHONY: deps
deps:
	opam install ppx_deriving
	opam install sedlex
	opam install menhir
	@if ! command -v gcc >/dev/null 2>&1; then \
		echo "Warning: gcc is not installed or not in PATH." >&2; \
	fi
	@if ! command -v ctags >/dev/null 2>&1; then \
		echo "Warning: ctags is not installed or not in PATH." >&2; \
	fi

# Run the compiler with the specified file
.PHONY: run
run: build
	@if [ -z "$(FILE)" ]; then \
		echo "Error: No input file specified. Usage: make run <filename>"; \
		exit 1; \
	fi
	./_build/default/bin/perkc.exe $(FILE)
	$(eval OUTFILE := $(basename $(FILE)).out)
	$(eval SRCFILE := $(basename $(FILE)).c)
	gcc -o $(OUTFILE) $(SRCFILE)
	./$(OUTFILE)
	@rm -f $(OUTFILE) $(SRCFILE)

# Run with perf profiling of the compilation only
.PHONY: run_perf
run_perf: build
	@if [ -z "$(FILE)" ]; then \
		echo "Error: No input file specified. Usage: make run_perf <filename>"; \
		exit 1; \
	fi
	OCAMLRUNPARAM=b perf record -F 1000 --call-graph dwarf ./_build/default/bin/perkc.exe $(FILE)
	$(eval SRCFILE := $(basename $(FILE)).c)
	$(eval PERFFILE := $(basename $(FILE)).perf)
	perf script -F +pid > $(PERFFILE)
	rm -f $(SRCFILE)

# Run with debug information
.PHONY: debug_run
debug_run: build
	@if [ -z "$(FILE)" ]; then \
		echo "Error: No input file specified. Usage: make debug_run <filename>"; \
		exit 1; \
	fi
	opam exec -- dune build --profile=dev
	OCAMLRUNPARAM=b ./_build/default/bin/perkc.exe $(FILE)
	$(eval OUTFILE := $(basename $(FILE)).out)
	$(eval SRCFILE := $(basename $(FILE)).c)
	gcc -o $(OUTFILE) $(SRCFILE)
	./$(OUTFILE)
	rm -f $(OUTFILE) $(SRCFILE)

# Install extensions for VS Code
# Requires vsce, npx and tsc to be installed
.PHONY: extensions
extensions:
	@if ! command -v vsce >/dev/null 2>&1; then \
		echo "Error: vsce is not installed" >&2; \
		exit 1; \
	fi
	@if ! command -v npx >/dev/null 2>&1; then \
		echo "Error: npx is not installed" >&2; \
		exit 1; \
	fi
	@if ! command -v tsc >/dev/null 2>&1; then \
		echo "Error: tsc is not installed" >&2; \
		exit 1; \
	fi
	cd perkelang-extension && \
	vsce package --allow-missing-repository
	cd perkelang-vscode-lsp && \
	npx tsc && \
	vsce package --allow-missing-repository

# Install the Perk compiler
.PHONY: install
install: build uninstall
	cd _build/default/ && \
	sudo mkdir -p $(PREFIX)/ && \
	sudo cp -r . $(PREFIX)/ && \
	sudo ln -s $(PREFIX)/bin/perkc.exe /usr/local/bin/perkc

# Uninstall the Perk compiler
.PHONY: uninstall
uninstall:
	sudo rm -rf $(PREFIX)
	sudo rm -f /usr/local/bin/perkc

# Test target: run tests on the compiler
# If FILE is specified, run a single test file; otherwise, run all tests
.PHONY: test
test: build
	@if [ -n "$(FILE)" ]; then \
		if [ ! -e test/normalexec/"$(FILE)"*.perk ]; then \
			echo "File starting with $(FILE) does not exist." >&2; \
			exit 1; \
		fi ;\
		FILE=$$(ls test/normalexec/$(FILE)*.perk | head -n 1) ;\
		echo "Testing single file: $$FILE"; \
		BASENAME="$${FILE%.*}"; \
		EXPECTED="$${BASENAME}.expected"; \
		CFILE="$${BASENAME}.c"; \
		RES=$$(_build/default/bin/perkc.exe "$$FILE" > /dev/null && gcc -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast "$$CFILE" -o "$$(dirname $$FILE)/a.out" && "$$(dirname $$FILE)/a.out"); \
		rm -f "$$(dirname $$FILE)/a.out"; \
		if [ $$? -eq 0 ]; then \
			if [ -e "$$EXPECTED" ]; then \
				echo "$$RES" | diff "$$EXPECTED" -; \
				if [ $$? -eq 0 ]; then \
					:; \
				else \
					echo "Test Failed"; \
				fi ;\
			else \
				echo "$$RES" ;\
			fi;\
		else \
			echo "An error occurred while compiling $(basename $$FILE)" >&2;\
			echo "$$RES" >&2;\
		fi ;\
	else \
		COUNT=$$(ls -1 test/normalexec/*.perk | wc -l) ;\
		CURRENT=0 ;\
		IGNORE=(18) ;\
		for f in test/normalexec/*.perk ; \
		do \
			CURRENT=$$((CURRENT+1)) ;\
			if printf '%s\n' "$${IGNORE[@]}" | grep -q "^$$CURRENT$$"; then \
				# echo "[$$CURRENT/$$COUNT] Ignoring $$(basename "$${f%.*}")" ;\
				continue ;\
			fi ;\
			echo "[$$CURRENT/$$COUNT] Testing $$(basename "$${f%.*}")" ; \
			EXPECTED="$${f%.*}.expected" ;\
			RES=$$(_build/default/bin/perkc.exe "$$f" > /dev/null && gcc -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast "$${f%.*}.c" -o "$$(dirname $$f)/a.out" && "$$(dirname $$f)/a.out") ; \
			rm -f "$$(dirname $$f)/a.out" ;\
			if [ $$? -eq 0 ]; then \
				# echo "$$RES" ;\
				if [ -e "$$EXPECTED" ]; then \
					echo "$$RES" | diff "$$EXPECTED" -;\
					if [ $$? -eq 0 ]; then \
						# rm -f "$${f%.*}.c" ;\
						:\
					else \
						echo "Test Failed";\
					fi ;\
				else \
					:;\
					# rm -f "$${f%.*}.c" ;\
					echo "$$RES" ;\
				fi;\
			else \
				echo "An error occurred while compiling $$(basename $${f%.*})" >&2;\
				echo "$$RES" >&2;\
			fi ;\
		done ;\
	fi

# Generate documentation
.PHONY: docs
docs:
	opam exec -- dune build @doc
	mkdir -p docs
	cp -r _build/default/_doc/_html/* docs/