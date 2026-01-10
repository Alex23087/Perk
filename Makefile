PREFIX ?= /usr/local/perk
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
	opam install -y dune ppx_deriving sedlex menhir odoc cmdliner fpath
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
	OCAMLRUNPARAM=b ./_build/default/bin/perkc.exe --verbose $(FILE)
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
	cd tools/vscode-extensions/perk-syntax && \
	npm install && \
	vsce package --allow-missing-repository
	cd tools/vscode-extensions/perk-vscode-lsp && \
	npm install && \
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
.PHONY: test_pass
test_pass: build
	@echo "Testing programs that are expected to pass..."
	@if [ -n "$(FILE)" ]; then \
		if [ ! -e test/pass/"$(FILE)"*.perk ]; then \
			echo "File starting with $(FILE) does not exist." >&2; \
			exit 1; \
		fi ;\
		FILE=$$(ls test/pass/$(FILE)*.perk | head -n 1) ;\
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
		if [ ! -d "test/pass" ]; then \
			echo "Error: test/pass directory does not exist." >&2; \
			exit 1; \
		fi ;\
		if [ ! -n "$$(ls -A test/pass/*.perk 2>/dev/null)" ]; then \
			echo "Error: No .perk files found in test/pass directory." >&2; \
			exit 1; \
		fi ;\
		COUNT=$$(ls -1 test/pass/*.perk | wc -l) ;\
		CURRENT=0 ;\
		IGNORE=() ;\
		for f in test/pass/*.perk ; \
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
# Test target: run tests on the compiler
# If FILE is specified, run a single test file; otherwise, run all tests
.PHONY: test_pass_static
test_pass_static: build
	@echo "Testing programs that are expected to pass..."
	@if [ -n "$(FILE)" ]; then \
		if [ ! -e test/pass_static/"$(FILE)"*.perk ]; then \
			echo "File starting with $(FILE) does not exist." >&2; \
			exit 1; \
		fi ;\
		FILE=$$(ls test/pass_static/$(FILE)*.perk | head -n 1) ;\
		echo "Testing single file: $$FILE"; \
		BASENAME="$${FILE%.*}"; \
		EXPECTED="$${BASENAME}.expected"; \
		CFILE="$${BASENAME}.c"; \
		RES=$$(_build/default/bin/perkc.exe --static "$$FILE" > /dev/null && gcc -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast "$$CFILE" -o "$$(dirname $$FILE)/a.out" && "$$(dirname $$FILE)/a.out"); \
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
		if [ ! -d "test/pass_static" ]; then \
			echo "Error: test/pass_static directory does not exist." >&2; \
			exit 1; \
		fi ;\
		if [ ! -n "$$(ls -A test/pass_static/*.perk 2>/dev/null)" ]; then \
			echo "Error: No .perk files found in test/pass_static directory." >&2; \
			exit 1; \
		fi ;\
		COUNT=$$(ls -1 test/pass_static/*.perk | wc -l) ;\
		CURRENT=0 ;\
		IGNORE=() ;\
		for f in test/pass_static/*.perk ; \
		do \
			CURRENT=$$((CURRENT+1)) ;\
			if printf '%s\n' "$${IGNORE[@]}" | grep -q "^$$CURRENT$$"; then \
				# echo "[$$CURRENT/$$COUNT] Ignoring $$(basename "$${f%.*}")" ;\
				continue ;\
			fi ;\
			echo "[$$CURRENT/$$COUNT] Testing $$(basename "$${f%.*}")" ; \
			EXPECTED="$${f%.*}.expected" ;\
			RES=$$(_build/default/bin/perkc.exe --static "$$f" > /dev/null && gcc -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast "$${f%.*}.c" -o "$$(dirname $$f)/a.out" && "$$(dirname $$f)/a.out") ; \
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

# Test programs that are expected to fail
.PHONY: test_fail
test_fail: build
	@echo "Testing programs that are expected to fail..."
	@if [ -n "$(FILE)" ]; then \
		if [ ! -e test/fail/"$(FILE)"*.perk ]; then \
			echo "File starting with $(FILE) does not exist." >&2; \
			exit 1; \
		fi ;\
		FILE=$$(ls test/fail/$(FILE)*.perk | head -n 1) ;\
		echo "Testing single file: $$FILE"; \
		BASENAME="$${FILE%.*}"; \
		CFILE="$${BASENAME}.c"; \
		RES=$$(_build/default/bin/perkc.exe "$$FILE" 2>&1); \
		EXIT_CODE=$$?; \
		rm -f "$$(dirname $$FILE)/a.out" "$$CFILE"; \
		if [ $$EXIT_CODE -ne 0 ]; then \
			# echo "$$RES" ;\
			:;\
		else \
			echo "Expected failure, but the test passed." >&2;\
			echo "File: $$FILE" >&2;\
			echo "Output: $$RES" >&2;\
			exit 1;\
		fi ;\
	else \
		if [ ! -d "test/fail" ]; then \
			echo "Error: test/fail directory does not exist." >&2; \
			exit 1; \
		fi ;\
		if [ ! -n "$$(ls -A test/fail/*.perk 2>/dev/null)" ]; then \
			echo "Error: No .perk files found in test/fail directory." >&2; \
			exit 1; \
		fi ;\
		COUNT=$$(ls -1 test/fail/*.perk | wc -l) ;\
		CURRENT=0 ;\
		IGNORE=() ;\
		for f in test/fail/*.perk ; \
		do \
			CURRENT=$$((CURRENT+1)) ;\
			if printf '%s\n' "$${IGNORE[@]}" | grep -q "^$$CURRENT$$"; then \
				# echo "[$$CURRENT/$$COUNT] Ignoring $$(basename "$${f%.*}")" ;\
				continue ;\
			fi ;\
			echo "[$$CURRENT/$$COUNT] Testing $$(basename "$${f%.*}")" ; \
			RES=$$(_build/default/bin/perkc.exe "$$f" 2>&1) ; \
			EXIT_CODE=$$?; \
			rm -f "$$(dirname $$f)/a.out" "$${f%.*}.c" ;\
			if [ $$EXIT_CODE -ne 0 ]; then \
				# echo "$$RES" ;\
				:;\
			else \
				echo "Expected failure, but the test passed." >&2;\
				echo "File: $$(basename $${f%.*})" >&2;\
				echo "Output: $$RES" >&2;\
			fi ;\
		done ;\
	fi

# Test static programs that are expected to fail
.PHONY: test_fail_static
test_fail_static: build
	@echo "Testing static programs that are expected to fail..."
	@if [ -n "$(FILE)" ]; then \
		if [ ! -e test/fail_static/"$(FILE)"*.perk ]; then \
			echo "File starting with $(FILE) does not exist." >&2; \
			exit 1; \
		fi ;\
		FILE=$$(ls test/fail_static/$(FILE)*.perk | head -n 1) ;\
		echo "Testing single file: $$FILE"; \
		BASENAME="$${FILE%.*}"; \
		CFILE="$${BASENAME}.c"; \
		RES=$$(_build/default/bin/perkc.exe --static "$$FILE" 2>&1); \
		EXIT_CODE=$$?; \
		rm -f "$$(dirname $$FILE)/a.out" "$$CFILE"; \
		if [ $$EXIT_CODE -ne 0 ]; then \
			# echo "$$RES" ;\
			:;\
		else \
			echo "Expected failure, but the test passed." >&2;\
			echo "File: $$FILE" >&2;\
			echo "Output: $$RES" >&2;\
			exit 1;\
		fi ;\
	else \
		if [ ! -d "test/fail_static" ]; then \
			echo "Error: test/fail_static directory does not exist." >&2; \
			exit 1; \
		fi ;\
		if [ ! -n "$$(ls -A test/fail_static/*.perk 2>/dev/null)" ]; then \
			echo "Error: No .perk files found in test/fail_static directory." >&2; \
			exit 1; \
		fi ;\
		COUNT=$$(ls -1 test/fail_static/*.perk | wc -l) ;\
		CURRENT=0 ;\
		IGNORE=() ;\
		for f in test/fail_static/*.perk ; \
		do \
			CURRENT=$$((CURRENT+1)) ;\
			if printf '%s\n' "$${IGNORE[@]}" | grep -q "^$$CURRENT$$"; then \
				# echo "[$$CURRENT/$$COUNT] Ignoring $$(basename "$${f%.*}")" ;\
				continue ;\
			fi ;\
			echo "[$$CURRENT/$$COUNT] Testing $$(basename "$${f%.*}")" ; \
			RES=$$(_build/default/bin/perkc.exe --static "$$f" 2>&1) ; \
			EXIT_CODE=$$?; \
			rm -f "$$(dirname $$f)/a.out" "$${f%.*}.c" ;\
			if [ $$EXIT_CODE -ne 0 ]; then \
				# echo "$$RES" ;\
				:;\
			else \
				echo "Expected failure, but the test passed." >&2;\
				echo "File: $$(basename $${f%.*})" >&2;\
				echo "Output: $$RES" >&2;\
			fi ;\
		done ;\
	fi

# Run all tests (both pass and fail)
.PHONY: test
test:
	@echo "Running all tests..."
	@$(MAKE) test_pass
	@$(MAKE) test_fail
	@$(MAKE) test_pass_static
	@$(MAKE) test_fail_static

# Generate documentation
.PHONY: docs
docs:
	opam exec -- dune build @doc
	mkdir -p docs
	cp -r _build/default/_doc/_html/* docs/
