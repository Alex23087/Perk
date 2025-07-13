PREFIX ?= /usr/local/perkelang
SHELL := /usr/bin/env bash

FILE := $(word 2,$(MAKECMDGOALS))
%:
	@:

.PHONY: build
build:
	opam exec dune build

.PHONY: clean
clean:
	rm -rf build
	opam exec dune clean
	rm -f test/test.out test/test.c
	rm -fr docs

.PHONY: deps
deps:
	opam install ppx_deriving
	opam install sedlex
	opam install menhir

run: build
	./_build/default/bin/perkc.exe $(FILE)
	$(eval OUTFILE := $(basename $(FILE)).out)
	$(eval SRCFILE := $(basename $(FILE)).c)
	gcc -o $(OUTFILE) $(SRCFILE)
	./$(OUTFILE)
	@rm -f $(OUTFILE) $(SRCFILE)

# Run with perf profiling of the compilation only
.PHONY: run_perf
run_perf: build
	OCAMLRUNPARAM=b perf record -F 1000 --call-graph dwarf ./_build/default/bin/perkc.exe $(FILE)
	$(eval SRCFILE := $(basename $(FILE)).c)
	$(eval PERFFILE := $(basename $(FILE)).perf)
	perf script -F +pid > $(PERFFILE)
	rm -f $(SRCFILE)

# Run with debug information
.PHONY: debug_run
debug_run: build
	opam exec -- dune build --profile=dev
	OCAMLRUNPARAM=b ./_build/default/bin/perkc.exe $(FILE)
	$(eval OUTFILE := $(basename $(FILE)).out)
	$(eval SRCFILE := $(basename $(FILE)).c)
	gcc -o $(OUTFILE) $(SRCFILE)
	./$(OUTFILE)
	rm -f $(OUTFILE) $(SRCFILE)

.PHONY: extensions
extensions:
	cd perkelang-extension && \
	vsce package --allow-missing-repository
	cd perkelang-vscode-lsp && \
	npx tsc && \
	vsce package --allow-missing-repository

.PHONY: install
install: build uninstall
	cd _build/default/ && \
	sudo mkdir -p $(PREFIX)/ && \
	sudo cp -r . $(PREFIX)/ && \
	sudo ln -s $(PREFIX)/bin/perkc.exe /usr/local/bin/perkc

.PHONY: uninstall
uninstall:
	sudo rm -rf $(PREFIX)
	sudo rm -f /usr/local/bin/perkc

.PHONY: test
test: build
	@COUNT=$$(ls -1 test/normalexec/*.perk | wc -l) ;\
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
	done

.PHONY: docs
docs:
	opam exec -- dune build @doc
	mkdir -p docs
	cp -r _build/default/_doc/_html/* docs/