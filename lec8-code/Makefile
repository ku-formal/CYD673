OCAMLC ?= ocamlc
FLAGS = -I _build
OBJECTS = _build/while_syntax.cmo _build/sign_analysis.cmo

.PHONY: all run test clean
all: _build/examples

_build:
	mkdir -p $@

_build/while_syntax.cmo: while_syntax.ml | _build
	$(OCAMLC) $(FLAGS) -c -o $@ $<

_build/sign_analysis.cmo: sign_analysis.ml _build/while_syntax.cmo
	$(OCAMLC) $(FLAGS) -c -o $@ $<

_build/examples.cmo: examples.ml $(OBJECTS)
	$(OCAMLC) $(FLAGS) -c -o $@ $<

_build/%.cmo: %.ml _build/examples.cmo
	$(OCAMLC) $(FLAGS) -c -o $@ $<

_build/examples: $(OBJECTS) _build/examples.cmo _build/run.cmo
	$(OCAMLC) $(FLAGS) -o $@ $^

_build/tests: $(OBJECTS) _build/examples.cmo _build/test.cmo
	$(OCAMLC) $(FLAGS) -o $@ $^

run: _build/examples
	./_build/examples

test: _build/tests
	./_build/tests

clean:
	$(RM) -r _build
