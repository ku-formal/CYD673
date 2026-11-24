OCAMLC ?= ocamlc
OCAMLFLAGS ?= -w +a-4-42-70 -warn-error +a-4-42-70
BUILD = _build
CORE = $(BUILD)/while_syntax.cmo $(BUILD)/taint_analysis.cmo
SHARED_AST = while_syntax.ml

.PHONY: all run test clean
all: $(BUILD)/examples

$(BUILD):
	mkdir -p $@

$(BUILD)/while_syntax.cmo: $(SHARED_AST) | $(BUILD)
	$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

$(BUILD)/taint_analysis.cmo: taint_analysis.ml $(BUILD)/while_syntax.cmo
	$(OCAMLC) $(OCAMLFLAGS) -I $(BUILD) -c -o $@ $<

$(BUILD)/examples.cmo: examples.ml $(CORE)
	$(OCAMLC) $(OCAMLFLAGS) -I $(BUILD) -c -o $@ $<

$(BUILD)/run.cmo: run.ml $(BUILD)/examples.cmo
	$(OCAMLC) $(OCAMLFLAGS) -I $(BUILD) -c -o $@ $<

$(BUILD)/test.cmo: test.ml $(BUILD)/examples.cmo
	$(OCAMLC) $(OCAMLFLAGS) -I $(BUILD) -c -o $@ $<

$(BUILD)/examples: $(CORE) $(BUILD)/examples.cmo $(BUILD)/run.cmo
	$(OCAMLC) -o $@ $^

$(BUILD)/test: $(CORE) $(BUILD)/examples.cmo $(BUILD)/test.cmo
	$(OCAMLC) -o $@ $^

run: $(BUILD)/examples
	./$(BUILD)/examples

test: $(BUILD)/test
	./$(BUILD)/test

clean:
	$(RM) -r $(BUILD)
