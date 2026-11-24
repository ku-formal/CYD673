# Lecture 9 OCaml implementation

Use an OCaml compiler (4.08 or newer) and Make.

```sh
make
./_build/examples
make test
```

`taint_analysis.ml` implements the security classifications and semantics from Lecture 5.
`examples.ml` defines the loop from Lecture 5, p. 27. The executable prints its fixed-point iterations and the verdict for the requested public output `x`. The tests check the trace, verdicts, and abstract operations.

Programs use the constructors of [the shared While AST](while_syntax.ml). The example calls `check propagation public_inputs public_outputs` with public inputs `["i"; "x"; "y"]` and requested public outputs `["x"]`.

`Yes` prints as `YES!`. It certifies termination-sensitive noninterference for the specified inputs and outputs. `Unknown` prints as `NO?` when the analysis cannot certify the program.

`AbsMem.t` maps variable names to security labels. The string `"history"` is reserved for control history. `AbsMem.init` stores `"history" -> Low`, and a lookup of any missing name returns `Low`.

`initial_state c public_inputs public_outputs` collects every variable named in the program and its input/output lists. It labels public inputs and `"history"` low and explicitly labels every other collected variable high. A loop starts its fixed-point iteration with all these keys low. A high guard makes all of them high, including `"history"`.
