# Sign analysis

From this directory, compile and run the Lecture 4 loop example:

```sh
make
./_build/examples
```

The example prints the loop trace and assertion result. Run `make test`
to check the iterations and abstract operators.

- `Sign.t` represents an integer sign. `Sign.porder`, `Sign.join`, `Sign.alpha`,
  `Sign.add`, `Sign.mul`, and `Sign.sub` implement the integer domain operations.
- `AbsMem.t` maps program variables to signs. `AbsMem.find x m` returns
  `Sign.Bot` for an unbound variable. `AbsMem.add x v m` gives the updated memory.
- `AbsBool.t` represents an abstract Boolean. `AbsBool.eq` and `AbsBool.leq`
  compare two signs. `AbsBool.not` and `AbsBool.band` operate on abstract Booleans.

Start with `AbsMem.add "n" Sign.Top AbsMem.empty` for an arbitrary input
integer named `n`. Analyze the program with
`eval_c : While_syntax.stmt -> AbsMem.t -> AbsMem.t`. Evaluate a Boolean
postcondition with `eval_b` on the final memory. A result of `AbsBool.True`
proves the postcondition.

- `while_syntax.ml` defines the shared While AST.
- `sign_analysis.ml` implements the domains and statement rules.
- `examples.ml` supplies the program, inputs and loop trace from Lecture 4,
  pp. 25–26. `run.ml` prints its trace and assertion result.
- `test.ml` checks the operators, branches, loops and the lecture trace.
