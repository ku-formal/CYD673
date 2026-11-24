open While_syntax
open Taint_analysis

let expect : string -> bool -> unit
= fun name condition ->
  if condition then Printf.printf "PASS %s\n" name
  else failwith name

let equal_state : AbsMem.t -> AbsMem.t -> bool
= fun s t ->
  AbsMem.porder s t && AbsMem.porder t s

let test_states : unit -> unit
= fun () ->
  let left = AbsMem.add "x" High AbsMem.init in
  let right = initial_state Skip ["x"; "y"] ["z"] in
  let both = AbsMem.join left right in
  expect "a missing variable and history are low in init"
    (AbsMem.find "z" AbsMem.init = Low &&
     AbsMem.Map.mem "history" AbsMem.init && AbsMem.find "history" AbsMem.init = Low);
  expect "pointwise join on one-sided bindings and history"
    (AbsMem.find "x" both = High &&
     AbsMem.find "y" both = Low &&
     AbsMem.find "z" both = High && AbsMem.proper both);
  expect "pointwise order on one-sided bindings"
    (AbsMem.porder AbsMem.init left && AbsMem.porder left both &&
     AbsMem.porder right both && not (AbsMem.porder left right) &&
     not (AbsMem.porder right left));
  let high_history = AbsMem.add "history" High left in
  expect "pointwise order on history"
    (AbsMem.porder left high_history && not (AbsMem.porder high_history left))

let test_expressions : unit -> unit
= fun () ->
  let arithmetic = Mul (Add (Var "x", Num 1), Sub (Num 0, Var "y")) in
  let s = initial_state (Assign ("result", arithmetic)) ["x"] [] in
  expect "initial policy covers assignment targets and expression operands"
    (AbsMem.find "result" s = High &&
     AbsMem.find "x" s = Low && AbsMem.find "y" s = High);
  expect "arithmetic joins operand dependencies" (eval_a arithmetic s = High);
  expect "conjunction tracks both operands"
    (eval_b (And (False, Le (Var "y", Var "x"))) s = High);
  let s = AbsMem.add "history" High s in
  expect "high history affects constants and otherwise-low variables"
    (eval_a (Num 0) s = High && eval_a (Var "x") s = High &&
     eval_b True s = High && eval_b (Not False) s = High)

let test_control : unit -> unit
= fun () ->
  let branches = If (True, Assign ("x", Num 0), Assign ("x", Var "secret")) in
  let s = initial_state branches ["x"] [] in
  let out = eval_c branches s in
  expect "low guard joins both branches, even for literal true"
    (AbsMem.proper out && AbsMem.find "x" out = High);
  let guard = Eq (Var "secret", Num 1) in
  let assign n = Assign ("x", Num n) in
  let lost_after_branch n =
    let c = If (guard, assign 0, assign n) in
    let s = initial_state c [] ["x"] in
    equal_state (eval_c c s) (AbsMem.lost s)
  in
  expect "high guard with different branches returns lost" (lost_after_branch 1);
  expect "high guard with identical branches returns lost" (lost_after_branch 0);
  let high_loop = While (Eq (Var "secret", Num 1), Skip) in
  expect "a private loop guard prevents YES! even without requested outputs"
    (check high_loop [] [] = Unknown);
  let s = initial_state high_loop ["out"; "input_only"] ["out"] in
  let lost = eval_c high_loop s in
  expect "lost includes public inputs and outputs absent from the program"
    (AbsMem.find "secret" s = High &&
     AbsMem.find "out" s = Low &&
     AbsMem.find "out" lost = High &&
     AbsMem.find "input_only" lost = High &&
     AbsMem.find "history" lost = High);
  expect "lost persists after a constant assignment"
    (equal_state (eval_c (Assign ("out", Num 0)) lost) lost)

let test_propagation : unit -> unit
= fun () ->
  let states = Examples.propagation_trace () in
  let row : AbsMem.t -> label list
  = fun s ->
    [AbsMem.find "i" s; AbsMem.find "x" s;
     AbsMem.find "y" s; AbsMem.find "secret" s;
     AbsMem.find "history" s]
  in
  let expected = [
    [Low; Low;  Low;  Low;  Low];
    [Low; Low;  Low;  High; Low];
    [Low; Low;  High; High; Low];
    [Low; High; High; High; Low];
    [Low; High; High; High; Low]
  ] in
  expect "every entry of the Lecture 5 p.27 iteration table"
    (List.map row states = expected);
  expect "whole While expression returns the displayed fixed point"
    (equal_state (List.hd (List.rev states)) Examples.propagation_result);
  expect "verdict depends on the requested outputs"
    (check Examples.propagation Examples.public_inputs ["i"] = Yes &&
     Examples.propagation_verdict = Unknown)

let test_overwrite : unit -> unit
= fun () ->
  let overwrite =
    Seq (Assign ("y", Num 1),
         While (Not (Eq (Var "x", Num 1)),
           Seq (Assign ("y", Mul (Var "y", Var "x")),
                Assign ("x", Sub (Var "x", Num 1)))))
  in
  let entry = initial_state overwrite ["x"] ["y"] in
  let result = eval_c overwrite entry in
  expect "overwriting private data (Lecture 5 p.29) certifies public y"
    (AbsMem.find "y" entry = High &&
     AbsMem.find "y" result = Low &&
     AbsMem.proper result && check overwrite ["x"] ["y"] = Yes);
  expect "unlisted outputs start private"
    (check Skip [] ["unlisted"] = Unknown);
  let outer = While (Eq (Var "x", Num 0),
                     While (Eq (Var "secret", Num 0), Skip)) in
  let entry = initial_state outer ["x"] [] in
  expect "nested private loop affects outer fixed point"
    (equal_state (eval_c outer entry) (AbsMem.lost entry))

let () =
  test_states ();
  test_expressions ();
  test_control ();
  test_propagation ();
  test_overwrite ()
