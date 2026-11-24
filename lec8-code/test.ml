open While_syntax
open Sign_analysis

let check : string -> bool -> unit
= fun name condition ->
  if not condition then failwith name

let signs = Sign.[Bot; Neg; Zero; Pos; NonPos; NonZero; NonNeg; Top]

let contains : Sign.t -> int -> bool
= fun s n ->
  match s with
  | Sign.Bot -> false   | Sign.Neg -> n < 0       | Sign.Zero -> n = 0    | Sign.Pos -> n > 0
  | Sign.NonPos -> n <= 0 | Sign.NonZero -> n <> 0 | Sign.NonNeg -> n >= 0 | Sign.Top -> true

(* Each nonzero sign gets two magnitudes so that like signs can compare
   equal or unequal and opposite signs can add to any sign. *)
let witnesses = [-2; -1; 0; 1; 2]
let values : Sign.t -> int list
= fun s ->
  List.filter (contains s) witnesses

let expected_sign : int list -> Sign.t
= fun ns ->
  List.find (fun s ->
    List.for_all (fun n ->
      contains s n = List.exists (fun k -> compare k 0 = compare n 0) ns
    ) [-1; 0; 1]
  ) signs

let expected_truth : bool list -> AbsBool.t
= fun ts ->
  match List.mem false ts, List.mem true ts with
  | false, false -> AbsBool.Bot | false, true -> AbsBool.True
  | true, false -> AbsBool.False | true, true -> AbsBool.Top

let pairs : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
= fun f xs ys ->
  List.concat_map (fun x -> List.map (f x) ys) xs

let check_operators : unit -> unit
= fun () ->
  List.iter (fun a -> List.iter (fun b ->
    let xs, ys = values a, values b in
    let verify : string -> bool -> unit
    = fun op test ->
      check (String.concat " " [Sign.to_string a; op; Sign.to_string b]) test in
    verify "<=" (Sign.porder a b = List.for_all (contains b) xs);
    verify "join" (Sign.join a b = expected_sign (xs @ ys));
    verify "+" (Sign.add a b = expected_sign (pairs ( + ) xs ys));
    verify "*" (Sign.mul a b = expected_sign (pairs ( * ) xs ys));
    verify "-" (Sign.sub a b = expected_sign (pairs ( - ) xs ys));
    verify "=" (AbsBool.eq a b = expected_truth (pairs ( = ) xs ys));
    verify "<= (comparison)" (AbsBool.leq a b = expected_truth (pairs ( <= ) xs ys))
  ) signs) signs;
  let booleans = [AbsBool.Bot, []; AbsBool.True, [true]; AbsBool.False, [false]; AbsBool.Top, [true; false]] in
  List.iter (fun (a, xs) ->
    check "negation" (AbsBool.not a = expected_truth (List.map not xs));
    List.iter (fun (b, ys) ->
      check "conjunction" (AbsBool.band a b = expected_truth (pairs ( && ) xs ys))
    ) booleans
  ) booleans

let check_statements : unit -> unit
= fun () ->
  let s = AbsMem.add "x" Sign.Pos AbsMem.empty in
  let branch : bexp -> stmt
  = fun b ->
    If (b, Assign ("r", Num 1), Assign ("r", Num (-1))) in
  check "true branch" (AbsMem.find "r" (eval_c (branch True) s) = Sign.Pos);
  check "false branch" (AbsMem.find "r" (eval_c (branch False) s) = Sign.Neg);
  let unavailable = Eq (Var "unset", Num 0) in
  check "bottom guard" (AbsMem.porder (eval_c (branch unavailable) s) AbsMem.empty);
  let unknown = Eq (Var "x", Var "x") in
  check "unrelated operand values" (AbsMem.find "r" (eval_c (branch unknown) s) = Sign.NonZero);
  let dead = While (False, Assign ("x", Num (-1))) in
  check "zero iterations" (AbsMem.find "x" (eval_c dead s) = Sign.Pos);
  let nested = While (True, Seq (Assign ("x", Num 0), dead)) in
  check "nested loop invariant" (AbsMem.find "x" (eval_c nested s) = Sign.NonNeg)

let check_lecture_trace : unit -> unit
= fun () ->
  let vars = ["x"; "y"; "n"; "z"] in
  let row : AbsMem.t -> Sign.t list
  = fun s ->
    List.map (fun x -> AbsMem.find x s) vars in
  let bot = Sign.[Bot; Bot; Bot; Bot]
  and entry = Sign.[Zero; Zero; Top; Top]
  and stable = Sign.[NonNeg; NonNeg; Top; Top] in
  let rows = List.map (fun (old, x) -> row old, row x) (Examples.loop_trace ()) in
  check "Lecture 4, p. 26: old and new states"
    (rows = [bot, entry; entry, stable; stable, stable]);
  check "Lecture 4, p. 26: program output" (row Examples.result = stable);
  check "Lecture 4, p. 26: assertion" (Examples.verdict = AbsBool.True)

let () =
  check_operators ();
  check_statements ();
  check_lecture_trace ();
  print_endline "Passed: sign and Boolean tables, branches, loops, lecture trace."
