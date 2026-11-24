open While_syntax
open Sign_analysis

(* programstart *)
let guard = Le (Var "y", Var "n")
let body =
  Seq (If (Eq (Var "z", Num 0),
           Assign ("x", Add (Var "x", Num 1)),
           Assign ("x", Add (Var "x", Var "y"))),
       Assign ("y", Add (Var "y", Num 1)))
let initialize = Seq (Assign ("x", Num 0), Assign ("y", Num 0))
let program = Seq (initialize, While (guard, body))
let input =
  AbsMem.empty |> AbsMem.add "n" Sign.Top |> AbsMem.add "z" Sign.Top
(* programend *)

(* querystart *)
let postcondition = Le (Num 0, Var "x")
let result = eval_c program input
let verdict = eval_b postcondition result
(* queryend *)

let loop_trace : unit -> (AbsMem.t * AbsMem.t) list
= fun () ->
  let entry = eval_c initialize input in
  let rec collect : AbsMem.t -> (AbsMem.t * AbsMem.t) list
  = fun old ->
    let body_result = cond (eval_b guard, eval_c body, (fun _ -> AbsMem.empty)) old in
    let x = AbsMem.join old (AbsMem.join entry body_result) in
    let row = (old, x) in
    if AbsMem.porder x old then [row] else row :: collect x
  in
  collect AbsMem.empty

let print : unit -> unit
= fun () ->
  let states = (AbsMem.empty, AbsMem.empty) :: loop_trace () in
  print_string "        ";
  List.iteri (fun i _ ->
    Printf.printf "%-16s" (if i = 0 then "before" else Printf.sprintf "after %d" i)
  ) states;
  print_newline ();
  print_string "var     ";
  List.iter (fun _ -> Printf.printf "%-8s%-8s" "X" "X'") states;
  print_newline ();
  List.iter (fun var ->
    Printf.printf "%-8s" var;
    List.iter (fun (old, x) ->
      Printf.printf "%-8s%-8s" (Sign.to_string (AbsMem.find var x))
                               (Sign.to_string (AbsMem.find var old))) states;
    print_newline ()) ["x"; "y"; "n"; "z"];
  Printf.printf "assert (x >= 0): %s\n"
    (if verdict = AbsBool.True then "proved" else "not proved")
