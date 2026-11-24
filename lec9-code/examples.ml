open While_syntax
open Taint_analysis

(* propagationstart *)
let positive_i = Not (Le (Var "i", Num 0))
let propagate =
  Seq (Assign ("x", Var "y"),
       Seq (Assign ("y", Var "secret"),
            Assign ("i", Sub (Var "i", Num 1))))
let propagation = While (positive_i, propagate)
let public_inputs = ["i"; "x"; "y"]
let public_outputs = ["x"]
let propagation_entry =
  initial_state propagation public_inputs public_outputs
(* propagationend *)

(* tracestart *)
let propagation_result = eval_c propagation propagation_entry
let propagation_verdict = check propagation public_inputs public_outputs
(* traceend *)

let show_label : label -> string
= fun l ->
  match l with Low -> "low" | High -> "high"
let show_verdict : verdict -> string
= fun v ->
  match v with Yes -> "YES!" | Unknown -> "NO?"

let propagation_trace : unit -> AbsMem.t list
= fun () ->
  let init = AbsMem.all_low propagation_entry in
  let rec collect : AbsMem.t -> AbsMem.t list
  = fun old ->
    let body_result =
      cond (eval_b positive_i, eval_c propagate, (fun _ -> init)) old
    in
    let x = AbsMem.join old (AbsMem.join propagation_entry body_result) in
    if AbsMem.porder x old then [x] else x :: collect x
  in
  init :: collect init

let run : unit -> unit
= fun () ->
  print_endline "Lecture 5, p.27";
  let states = propagation_trace () in
  Printf.printf "%-8s" "";
  List.iteri (fun k _ -> Printf.printf "  X%-3d" k) states;
  print_newline ();
  let row : string -> (AbsMem.t -> label) -> unit
  = fun name label ->
    Printf.printf "%-8s" name;
    List.iter (fun s -> Printf.printf "  %-4s" (show_label (label s))) states;
    print_newline ()
  in
  List.iter (fun name -> row name (AbsMem.find name))
    ["i"; "x"; "y"; "secret"; "history"];
  Printf.printf "\nRequested public output %s: %s\n"
    (String.concat ", " public_outputs) (show_verdict propagation_verdict)
