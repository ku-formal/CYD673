open While_syntax

(* labelsstart *)
type label = Low | High

let porder : label -> label -> bool
= fun a b ->
  a = Low || b = High

let join : label -> label -> label
= fun a b ->
  if a = High || b = High then High else Low
(* labelsend *)

(* statestart *)
module AbsMem = struct
  module Map = Map.Make(String)  (* Map module with string keys. *)
  type t = label Map.t           (* A state maps names to labels. *)
  let add = Map.add
  let find x m = try Map.find x m with Not_found -> Low
(* stateend *)

(* boundsstart *)
  let init = Map.singleton "history" Low
  let all_low : t -> t = Map.map (fun _ -> Low)

  let lost : t -> t
  = fun m ->
    Map.add "history" High (Map.map (fun _ -> High) m)

  let proper : t -> bool
  = fun m ->
    find "history" m = Low
(* boundsend *)

(* stateorderstart *)
  let porder : t -> t -> bool
  = fun m n ->
    Map.for_all (fun x l -> porder l (find x n)) m
(* stateorderend *)

(* statejoinstart *)
  let join : t -> t -> t
  = fun m n ->
    Map.union (fun _ a b -> Some (join a b)) m n
(* statejoinend *)
end

(* arithstart *)
let rec eval_a : aexp -> AbsMem.t -> label
= fun a s ->
  match a with
  | Num _ -> AbsMem.find "history" s
  | Var x -> join (AbsMem.find x s) (AbsMem.find "history" s)
  | Add (a1, a2) | Mul (a1, a2) | Sub (a1, a2) ->
    join (eval_a a1 s) (eval_a a2 s)
(* arithend *)

(* boolstart *)
let rec eval_b : bexp -> AbsMem.t -> label
= fun b s ->
  match b with
  | True | False -> AbsMem.find "history" s
  | Eq (a1, a2) | Le (a1, a2) -> join (eval_a a1 s) (eval_a a2 s)
  | Not b -> eval_b b s
  | And (b1, b2) -> join (eval_b b1 s) (eval_b b2 s)
(* boolend *)

(* evalcstart *)
let rec eval_c : stmt -> AbsMem.t -> AbsMem.t
= fun c s ->
  match c with
  | Assign (x, a) -> AbsMem.add x (eval_a a s) s
  | Skip -> s
  | Seq (c1, c2) -> eval_c c2 (eval_c c1 s)
  | If (b, c1, c2) -> cond (eval_b b, eval_c c1, eval_c c2) s
  | While (b, body) ->
    let init = AbsMem.all_low s in
    let onestep : AbsMem.t -> AbsMem.t
    = fun x ->
      AbsMem.join s (cond (eval_b b, eval_c body, (fun _ -> init)) x) in
    let rec fix f x i =
      let x_old = x in
      let x = AbsMem.join x (f x) in
      if AbsMem.porder x x_old then x
      else fix f x (i + 1)
    in
    fix onestep init 1
(* evalcend *)

(* condstart *)
and cond (f, h1, h2) s =
  match f s with
  | Low  -> AbsMem.join (h1 s) (h2 s)
  | High -> AbsMem.lost s
(* condend *)

module Names = Set.Make(String)

let rec variables_a = function
  | Num _ -> Names.empty
  | Var x -> Names.singleton x
  | Add (a, b) | Mul (a, b) | Sub (a, b) ->
    Names.union (variables_a a) (variables_a b)

let rec variables_b = function
  | True | False -> Names.empty
  | Eq (a, b) | Le (a, b) -> Names.union (variables_a a) (variables_a b)
  | Not b -> variables_b b
  | And (a, b) -> Names.union (variables_b a) (variables_b b)

let rec variables = function
  | Assign (x, a) -> Names.add x (variables_a a)
  | Skip -> Names.empty
  | Seq (c1, c2) -> Names.union (variables c1) (variables c2)
  | If (b, c1, c2) ->
    Names.union (variables_b b) (Names.union (variables c1) (variables c2))
  | While (b, c) -> Names.union (variables_b b) (variables c)

(* initialstart *)
let initial_state : stmt -> string list -> string list -> AbsMem.t
= fun c public_inputs public_outputs ->
  let names = Names.union (variables c) (Names.of_list public_outputs) in
  let set_high x s = AbsMem.add x High s in
  let set_low s x = AbsMem.add x Low s in
  let s = Names.fold set_high names AbsMem.init in
  List.fold_left set_low s public_inputs
(* initialend *)

(* checkstart *)
type verdict = Yes | Unknown

let check : stmt -> string list -> string list -> verdict
= fun c public_inputs public_outputs ->
  let s = eval_c c (initial_state c public_inputs public_outputs) in
  let is_low x = AbsMem.find x s = Low in
  if AbsMem.proper s && List.for_all is_low public_outputs
  then Yes else Unknown
(* checkend *)
