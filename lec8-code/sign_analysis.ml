open While_syntax

(* signstart *)
module Sign = struct
  type t = Bot | Neg | Zero | Pos | NonPos | NonZero | NonNeg | Top
(* signend *)

(* orderstart *)
  let porder : t -> t -> bool
  = fun a b ->
    match a, b with
    | Bot, _ | _, Top -> true
    | Neg, (NonPos | NonZero) -> true
    | Zero, (NonPos | NonNeg) -> true
    | Pos, (NonNeg | NonZero) -> true
    | _ -> a = b
(* orderend *)

(* joinstart *)
  let join : t -> t -> t
  = fun a b ->
    match a, b with
    | _ when porder b a -> a
    | _ when porder a b -> b
    | Neg, Pos | Pos, Neg -> NonZero
    | Neg, Zero | Zero, Neg -> NonPos
    | Zero, Pos | Pos, Zero -> NonNeg
    | _ -> Top
(* joinend *)

(* alphastart *)
  let alpha : int -> t
  = fun n ->
    if n < 0 then Neg
    else if n = 0 then Zero
    else Pos
(* alphaend *)

(* addstart *)
  let add : t -> t -> t
  = fun a b ->
    match a, b with
    | Bot, _ | _, Bot -> Bot
    | Zero, v | v, Zero -> v
    | Neg, (Neg | NonPos) | NonPos, Neg -> Neg
    | Pos, (Pos | NonNeg) | NonNeg, Pos -> Pos
    | NonPos, NonPos -> NonPos
    | NonNeg, NonNeg -> NonNeg
    | _ -> Top
(* addend *)

(* mulstart *)
  let mul : t -> t -> t
  = fun a b ->
    match a, b with
    | Bot, _ | _, Bot -> Bot
    | Zero, _ | _, Zero -> Zero
    | Pos, v | v, Pos -> v
    | Neg, Neg -> Pos
    | Neg, NonPos | NonPos, Neg
    | NonPos, NonPos | NonNeg, NonNeg -> NonNeg
    | Neg, NonNeg | NonNeg, Neg
    | NonPos, NonNeg | NonNeg, NonPos -> NonPos
    | Neg, NonZero | NonZero, Neg | NonZero, NonZero -> NonZero
    | _ -> Top
(* mulend *)

(* substart *)
  let sub : t -> t -> t
  = fun a b ->
    match a, b with
    | Bot, _ | _, Bot -> Bot
    | v, Zero -> v
    | Neg, (Pos | NonNeg) | (Zero | NonPos), Pos -> Neg
    | Pos, (Neg | NonPos) | (Zero | NonNeg), Neg -> Pos
    | (Zero | NonPos), NonNeg -> NonPos
    | (Zero | NonNeg), NonPos -> NonNeg
    | Zero, NonZero -> NonZero
    | _ -> Top
(* subend *)

  let to_string : t -> string
  = fun s ->
    match s with
    | Bot -> "Bottom" | Neg -> "Neg" | Zero -> "Zero" | Pos -> "Pos"
    | NonPos -> "Non-Pos" | NonZero -> "Non-Zero" | NonNeg -> "Non-Neg"
    | Top -> "Top"
end

(* statestart *)
module AbsMem = struct
  module Map = Map.Make(String)  (* Keys are variable names. *)
  type t = Sign.t Map.t          (* A map from variables to signs. *)
  let empty = Map.empty          (* bottom memory state *)
  let add = Map.add
  let find x m = try Map.find x m with Not_found -> Sign.Bot
(* stateend *)

(* stateopsstart *)
  let porder : t -> t -> bool
  = fun m n ->
    Map.for_all (fun x v -> Sign.porder v (find x n)) m

  let join : t -> t -> t
  = fun m n ->
    Map.union (fun _ a b -> Some (Sign.join a b)) m n
(* stateopsend *)
end

(* truthstart *)
module AbsBool = struct
  type t = Bot | True | False | Top

  let porder : t -> t -> bool
  = fun a b ->
    a = b || a = Bot || b = Top
(* truthend *)

(* eqstart *)
  let eq : Sign.t -> Sign.t -> t
  = fun a b ->
    match a, b with
    | Sign.Bot, _ | _, Sign.Bot -> Bot
    | Sign.Zero, Sign.Zero -> True
    | Sign.Neg, (Sign.Zero | Sign.Pos | Sign.NonNeg)
    | Sign.Zero, (Sign.Neg | Sign.Pos | Sign.NonZero)
    | Sign.Pos, (Sign.Neg | Sign.Zero | Sign.NonPos)
    | Sign.NonPos, Sign.Pos | Sign.NonNeg, Sign.Neg
    | Sign.NonZero, Sign.Zero -> False
    | _ -> Top
(* eqend *)

(* lestart *)
  let leq : Sign.t -> Sign.t -> t
  = fun a b ->
    match a, b with
    | Sign.Bot, _ | _, Sign.Bot -> Bot
    | (Sign.Neg | Sign.Zero | Sign.NonPos),
      (Sign.Zero | Sign.Pos | Sign.NonNeg) -> True
    | (Sign.Zero | Sign.NonNeg), Sign.Neg
    | Sign.Pos, (Sign.Neg | Sign.Zero | Sign.NonPos) -> False
    | _ -> Top
(* leend *)

(* notstart *)
  let not : t -> t
  = fun b ->
    match b with
    | Bot -> Bot | True -> False | False -> True | Top -> Top
(* notend *)

(* andstart *)
  let band : t -> t -> t
  = fun a b ->
    match a, b with
    | Bot, _ | _, Bot -> Bot
    | True, v | v, True -> v
    | False, _ | _, False -> False
    | Top, Top -> Top
(* andend *)
end

(* evalastart *)
let rec eval_a : aexp -> AbsMem.t -> Sign.t
= fun a s ->
  match a with
  | Num n -> Sign.alpha n
  | Var x -> AbsMem.find x s
  | Add (a1, a2) -> Sign.add (eval_a a1 s) (eval_a a2 s)
  | Mul (a1, a2) -> Sign.mul (eval_a a1 s) (eval_a a2 s)
  | Sub (a1, a2) -> Sign.sub (eval_a a1 s) (eval_a a2 s)
(* evalaend *)

(* evalbstart *)
let rec eval_b : bexp -> AbsMem.t -> AbsBool.t
= fun b s ->
  match b with
  | True -> AbsBool.True
  | False -> AbsBool.False
  | Eq (a1, a2) -> AbsBool.eq (eval_a a1 s) (eval_a a2 s)
  | Le (a1, a2) -> AbsBool.leq (eval_a a1 s) (eval_a a2 s)
  | Not b -> AbsBool.not (eval_b b s)
  | And (b1, b2) -> AbsBool.band (eval_b b1 s) (eval_b b2 s)
(* evalbend *)

(* evalcstart *)
let rec eval_c : stmt -> AbsMem.t -> AbsMem.t
= fun c s ->
  match c with
  | Assign (x, a) -> AbsMem.add x (eval_a a s) s
  | Skip -> s
  | Seq (c1, c2) -> eval_c c2 (eval_c c1 s)
  | If (b, c1, c2) -> cond (eval_b b, eval_c c1, eval_c c2) s
  | While (b, body) ->
    let onestep : AbsMem.t -> AbsMem.t
    = fun x ->
      AbsMem.join s
        (cond (eval_b b, eval_c body, (fun _ -> AbsMem.empty)) x) in
    let rec fix f x i =
      let x' = x in
      let x = AbsMem.join x (f x) in
      if AbsMem.porder x x' then x'
      else fix f x (i + 1)
    in
    fix onestep AbsMem.empty 1
(* evalcend *)

(* condstart *)
and cond (f, g, h) m =
  match f m with
  | AbsBool.Bot -> AbsMem.empty
  | AbsBool.True -> g m
  | AbsBool.False -> h m
  | AbsBool.Top -> AbsMem.join (g m) (h m)
(* condend *)
