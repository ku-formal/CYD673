(* aexpstart *)
type aexp =
  | Num of int | Var of string
  | Add of aexp * aexp | Mul of aexp * aexp | Sub of aexp * aexp
(* aexpend *)

(* bexpstart *)
type bexp =
  | True | False | Eq of aexp * aexp | Le of aexp * aexp
  | Not of bexp | And of bexp * bexp
(* bexpend *)

(* stmtstart *)
type stmt =
  | Assign of string * aexp | Skip | Seq of stmt * stmt
  | If of bexp * stmt * stmt | While of bexp * stmt
(* stmtend *)
