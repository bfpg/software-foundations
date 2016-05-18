type bool =
| True
| False

(** val negb : bool -> bool **)

let negb = function
| True -> False
| False -> True

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

(** val plus : nat -> nat -> nat **)

let rec plus n m =
  match n with
  | O -> m
  | S p -> S (plus p m)

(** val mult : nat -> nat -> nat **)

let rec mult n m =
  match n with
  | O -> O
  | S p -> plus m (mult p m)

(** val minus : nat -> nat -> nat **)

let rec minus n m =
  match n with
  | O -> n
  | S k ->
    (match m with
     | O -> n
     | S l -> minus k l)

(** val leb : nat -> nat -> bool **)

let rec leb m x =
  match m with
  | O -> True
  | S m' ->
    (match x with
     | O -> False
     | S n' -> leb m' n')

(** val beq_nat : nat -> nat -> bool **)

let rec beq_nat n m =
  match n with
  | O ->
    (match m with
     | O -> True
     | S n0 -> False)
  | S n1 ->
    (match m with
     | O -> False
     | S m1 -> beq_nat n1 m1)

type id =
  nat
  (* singleton inductive, whose constructor was Id *)

(** val beq_id : id -> id -> bool **)

let beq_id id1 id2 =
  beq_nat id1 id2

type 'a total_map = id -> 'a

(** val t_update : 'a1 total_map -> id -> 'a1 -> id -> 'a1 **)

let t_update m x v x' =
  match beq_id x x' with
  | True -> v
  | False -> m x'

type state = nat total_map

type aexp =
| ANum of nat
| AId of id
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

type bexp =
| BTrue
| BFalse
| BEq of aexp * aexp
| BLe of aexp * aexp
| BNot of bexp
| BAnd of bexp * bexp

(** val aeval : state -> aexp -> nat **)

let rec aeval st = function
| ANum n -> n
| AId x -> st x
| APlus (a1, a2) -> plus (aeval st a1) (aeval st a2)
| AMinus (a1, a2) -> minus (aeval st a1) (aeval st a2)
| AMult (a1, a2) -> mult (aeval st a1) (aeval st a2)

(** val beval : state -> bexp -> bool **)

let rec beval st = function
| BTrue -> True
| BFalse -> False
| BEq (a1, a2) -> beq_nat (aeval st a1) (aeval st a2)
| BLe (a1, a2) -> leb (aeval st a1) (aeval st a2)
| BNot b1 -> negb (beval st b1)
| BAnd (b1, b2) ->
  (match beval st b1 with
   | True -> beval st b2
   | False -> False)

type com =
| CSkip
| CAss of id * aexp
| CSeq of com * com
| CIf of bexp * com * com
| CWhile of bexp * com

(** val ceval_step : state -> com -> nat -> state option **)

let rec ceval_step st c = function
| O -> None
| S i' ->
  (match c with
   | CSkip -> Some st
   | CAss (l, a1) -> Some (t_update st l (aeval st a1))
   | CSeq (c1, c2) ->
     (match ceval_step st c1 i' with
      | Some st' -> ceval_step st' c2 i'
      | None -> None)
   | CIf (b, c1, c2) ->
     (match beval st b with
      | True -> ceval_step st c1 i'
      | False -> ceval_step st c2 i')
   | CWhile (b1, c1) ->
     (match beval st b1 with
      | True ->
        (match ceval_step st c1 i' with
         | Some st' -> ceval_step st' c i'
         | None -> None)
      | False -> Some st))

