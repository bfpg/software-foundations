(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type 'a option =
| Some of 'a
| None

(** val plus : int -> int -> int **)

let rec plus = ( + )

(** val mult : int -> int -> int **)

let rec mult = ( * )

(** val minus : int -> int -> int **)

let rec minus n m =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ ->
    n)
    (fun k ->
    (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
      (fun _ ->
      n)
      (fun l ->
      minus k l)
      m)
    n

(** val leb : int -> int -> bool **)

let rec leb m x =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ ->
    true)
    (fun m' ->
    (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
      (fun _ ->
      false)
      (fun n' ->
      leb m' n')
      x)
    m

(** val beq_nat : int -> int -> bool **)

let rec beq_nat = ( = )

type id =
  int
  (* singleton inductive, whose constructor was Id *)

(** val beq_id : id -> id -> bool **)

let beq_id id1 id2 =
  beq_nat id1 id2

type 'a total_map = id -> 'a

(** val t_update : 'a1 total_map -> id -> 'a1 -> id -> 'a1 **)

let t_update m x v x' =
  if beq_id x x' then v else m x'

type state = int total_map

type aexp =
| ANum of int
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

(** val aeval : state -> aexp -> int **)

let rec aeval st = function
| ANum n -> n
| AId x -> st x
| APlus (a1, a2) -> plus (aeval st a1) (aeval st a2)
| AMinus (a1, a2) -> minus (aeval st a1) (aeval st a2)
| AMult (a1, a2) -> mult (aeval st a1) (aeval st a2)

(** val beval : state -> bexp -> bool **)

let rec beval st = function
| BTrue -> true
| BFalse -> false
| BEq (a1, a2) -> beq_nat (aeval st a1) (aeval st a2)
| BLe (a1, a2) -> leb (aeval st a1) (aeval st a2)
| BNot b1 -> negb (beval st b1)
| BAnd (b1, b2) -> if beval st b1 then beval st b2 else false

type com =
| CSkip
| CAss of id * aexp
| CSeq of com * com
| CIf of bexp * com * com
| CWhile of bexp * com

(** val ceval_step : state -> com -> int -> state option **)

let rec ceval_step st c i =
  (fun zero succ n ->
      if n=0 then zero () else succ (n-1))
    (fun _ ->
    None)
    (fun i' ->
    match c with
    | CSkip -> Some st
    | CAss (l, a1) -> Some (t_update st l (aeval st a1))
    | CSeq (c1, c2) ->
      (match ceval_step st c1 i' with
       | Some st' -> ceval_step st' c2 i'
       | None -> None)
    | CIf (b, c1, c2) ->
      if beval st b then ceval_step st c1 i' else ceval_step st c2 i'
    | CWhile (b1, c1) ->
      if beval st b1
      then (match ceval_step st c1 i' with
            | Some st' -> ceval_step st' c i'
            | None -> None)
      else Some st)
    i

