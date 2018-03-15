type set =
  | Range of int * int
  | Elements of int list

let int_for_all = fun p lb ub ->
  let rec iter = fun i ->
    if i > ub then true else p i && iter (i + 1) in
  iter lb

let subset = fun s1 s2 ->
  match s1, s2 with
  | Range (lb1, ub1), Range (lb2, ub2) -> lb2 <= lb1 && ub1 <= ub2
  | Elements l1, Elements l2 -> List.for_all (fun x -> List.mem x l2) l1
  | Elements l1, Range (lb, ub) -> List.for_all (fun x -> lb <= x && x <= ub) l1
  | Range (lb, ub), Elements l -> int_for_all (fun x -> List.mem x l) lb ub

let mem = fun s i ->
  match s with
  | Range (i1, i2) -> i1 <= i && i <= i2
  | Elements l -> List.mem i l

module Expr = struct

  type t =
    | Bool of bool
    | Float of float
    | Int of int
    | Set of set
    | Var of string
    | Elt of string * int
    | Array of t array
    | Annot of string
    | Pred of string * t list
    | String of string

  let rec literal = function
    | Bool _ | Float _ | Int _ | Set _ -> true
    | Var _ | Elt _ | Annot _ | Pred _ | String _ -> false
    | Array a -> Array.for_all literal a

end

exception TypeError

module Decl = struct

  type dtype =
    | Bool
    | Float of (float * float) option
    | Int of set option
    | Set of set option
    | Array of int option * dtype

  type pred_param =
    | Param of dtype
    | Var of dtype

  type t =
    | Parameter of { dtype : dtype; name : string; value : Expr.t }
    | Variable of { dtype : dtype; name : string;
                    annotations : Expr.t list;
                    value : Expr.t option }
    | PredParam of { dtype : pred_param; name : string }

  let rec subtype = fun t1 t2 ->
    match t1, t2 with
    | Bool, Bool -> true
    | Float _, Float None -> true
    | Float (Some (lb1, ub1)), Float (Some (lb2, ub2)) ->
       lb2 <= lb1 && ub1 <= ub2
    | Int _, Int None -> true
    | Int (Some s1), Int (Some s2) -> subset s1 s2
    | Set _, Set None -> true
    | Set (Some s1), Set (Some s2) -> subset s1 s2
    | Array (_, t1), Array (None, t2) -> subtype t1 t2
    | Array (Some n1, t1), Array (Some n2, t2) -> n1 = n2 && subtype t1 t2
    | _, _ -> false

  let rec compatible_expr = fun dtype expr ->
    match dtype, expr with
    | Bool, Expr.Bool _ | Float None, Expr.Float _
      | Int None, Expr.Int _ | Set None, Expr.Set _ -> true
    | Float (Some (f1, f2)), Expr.Float f -> f1 <= f && f <= f2
    | Int (Some s), Expr.Int i -> mem s i
    | Set (Some s1), Expr.Set s2 -> subset s2 s1
    | Array (i, t), Expr.Array a ->
       (match i with None -> true | Some n -> n = Array.length a) &&
         Array.for_all (compatible_expr t) a
    (* TODO: non-literal cases *)
    | _ -> false

  let parameter = fun dtype name expr ->
    if Expr.literal expr && compatible_expr dtype expr
    then Parameter { dtype; name; value = expr }
    else raise TypeError

  let variable = fun dtype name annotations expr ->
    match expr with
    | None -> Variable { dtype; name; annotations; value = None }
    | Some e ->
       if compatible_expr dtype e
       then Variable { dtype; name; annotations; value = expr }
       else raise TypeError

end

module Predicate = struct

  type t = { name : string; parameters : Decl.t list }

end

module Constraint = struct

  type t = { name : string;
             args : Expr.t list;
             annotations : Expr.t list }

end

module Goal = struct

  type t =
    | Satisfy of { annotations : Expr.t list }
    | Minimize of { annotations : Expr.t list; objective : Expr.t }
    | Maximize of { annotations : Expr.t list; objective : Expr.t }

end

module Model = struct

  type t = { predicates : Predicate.t list;
             declarations : Decl.t list;
             constraints : Constraint.t list;
             goal : Goal.t }

end
