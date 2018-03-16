(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

module IntSet = struct

  type t =
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
    | Elements l1, Range (lb, ub) ->
       List.for_all (fun x -> lb <= x && x <= ub) l1
    | Range (lb, ub), Elements l -> int_for_all (fun x -> List.mem x l) lb ub

  let mem = fun s i ->
    match s with
    | Range (i1, i2) -> i1 <= i && i <= i2
    | Elements l -> List.mem i l

end

module Expr = struct

  type t =
    | Bool of bool
    | Float of float
    | Int of int
    | Set of IntSet.t
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
exception UnboundVariable of string

module Decl = struct

  type dtype =
    | Bool
    | Float of (float * float) option
    | Int of IntSet.t option
    | Set of IntSet.t option
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
    | Int (Some s1), Int (Some s2) -> IntSet.subset s1 s2
    | Set _, Set None -> true
    | Set (Some s1), Set (Some s2) -> IntSet.subset s1 s2
    | Array (_, t1), Array (None, t2) -> subtype t1 t2
    | Array (Some n1, t1), Array (Some n2, t2) -> n1 = n2 && subtype t1 t2
    | _, _ -> false

end

module Predicate = struct

  type t = { name : string; parameters : Decl.t list }

end

module Env = struct

  type t = { predicates : (string, Predicate.t) Hashtbl.t;
             vars : (string, Decl.t) Hashtbl.t }

  let make = fun () ->
    { predicates = Hashtbl.create 17; vars = Hashtbl.create 17 }

  let find_predicate = fun env name ->
    try Hashtbl.find env.predicates name
    with Not_found -> raise (UnboundVariable name)

  let find_variable = fun env name ->
    try Hashtbl.find env.vars name
    with Not_found -> raise (UnboundVariable name)

  let register_predicate = fun env pred ->
    Hashtbl.add env.predicates pred.Predicate.name pred

  let register_var = fun env var ->
    match var with
    | Decl.Variable { dtype; name; annotations; value } ->
       Hashtbl.add env.vars name var
    | Decl.Parameter { dtype; name; value } ->
       Hashtbl.add env.vars name var
    | Decl.PredParam _ -> failwith "Syntax.Env.register_var: unreachable"

end

let rec compatible_expr = fun dtype expr ->
  let open Decl in
  match dtype, expr with
  | Bool, Expr.Bool _ | Float None, Expr.Float _
    | Int None, Expr.Int _ | Set None, Expr.Set _ -> true
  | Float (Some (f1, f2)), Expr.Float f -> f1 <= f && f <= f2
  | Int (Some s), Expr.Int i -> IntSet.mem s i
  | Set (Some s1), Expr.Set s2 -> IntSet.subset s2 s1
  | Array (i, t), Expr.Array a ->
     (match i with None -> true | Some n -> n = Array.length a) &&
       Array.for_all (compatible_expr t) a
  (* TODO: non-literal cases *)
  | _ -> false

let declare_parameter = fun env dtype name expr ->
  let p =
    if Expr.literal expr && compatible_expr dtype expr
    then Decl.Parameter { dtype; name; value = expr }
    else raise TypeError in
  Env.register_var env p;
  p

let declare_variable = fun env dtype name annotations expr ->
  let v = match expr with
    | None -> Decl.Variable { dtype; name; annotations; value = None }
    | Some e ->
       if compatible_expr dtype e
       then Decl.Variable { dtype; name; annotations; value = expr }
       else raise TypeError in
  Env.register_var env v;
  v

let declare_predicate = fun env name parameters ->
  let pred = Predicate.{ name; parameters } in
  Env.register_predicate env pred;
  pred

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
