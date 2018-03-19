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
exception IndexOutOfBounds

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

  let get_dtype = function
    | Parameter { dtype } -> dtype
    | Variable { dtype } -> dtype
    | PredParam { dtype = Param dtype } -> dtype
    | PredParam { dtype = Var dtype } -> dtype

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

  let applicable = fun d1 d2 ->
    match d1, d2 with
    | Variable _, PredParam { dtype = Param _ } -> false
    | Variable { dtype = t1 }, PredParam { dtype = Var t2 } -> subtype t1 t2
    | Parameter { dtype = t1 }, PredParam { dtype = Var t2 } -> subtype t1 t2
    | Parameter { dtype = t1 }, PredParam { dtype = Param t2 } -> subtype t1 t2
    | _ -> failwith "Syntax.Decl.applicable: unreachable"

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

let rec compatible_expr = fun env dtype expr ->
  let open Decl in
  match dtype, expr with
  | Bool, Expr.Bool _ | Float None, Expr.Float _
    | Int None, Expr.Int _ | Set None, Expr.Set _ -> true
  | Float (Some (f1, f2)), Expr.Float f -> f1 <= f && f <= f2
  | Int (Some s), Expr.Int i -> IntSet.mem s i
  | Set (Some s1), Expr.Set s2 -> IntSet.subset s2 s1
  | Array (i, t), Expr.Array a ->
     (match i with None -> true | Some n -> n = Array.length a)
     && Array.for_all (compatible_expr env t) a
  | _, Expr.Var v ->
     Decl.subtype (Decl.get_dtype (Env.find_variable env v)) dtype
  | _, Expr.Elt (a, i) ->
     begin match Decl.get_dtype (Env.find_variable env a) with
     | Array (Some n, atype) -> 1 <= i && i <= n && Decl.subtype atype dtype
     | _ -> false end
  | _ -> false

let declare_parameter = fun env dtype name expr ->
  let p =
    if Expr.literal expr && compatible_expr env dtype expr
    then Decl.Parameter { dtype; name; value = expr }
    else raise TypeError in
  Env.register_var env p;
  p

let declare_variable = fun env dtype name annotations expr ->
  let v = match expr with
    | None -> Decl.Variable { dtype; name; annotations; value = None }
    | Some e ->
       if compatible_expr env dtype e
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

  let set = fun env name args annotations ->
    let pred = Env.find_predicate env name in
    if List.for_all2 (fun predparam arg ->
           match predparam with
           | Decl.Parameter _ | Decl.Variable _ ->
              failwith "Syntax.Constraint.set: unreachable"
           | Decl.PredParam { dtype } ->
              let t1 = Decl.get_dtype predparam in
              match arg with
              | Expr.Bool _ | Expr.Float _ | Expr.Int _ | Expr.Set _
                | Expr.Array _ -> compatible_expr env t1 arg
              | Expr.Annot _ | Expr.Pred _ | Expr.String _ -> false
              | Expr.Var v ->
                 let dv = Env.find_variable env v in
                 Decl.applicable dv predparam
              | Expr.Elt (a, i) ->
                 let da = Env.find_variable env a in
                 let elt_type =
                   match da with
                   | Decl.Variable { dtype = Decl.Array (Some n, t2) } ->
                      if i <= n then Decl.Var t2 else raise IndexOutOfBounds
                   | Decl.Parameter { dtype = Decl.Array (Some n, t2) } ->
                      if i <= n then Decl.Param t2 else raise IndexOutOfBounds
                   | _ -> failwith "Syntax.Constraint.set: unreachable (2)" in
                 match dtype, elt_type with
                 | Decl.Var t1, Decl.Var t2
                   | Decl.Var t1, Decl.Param t2
                   | Decl.Param t1, Decl.Param t2 -> Decl.subtype t2 t1
                 | Decl.Param _, Decl.Var _ -> false
         ) pred.Predicate.parameters args then
      { name; args; annotations }
    else raise TypeError

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
