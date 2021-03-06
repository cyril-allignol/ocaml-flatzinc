exception TypeError
exception UnboundVariable of string
exception IndexOutOfBounds

module Env : sig
  type t
  val make : unit -> t
end

module IntSet : sig
  type t = Range of int * int | Elements of int list
end

module Expr : sig
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
end

module Decl : sig
  type dtype =
    | Bool
    | Float of (float * float) option
    | Int of IntSet.t option
    | Set of IntSet.t option
    | Array of int option * dtype

  type pred_param = Param of dtype | Var of dtype

  type t =
    | Parameter of { dtype : dtype; name : string; value : Expr.t; }
    | Variable of { dtype : dtype; name : string;
                    annotations : Expr.t list; value : Expr.t option;
                  }
    | PredParam of { dtype : pred_param; name : string; }
end

module Predicate : sig
  type t = { name : string; parameters : Decl.t list; }
end

val declare_parameter : Env.t -> Decl.dtype -> string -> Expr.t -> Decl.t
val declare_variable :
  Env.t -> Decl.dtype -> string -> Expr.t list -> Expr.t option -> Decl.t
val declare_predicate : Env.t -> string -> Decl.t list -> Predicate.t

module Constraint : sig
  type t = {
      name : string;
      args : Expr.t list;
      annotations : Expr.t list;
    }
  val set : Env.t -> string -> Expr.t list -> Expr.t list -> t
end

module Goal : sig
  type t =
    | Satisfy of { annotations : Expr.t list; }
    | Minimize of { annotations : Expr.t list; objective : Expr.t; }
    | Maximize of { annotations : Expr.t list; objective : Expr.t; }
end

module Model : sig
  type t = {
      predicates : Predicate.t list;
      declarations : Decl.t list;
      constraints : Constraint.t list;
      goal : Goal.t;
    }
end
