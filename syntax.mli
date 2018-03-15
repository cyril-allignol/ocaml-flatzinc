type set = Range of int * int | Elements of int list
val int_for_all : (int -> bool) -> int -> int -> bool
val subset : set -> set -> bool
val mem : set -> int -> bool
module Expr :
  sig
    type t =
        Bool of bool
      | Float of float
      | Int of int
      | Set of set
      | Var of string
      | Elt of string * int
      | Array of t array
      | Annot of string
      | Pred of string * t list
      | String of string
    val literal : t -> bool
  end
exception TypeError
module Decl :
  sig
    type dtype =
        Bool
      | Float of (float * float) option
      | Int of set option
      | Set of set option
      | Array of int option * dtype
    type pred_param = Param of dtype | Var of dtype
    type t =
        Parameter of { dtype : dtype; name : string; value : Expr.t; }
      | Variable of { dtype : dtype; name : string;
          annotations : Expr.t list; value : Expr.t option;
        }
      | PredParam of { dtype : pred_param; name : string; }
    val subtype : dtype -> dtype -> bool
    val compatible_expr : dtype -> Expr.t -> bool
    val parameter : dtype -> string -> Expr.t -> t
    val variable : dtype -> string -> Expr.t list -> Expr.t option -> t
  end
module Predicate :
  sig type t = { name : string; parameters : Decl.t list; } end
module Constraint :
  sig
    type t = {
      name : string;
      args : Expr.t list;
      annotations : Expr.t list;
    }
  end
module Goal :
  sig
    type t =
        Satisfy of { annotations : Expr.t list; }
      | Minimize of { annotations : Expr.t list; objective : Expr.t; }
      | Maximize of { annotations : Expr.t list; objective : Expr.t; }
  end
module Model :
  sig
    type t = {
      predicates : Predicate.t list;
      declarations : Decl.t list;
      constraints : Constraint.t list;
      goal : Goal.t;
    }
  end
