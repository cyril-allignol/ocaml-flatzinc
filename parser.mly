/* Copyright 2018 Cyril Allignol
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
 * under the License. */

%{
    open Syntax
    exception InvalidArrayBounds
    exception InvalidRange
    let env = Env.make ()
%}
%token VAR CONSTRAINT
%token BOOL FLOAT INT ARRAY SET OF
%token PREDICATE SATISFY MAXIMIZE MINIMIZE SOLVE
%token COMMA COLON SEMICOLON EQ DOUBLEDOT DOUBLECOLON
%token LP RP LSB RSB LCB RCB
%token <string> IDENTIFIER U_IDENTIFIER STRING_LIT
%token <bool> BOOL_LIT
%token <int> INT_LIT
%token <float> FLOAT_LIT
%start model
%type <Syntax.Model.t> model
%%

model :
| predicates = list(pred_decl);
  declarations = list(param_or_var_decl);
  constraints = list(constraint_);
  goal = solve_goal;
  { Model.{ predicates; declarations; constraints; goal } }

pred_decl :
| PREDICATE; name = IDENTIFIER;
  LP; parameters = separated_nonempty_list(COMMA, pred_param); RP; SEMICOLON;
  { declare_predicate env name parameters }

pred_param :
| t = pred_param_type; COLON; id = pred_ann_identifier;
  { Decl.PredParam { dtype = t; name = id } }

pred_param_type :
| t = par_pred_param_type { Decl.Param t }
| t = var_pred_param_type { Decl.Var t }

param_or_var_decl :
| d = param_decl { d }
| d = var_decl { d }

par_type :
| BOOL { Decl.Bool }
| FLOAT { Decl.Float None }
| INT { Decl.Int None }
| SET OF INT { Decl.Set None }
| ARRAY LSB; i = index_set RSB OF BOOL { Decl.Array (i, Decl.Bool) }
| ARRAY LSB; i = index_set RSB OF FLOAT { Decl.Array (i, Decl.Float None) }
| ARRAY LSB; i = index_set RSB OF INT { Decl.Array (i, Decl.Int None) }
| ARRAY LSB; i = index_set RSB OF SET OF INT { Decl.Array (i, Decl.Set None) }

par_pred_param_type :
| t = par_type { t }
| f1 = FLOAT_LIT; DOUBLEDOT; f2 = FLOAT_LIT;
  { if f1 <= f2 then Decl.Float (Some (f1, f2)) else raise InvalidRange }
| i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Int (Some (IntSet.Range (i1, i2)))
    else raise InvalidRange }
| LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Int (Some (IntSet.Elements l)) }
| SET OF; i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Set (Some (IntSet.Range (i1, i2)))
    else raise InvalidRange }
| SET OF LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Set (Some (IntSet.Elements l)) }
| ARRAY LSB; i = index_set; RSB OF;
  f1 = FLOAT_LIT; DOUBLEDOT; f2 = FLOAT_LIT;
  { if f1 <= f2 then Decl.Array (i, Decl.Float (Some (f1, f2)))
    else raise InvalidRange }
| ARRAY LSB; i = index_set; RSB OF; i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Array (i, Decl.Int (Some (IntSet.Range (i1, i2))))
    else raise InvalidRange }
| ARRAY LSB; i = index_set; RSB OF;
  LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Array (i, Decl.Int (Some (IntSet.Elements l))) }
| ARRAY LSB; i = index_set; RSB OF SET OF;
  i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Array (i, Decl.Set (Some (IntSet.Range (i1, i2))))
    else raise InvalidRange }
| ARRAY LSB; i = index_set; RSB OF SET OF;
  LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Array (i, Decl.Set (Some (IntSet.Elements l))) }

var_type :
| VAR BOOL { Decl.Bool }
| VAR FLOAT { Decl.Float None }
| VAR; f1 = FLOAT_LIT; DOUBLEDOT; f2 = FLOAT_LIT;
  { if f1 <= f2 then Decl.Float (Some (f1, f2)) else raise InvalidRange }
| VAR INT { Decl.Int None }
| VAR; i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Int (Some (IntSet.Range (i1, i2)))
    else raise InvalidRange }
| VAR LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Int (Some (IntSet.Elements l)) }
| VAR SET OF; i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Set (Some (IntSet.Range (i1, i2)))
    else raise InvalidRange }
| VAR SET OF LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Set (Some (IntSet.Elements l)) }
| ARRAY LSB; i = index_set; RSB OF VAR BOOL { Decl.Array (i, Decl.Bool) }
| ARRAY LSB; i = index_set; RSB OF VAR FLOAT { Decl.Array (i, Decl.Float None) }
| ARRAY LSB; i = index_set; RSB OF VAR;
  f1 = FLOAT_LIT; DOUBLEDOT; f2 = FLOAT_LIT;
  { if f1 <= f2 then Decl.Array (i, Decl.Float (Some (f1, f2)))
    else raise InvalidRange }
| ARRAY LSB; i = index_set; RSB OF VAR INT { Decl.Array (i, Decl.Int None) }
| ARRAY LSB; i = index_set; RSB OF VAR; i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Array (i, Decl.Int (Some (IntSet.Range (i1, i2))))
    else raise InvalidRange }
| ARRAY LSB; i = index_set; RSB OF VAR;
  LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Array (i, Decl.Int (Some (IntSet.Elements l))) }
| ARRAY LSB; i = index_set; RSB OF VAR SET OF;
  i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then Decl.Array (i, Decl.Set (Some (IntSet.Range (i1, i2))))
    else raise InvalidRange}
| ARRAY LSB; i = index_set; RSB OF VAR SET OF;
  LCB; l = separated_nonempty_list(COMMA, INT_LIT); RCB;
  { Decl.Array (i, Decl.Set (Some (IntSet.Elements l))) }

var_pred_param_type :
| t = var_type { t }
| VAR SET OF INT { Decl.Set None }
| ARRAY LSB; i = index_set; RSB OF VAR SET OF INT;
  { Decl.Array (i, Decl.Set None) }

index_set :
| i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <> 1 || i2 < i1 then raise InvalidArrayBounds else Some i2 }
| INT { None }

expr :
| l = literal { l }
| v = var_par_id { Expr.Var v }
| v = var_par_id; LSB; i = INT_LIT; RSB { Expr.Elt (v, i) }
| a = array_expr { Expr.Array a }

literal :
| b = BOOL_LIT { Expr.Bool b }
| f = FLOAT_LIT { Expr.Float f }
| i = INT_LIT { Expr.Int i }
| s = set_const { Expr.Set s }

par_expr :
| l = literal { l }
| LSB; l = separated_list(COMMA, literal); RSB { Expr.Array (Array.of_list l) }

annot_expr :
| e = expr { e }
| a = annotation { a }
| s = STRING_LIT { Expr.String s }

pred_ann_identifier :
| id = IDENTIFIER { id }

var_par_id :
| id = IDENTIFIER { id }
| id = U_IDENTIFIER { id }

set_const :
| i1 = INT_LIT; DOUBLEDOT; i2 = INT_LIT;
  { if i1 <= i2 then IntSet.Range (i1, i2) else raise InvalidRange }
| LCB; l = separated_list(COMMA, INT_LIT); RCB { IntSet.Elements l }

array_expr :
| LSB; l = separated_list(COMMA, expr); RSB { Array.of_list l }

param_decl :
| t = par_type; COLON; v = var_par_id; EQ; e = par_expr; SEMICOLON;
  { declare_parameter env t v e }

var_decl :
| t = var_type; COLON; v = var_par_id; a = annotations; SEMICOLON;
  { declare_variable env t v a None }
| t = var_type; COLON; v = var_par_id; a = annotations; EQ; e = expr; SEMICOLON;
  { declare_variable env t v a (Some e) }

constraint_ :
| CONSTRAINT; id = pred_ann_identifier;
  LP; l = separated_nonempty_list(COMMA, expr); RP; a = annotations; SEMICOLON;
  { Constraint.set env id l a }

solve_goal :
| SOLVE; a = annotations; SATISFY SEMICOLON;
  { Goal.Satisfy { annotations = a } }
| SOLVE; a = annotations; MINIMIZE; e = expr; SEMICOLON;
  { Goal.Minimize { annotations = a; objective = e } }
| SOLVE; a = annotations; MAXIMIZE; e = expr; SEMICOLON;
  { Goal.Maximize { annotations = a; objective = e } }

annotations :
| a = list(annotation) { a }

annotation :
| DOUBLECOLON; id = pred_ann_identifier { Expr.Annot id }
| DOUBLECOLON; id = pred_ann_identifier;
  LP; args = separated_nonempty_list(COMMA, annot_expr); RP;
  { Expr.Pred (id, args) }
