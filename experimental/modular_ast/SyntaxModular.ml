(* This module defines the primiitve syntax elements of the source language
 * A primitive syntax describes only the "shape" of a single sytax element:
 * 
 * For instance, the primitive "p_exer" syntax lists defines all possible alternatives
 * for an expression, but the type of concrete type of each case is paramertrized, 
 * and this includes the recursive "expression" type itself.  
 *
 * The idea this, anything that constitutes a AST "subtree" must be abstracted using 
 * a type variable.
 * 
 * With the primitive syntax, one can easily connect them and form a concrete
 * synatx, or swap out the primitive syntax for, say, expr for another without having
 * to rewrite the vast majority of code use for other syntax elements. They also make
 * constructing aggregation functions such as map/reduce simpler.
 *
 * To achieve recursiveness in data structure we usually need to resort to the help of
 * "arbitrary" recursive types. Recursive types are types whose defintions references 
 * themselves. This normally is only allowed when defining variants, but with -rectype
 * option this restriction can be relaxed to arbitrary type alias. This confuses dev 
 * tools and language servers however. At the cost of verboseness the feature can be
 * simulated using single-summand sums, which is denoted "Box" here. Unfortunately 
 * this makes definitions involving a lot of pieces extremely complicated.
 *)

module type SYMBOL = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

(* Type of variable namespace *)
module VarId : SYMBOL =
struct
  type t = string
  let of_string s = s
  let to_string t = t
end

(* Type of CONID namespace *)
module ConId : SYMBOL =
struct
  type t = string
  let of_string s = s
  let to_string t = t
end

(* Namespace of record fields *)
module FieldId : SYMBOL =
struct
  type t = string
  let of_string s = s
  let to_string t = t
end

module type SYNTAX = 
sig 
  type t
end

module type WRAP = 
sig 
  type 'expr t
end

module Var =
struct
  type t  = VarId.t
end

module Path =
struct
  type t =
    | Just of ConId.t
    | More of ConId.t * t
end

module QVar (Path : SYNTAX) (Var : SYNTAX) =
struct
  type t = Path.t option * Var.t
end

module Con =
struct
  type t = ConId.t
end 

module QCon (Path : SYNTAX) (Con : SYNTAX) =
struct 
  type t = Path.t option * Con.t
end

(* Sequence of modules forms a "path" *)

module Literal =
struct
  type t =
      | Int    of string
      | Float  of string
      | String of string
end

module Pattern (Var     : SYNTAX) 
                (ConRef  : SYNTAX) 
                (Literal : SYNTAX) = 
struct
  type ('var, 'lit, 'conref, 'pat) pattern = 
    | Var        of 'var
    | Any
    | Unit 
    | EmptyList
    | Literal    of 'lit
    | List       of 'conref list
    | Tuple      of 'pat list
    | Ctor       of 'conref * ('pat list)

  type t = Box of (Var.t, Literal.t, ConRef.t, t) pattern
end

module Typ = 
struct
  type ('typ) typ = 
    | Unit
    | Tuple of 'typ list

  type t = Box of t typ
end

(* Expr passed as polymorphic variable to prevent mutually recursive modules *)
module Decl (Var     : SYNTAX) 
            (Pattern : SYNTAX)
            (Typ     : SYNTAX) 
            (Wrap    : WRAP  ) =
struct
  type ('var, 'typ, 'pattern, 'expr) decl =
    | TyCon 
    (* Type annotations  *)
    | Annot of Var.t * Typ.t
    (* Pattern binding *)
    | Pat   of Pattern.t * 'expr
    (* Function binding *)
    | Fun   of (Var.t * (Pattern.t list)) * 'expr

  type 'expr t = (Var.t, Typ.t, Pattern.t, 'expr) decl
end

module Expr (Decl    : WRAP    )
            (Var     : SYNTAX  ) 
            (Field   : SYNTAX  )
            (Pattern : SYNTAX  ) 
            (Typ     : SYNTAX  )
            (ConRef  : SYNTAX  ) 
            (VarRef  : SYNTAX  )
            (Literal : SYNTAX  )
            (Wrap    : WRAP    ) =
struct

  type op_math =
    | PLUS  | MINUS
    | TIMES | DIV

  type op_cmp =
    | EQ
    | GT    | GEQ
    | LT    | LEQ

  (* Remark: 
    * We we see a non qualified variable/constructor reference in code, such as
    * 
    *    let ... in (let ... in x)
    *
    * It is not always immediately clear whether x references to a locally binded variable
    * (or a variable binded on the top level for that matter). The reason being the "exposing"
    * keyword. It's not even clear whether x is valid reference to a variable in another module
    * without consulting the content of the other module, due to directive such as
    *
    *    import M exposing(..)
    *
    * General type-checking cannot proceed until we have properly type-checked M. This limitation
    * prompts the use of `option` in the Qualified variant in two subsequent modules.
    *
    * However, it is possible to rule out the possibility of the variable being a binded variable,
    * by walking down the parser tree and track variables in context. A fix-up pass is required to 
    * establish binding relations between variables after parsing.
    *
    * Qualified (None, Id) 
    *
    * therefore is chosen as placeholder before fix-up kicks in.
    *)
  type ('decl, 'pat, 'field, 'conref, 'varref, 'literal, 'expr) expr = 
    (* Control flow constructs *)
    | Let        of 'decl list * 'expr 
    | Case       of ('expr) * (('pat * 'expr) list)
    | If         of 'expr * ('expr * 'expr)
    | Lambda     of 'pat list * 'expr
    | App        of 'expr * ('expr list) (* This is bad for type checking, may need update *)
    (* Operators *)
    | InfixMath  of op_math * 'expr * 'expr
    | InfixCmp   of op_cmp  * 'expr * 'expr
    | OpFuncMath of op_math
    | OpFuncCmp  of op_cmp
    (* Builtin Types *)
    | Unit
    | Tuple      of 'expr list  (* >= 2 elements *)
    | List       of 'expr list  (* >= 0 elements *)
    | Record     of ('field * 'expr) list
    (* Data construction *)
    | Con        of 'conref * ('expr list)
    (* Identifier refernce *)
    | Var        of 'varref
    (* Literals *)
    | Literal    of 'literal


  (* ('p, 'f, 'cr, 'v, 'vr, 'l, 't) expr' = 
  *     ((('v, 'p, 'e, 't) decl, 'p, 'f, 'cr, 'vr, 'l, 't, 'e) expr') as 'e*)
  type t = Box of (t Decl.t, Pattern.t, Field.t, ConRef.t, VarRef.t, Literal.t, t Wrap.t) expr
  
  (* Current defintion of Expr.t strongly couples its defintion to that of decl. Ideally 
  * we would like to paramertize over that as well, but doing so returns higher-kinded types.
  * This could also be achieved through functor magics, but it's highly unlikely we want to 
  * swap out definitions of decl.t without touching definition of 'expr as well. *)
end

module Module (Decl : SYNTAX) = 
struct
  type import = unit

  type module_decl = unit

  type t = unit option * (unit list) * Decl.t
end