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


(* It is important to different, in the AST nodes that refers to a variable 
  * purely for it's value, or a place of binding. For instance in 
  * 
  *   let x = M.p in x + N.q 
  *
  * - x in the binder place must not qualified as it represents a place of binding, 
  *   therefore must not be qualified
  * - x in x + N.q represents essentially a referce to the binding site
  * - N.q and M.p represents "free" values not binded anywhere
  *
  * Essentially it's important to differiate variables (things that can afford 
  * alpha-variance) to "free" variables, such as the qualified variants
  * 
  * Although a similar argument can be made for constructors, the argument is much 
  * weaker as one cannot "bind" a constructor. That is, no syntax for somethign like 
  *
  *   let ConY = ConX in (ConY 1)
  * 
  * Still, one can consider coming up a tool that alpha varies all constructors defined
  * in this current module.
  *)
module Var = VarId

module Con = ConId

(* Sequence of modules forms a "path" *)
module Path =
struct
  type t =
    | Just of ConId.t
    | More of ConId.t * t
end

module Literal =
struct
  type t =
      | Int    of string
      | Float  of string
      | String of string
end

module QVar =
struct
  type ('path, 'var) t = 
    | QVar of 'path option * 'var

  let fold ~fpath ~fvar (QVar (path, var)) =
      QVar (Option.map fpath path, fvar var)
end

module QCon =
struct 
  type ('path, 'con) t = 
    | QCon of 'path option * 'con

  let fold ~fpath ~fcon (QCon (path, con)) =
      QCon (Option.map fpath path, fcon con)
end

module Pattern = 
struct
  type ('var, 'lit, 'qcon, 'pat) pattern = 
    | Var        of 'var
    | Any
    | Unit 
    | EmptyList
    | Literal    of 'lit
    | List       of 'pat list
    | Tuple      of 'pat list
    | Ctor       of 'qcon * ('pat list)

  let fold ~fvar ~flit ~fqcon ~fpat pat = 
    match pat with 
    | Var v             -> Var     (fvar v)
    | Any               -> Any
    | Unit              -> Unit
    | EmptyList         -> EmptyList
    | Literal l         -> Literal (flit l)
    | List pats         -> List    (List.map fpat pats)
    | Tuple pats        -> Tuple   (List.map fpat pats)
    | Ctor (qcon, pats) -> Ctor    (fqcon qcon, List.map fpat pats)

end

module Typ = 
struct
  type ('typ) typ = 
    | Unit
    | Tuple of 'typ list

  let fold ~ftyp typ =
    match typ with 
    | Unit             -> Unit
    | Tuple typs       -> Tuple (List.map ftyp typs)
end

module Decl =
struct
  type ('var, 'pat, 'expr, 'typ) decl =
    | TyCon 
    (* Type annotations  *)
    | Annot of 'var * 'typ
    (* Pattern binding *)
    | Pat   of 'pat * 'expr
    (* Function binding *)
    | Fun   of ('var * ('pat list)) * 'expr

  let fold ~fvar ~fpat ~fexpr ~ftyp decl =
    match decl with 
    | TyCon                   -> TyCon
    | Annot (var, typ)        -> Annot (fvar var, ftyp typ) 
    | Pat (pat, expr)         -> Pat (fpat pat, fexpr expr)
    | Fun ((var, pats), expr) -> Fun ((fvar var, List.map fpat pats), fexpr expr)
end

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
module Expr =
struct
  type op =
    | PLUS  | MINUS
    | TIMES | DIV
    | EQ
    | GT    | GEQ
    | LT    | LEQ

  type ('decl, 'pat, 'field, 'qcon, 'qvar, 'literal, 'expr) expr = 
    (* Control flow constructs *)
    | Let        of 'decl list * 'expr 
    | Case       of 'expr * (('pat * 'expr) list)
    | If         of 'expr * ('expr * 'expr)
    | Lambda     of 'pat list * 'expr
    | App        of 'expr * ('expr list) (* This is bad for type checking, may need update *)
    (* Operators *)
    | Infix      of op * 'expr * 'expr
    | OpFunc     of op
    (* Builtin Types *)
    | Unit
    | Tuple      of 'expr list  (* >= 2 elements *)
    | List       of 'expr list  (* >= 0 elements *)
    | Record     of ('field * 'expr) list
    (* Data construction *)
    | Con        of 'qcon * ('expr list)
    (* Identifier refernce *)
    | Var        of 'qvar
    (* Literals *)
    | Literal    of 'literal

  
  let fold ~fdecl ~fpat ~ffield ~fqcon ~fqvar ~flit ~fexpr expr =
    let map2 f g (x, y) = (f x, g y) in
    match expr with 
    | Let (decls, e)     -> Let (List.map fdecl decls, fexpr e)
    | Case (e, branches) -> Case (fexpr e, List.map (map2 fpat fexpr) branches)
    | If (e1, (e2, e3))  -> If (fexpr e1, (fexpr e2, fexpr e3))
    | Lambda (pats, e)   -> Lambda (List.map fpat pats, fexpr e)
    | App (e, es)        -> App (fexpr e, List.map fexpr es)
    | Infix (op, e1, e2) -> Infix (op, fexpr e1, fexpr e2)
    | OpFunc op          -> OpFunc op
    | Unit               -> Unit
    | Tuple es           -> Tuple (List.map fexpr es)
    | List es            -> List (List.map fexpr es)
    | Record field_es    -> Record (List.map (map2 ffield fexpr) field_es)
    | Con (qc, es)       -> Con (fqcon qc, List.map fexpr es)
    | Var qv             -> Var (fqvar qv)
    | Literal l          -> Literal (flit l)

end


module Module = 
struct

  type ('con, 'var) exposing =
    | Any
    | Identifiers of ('con, 'var) ident list
  
  and ('con, 'var) ident =
    | Con of 'con
    | Var of 'var

  module Decl =
  struct
    type ('con, 'var) decl = 
      | Decl of 'con * ('con, 'var) exposing

    let fold ~fcon ~fvar (Decl (con, exposing)) =
      let map_id id =
        match id with
        | Con c -> Con (fcon c)
        | Var v -> Var (fvar v)
      in 
      let exposing' = 
        match exposing with
        | Any -> Any
        | Identifiers ids -> Identifiers (List.map map_id ids)
      in Decl (fcon con, exposing')
  end

  module Import =
  struct 
    type ('qcon, 'con, 'var) import = 
      | Import of 'qcon * 'con option * ('con, 'var) exposing option

    let fold ~fqcon ~fcon ~fvar (Import (qcon, c_opt, exposing)) =
      let map_id id =
        match id with
        | Con c -> Con (fcon c)
        | Var v -> Var (fvar v)
      in 
      let exposing' =
        match exposing with
        | None -> None
        | Some Any -> Some Any
        | Some (Identifiers ids) -> Some (Identifiers (List.map map_id ids))
    in Import (fqcon qcon, Option.map fcon c_opt, exposing')
  end

  type ('mdecl, 'imp, 'decl) m = 
      'mdecl option * ('imp list) * ('decl list)

  let fold ~fmdecl ~fimp ~fdecl (mdecl, imps, decls) =
    let rmod   = Option.map fmdecl mdecl in
    let rimps  = List.map fimp imps in
    let rdecls = List.map fdecl decls in 
    (rmod, rimps, rdecls)
end
