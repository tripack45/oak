(* ElAstLexical.ml
 *
 * This file defines the AST syntax elements for the *external language*, the language that 
 * most closely resembles the surface syntax of Elm source code itself.  The EL AST is a
 * a *attribute-carrying* AST, with each AST node it also carries attributes associated with 
 * the AST.
 *
 *)

module Shared =
struct
  module Node = ElAst.Node

  module VarId   = ElAst.VarId
  module TVarId  = ElAst.TVarId
  module TyConId = ElAst.TyConId
  module DConId  = ElAst.DConId

  module MConId  = ElAst.MConId
  module FieldId = ElAst.FieldId
  module Path    = ElAst.Path
end

include Shared

(* Definitions of external language syntax elements *)
module Syntax = 
struct
  module Shared = 
  struct 
    type pos = ElAst.Syntax.pos

    type 'a node     = 'a ElAst.Syntax.node
    type 'a par_node = 'a ElAst.Syntax.par_node

    type var    = ElAst.Syntax.var
    type var'   = var node

    type tvar   = ElAst.Syntax.tvar
    type tvar'  = tvar node  (* List a => a *)

    type mcon   = ElAst.Syntax.mcon
    type mcon'  = mcon node

    type tycon  = ElAst.Syntax.tycon
    type tycon' = tycon node

    type dcon   = ElAst.Syntax.dcon
    type dcon'  = dcon node

    type field  = ElAst.Syntax.field
    type field' = field node

    type path = Path.t =
      | Just of MConId.t
      | More of MConId.t * path
    type path' = path node

    type lit = ElAst.Syntax.lit =
      | Int    of string
      | Float  of string
      | String of string
    type lit' = lit node

    type qvar = ElAst.Syntax.qvar = 
      QVar of path' option * var'
    type qvar' = qvar node

    type qtycon = ElAst.Syntax.qtycon = 
      QTyCon of path' option * tycon'
    type qtycon' = qtycon node

    type qdcon = ElAst.Syntax.qdcon = 
      QDCon of path' option * dcon'
    type qdcon' = qdcon node


    type typ' = ElAst.Syntax.typ node

    type row = ElAst.Syntax.row = 
      | RVar      of tvar'
      | Extension of row * (field' * typ') list
      | Fields    of (field' * typ') list

    type typ = ElAst.Syntax.typ =
      | TVar   of tvar'
      | Unit
      | TyCon  of qtycon'
      | Arrow  of typ' * typ'
      | TApp   of typ' * typ'
      | Record of row
      | Tuple  of typ' list

    type op = ElAst.Syntax.op =
      | APL  | APR
      | OR   | AND
      | EQU  | NE
      | GT   | GEQ
      | LT   | LEQ
      | CONS | APPEND
      | PLUS | MINUS
      | TIMES
      | FDIV | IDIV
      | POW
      | COMPOSEL
      | COMPOSER
      
    type exposing_ident = ElAst.Syntax.exposing_ident =
      | AbsTyCon of tycon'
      | TyCon    of tycon'
      | Var      of var'

    type exposing = ElAst.Syntax.exposing =
      | Any
      | Idents of exposing_ident list

    type mdecl = ElAst.Syntax.mdecl =
      | MDecl of path' * exposing option

    type import = ElAst.Syntax.import =
      | Import of path' * MConId.t node option * exposing option
  end

  include Shared

  type annot = var' * typ' 

  type pat =
    | Var        of var' * annot option
    | Any
    | Unit 
    | EmptyList
    | Literal    of lit'
    | List       of (pat node) list
    | Tuple      of (pat node) list
    | Con        of qdcon' * (pat node list)

  type pat' = pat node

  (* New forms *)
  type let_m = 
    | LetUnit
    | Let       of val_decl'      * let_m
    | LetRec    of val_decl' list * let_m
    | LetTyp    of typ_decl'      * let_m
    | LetTypRec of typ_decl' list * let_m

  and typ_decl =
    | TyCon of (tycon' * tvar' list) * ((dcon' * typ' list) list)
    | Alias of (tycon' * tvar' list) * typ'

  and typ_decl' = typ_decl node

  and val_decl =
    | Pat   of pat' * expr'
    | Fun   of ((var' * annot option) * (pat' list)) * expr'

  and val_decl' = val_decl node

  and expr = 
    | LetM       of let_m * expr'
    | Case       of expr' * ((pat' * expr') list)
    | If         of expr' * (expr' * expr')
    | Lambda     of pat' list * expr'
    | App        of expr' * (expr' list)
    (* Operators *)
    | Infix      of op * expr' * expr'
    | OpFunc     of op
    (* Builtin Types *)
    | Unit
    | Tuple      of expr' list  (* >= 2 elements *)
    | List       of expr' list  (* >= 0 elements *)
    | Record     of (field' * expr') list
    (* Data construction *)
    | Con        of qdcon' * (expr' list)
    (* Identifier refernce *)
    | Var        of qvar'
    (* Literals *)
    | Literal    of lit'

  and expr' = expr par_node

  (* The whole module *)
  type m = 
    | Mod of mdecl option * (import list) * let_m
end

(* This module copies over type definitions listed above and places them into 
 * sub module spaces. This is a tedious work as ocaml syntax forces us to reiterate
 * all cases in the definition. *)
module Alias =
struct
  open Syntax

  module Literal = 
  struct
    type lit = Syntax.lit =
      | Int    of string
      | Float  of string
      | String of string

    type lit' = Syntax.lit'
  end

  module Pattern =
  struct
    type pat = Syntax.pat =
      | Var        of var' * annot option
      | Any
      | Unit 
      | EmptyList
      | Literal    of lit'
      | List       of (pat node) list
      | Tuple      of (pat node) list
      | Con        of qdcon' * (pat node list)

    type pat' = Syntax.pat'
  end

  module LetM = 
  struct
    type typ_decl = Syntax.typ_decl =
      | TyCon of (tycon' * tvar' list) * ((dcon' * typ' list) list)
      | Alias of (tycon' * tvar' list) * typ'

    type typ_decl' = Syntax.typ_decl'

    type val_decl = Syntax.val_decl =
      | Pat   of pat' * expr'
      | Fun   of ((var' * annot option) * (pat' list)) * expr'

    type val_decl' = Syntax.val_decl'

    type let_m = Syntax.let_m =
      | LetUnit
      | Let       of val_decl'      * let_m
      | LetRec    of val_decl' list * let_m
      | LetTyp    of typ_decl'      * let_m
      | LetTypRec of typ_decl' list * let_m
  end

  module Expr =
  struct 
    type op = Syntax.op =
      | APL  | APR
      | OR   | AND
      | EQU  | NE
      | GT   | GEQ
      | LT   | LEQ
      | CONS | APPEND
      | PLUS | MINUS
      | TIMES
      | FDIV | IDIV
      | POW
      | COMPOSEL
      | COMPOSER

    type expr = Syntax.expr =
      (* Control flow constructs *)
      | LetM       of let_m  * expr'
      | Case       of expr' * ((pat' * expr') list)
      | If         of expr' * (expr' * expr')
      | Lambda     of pat' list * expr'
      | App        of expr' * (expr' list) 
      (* Operators *)
      | Infix      of op * expr' * expr'
      | OpFunc     of op
      (* Builtin Types *)
      | Unit
      | Tuple      of expr' list  (* >= 2 elements *)
      | List       of expr' list  (* >= 0 elements *)
      | Record     of (field' * expr') list
      (* Data construction *)
      | Con        of qdcon' * (expr' list)
      (* Identifier refernce *)
      | Var        of qvar'
      (* Literals *)
      | Literal    of lit'
    
    type expr' = Syntax.expr'
  end

  module Typ = 
  struct
    type typ = Syntax.typ =
      | TVar   of tvar'
      | Unit
      | TyCon  of qtycon'
      | Arrow  of typ' * typ'
      | TApp   of typ' * typ'
      | Record of row
      | Tuple  of typ' list

    type typ' = Syntax.typ'

    type row = Syntax.row =
      | RVar      of tvar'
      | Extension of row * (field' * typ') list
      | Fields    of (field' * typ') list
  end

  module Module =
  struct 
    type exposing = Syntax.exposing =
      | Any
      | Idents of exposing_ident list

    type mdecl = Syntax.mdecl =
      | MDecl of path' * exposing option

    type import = Syntax.import =
      | Import of path' * MConId.t node option * exposing option

    type m = Syntax.m =
      | Mod of mdecl option * (import list) * let_m
  end
end

module ToString =
struct 
  open Printf
  open Syntax
  open Util.Format

  module ToString = ElAst.ToString

  let pos_to_string   = ToString.pos_to_string

  let field_to_string = ToString.field_to_string

  let lit_to_string   = ToString.lit_to_string

  let var_to_string   = ToString.var_to_string
  let tvar_to_string  = ToString.tvar_to_string
  let tycon_to_string = ToString.tycon_to_string
  let dcon_to_string  = ToString.dcon_to_string
  let mcon_to_string  = ToString.mcon_to_string
  let path_to_string  = ToString.path_to_string

  let qid_to_string     = ToString.qid_to_string
  let qtycon_to_string  = ToString.qtycon_to_string
  let qdcon_to_string   = ToString.qdcon_to_string
  let qvar_to_string    = ToString.qvar_to_string

  let exposing_to_string  = ToString.exposing_to_string
  let mdecl_to_string     = ToString.mdecl_to_string
  let import_to_string    = ToString.import_to_string


  let typ_to_string       = ToString.typ_to_string
  let row_to_string       = ToString.row_to_string

  let op_to_string        = ToString.op_to_string

  let rec m_to_string (Mod (mdecl, imports, letm)) =
    let mdecl_str   = Option.map mdecl_to_string  mdecl   in
    let import_strs = List.map   import_to_string imports in 
    sprintf ("%s {%s} {%s}") 
            (Option.value mdecl_str ~default:"module")
            (String.concat ";" import_strs) 
            (letm_to_string letm)

  and letm_to_string = function
    | LetUnit -> "_"
    | Let (val_decl, letm') -> 
      sprintf "val %s;%s" (val_decl_to_string val_decl) 
                          (letm_to_string letm')
    | LetRec (val_decls, letm') ->
      let val_strs = concat_map ";" val_decl_to_string val_decls in
      sprintf "val rec {%s}%s" val_strs (letm_to_string letm')
    | LetTyp (typ_decl, letm') ->
      sprintf "type %s;%s" (typ_decl_to_string typ_decl) 
                           (letm_to_string letm')
    | LetTypRec (typ_decls, letm') ->
      let val_strs = concat_map ";" typ_decl_to_string typ_decls in
      sprintf "type rec {%s}%s" val_strs (letm_to_string letm')

  and typ_decl_to_string typ_decl =
    match Node.elem typ_decl with 
    | Alias.LetM.TyCon ((con, vars), dcons) ->
      let dcon_to_string (dcon, typs) = 
        sprintf "%s %s" (dcon_to_string dcon)
                        (concat_map " " typ_to_string typs)
      in
      sprintf "data %s %s = {%s}" 
              (tycon_to_string con)
              (concat_map " " tvar_to_string vars)
              (concat_map "|" dcon_to_string dcons)
    | Alias.LetM.Alias ((con, vars), typ) -> 
      sprintf "%s %s = %s" 
              (tycon_to_string con)
              (concat_map " " tvar_to_string vars)
              (typ_to_string typ)

  and val_decl_to_string val_decl = 
    match Node.elem val_decl with
    | Pat   (pat, e) -> 
      sprintf "%s = %s" 
                    (pat_to_string pat) 
                    (expr_to_string e)
    | Fun   (((var, annot_opt), pats), e) ->
      match annot_opt with 
      | None -> 
        sprintf "fun %s %s = %s" 
                      (var_to_string var) 
                      (concat_map " " pat_to_string pats) 
                      (expr_to_string e)
      | Some (_, typ) ->
        sprintf "fun (%s : %s) %s = %s" 
                      (var_to_string var) 
                      (typ_to_string typ)
                      (concat_map " " pat_to_string pats) 
                      (expr_to_string e)
  and pat_to_string pat = 
    match Node.elem pat with
    | Var (var, annot) -> (
        match annot with 
        | Some (_, typ) -> var_to_string var ^ " : " ^ typ_to_string typ
        | None          -> var_to_string var
      )
    | Any          -> "_"
    | Unit         -> "()"
    | EmptyList    -> "[]"
    | Literal lit  -> lit_to_string lit
    | List pats   -> surround ("[", "]") @@ concat_map ", " pat_to_string pats
    | Tuple pats  -> surround ("(", ")") @@ concat_map ", " pat_to_string pats
    | Con (qdcon, pats) -> 
      match pats with 
      | [] -> qdcon_to_string qdcon
      | _ -> sprintf "%s of %s" (qdcon_to_string qdcon)  (concat_map " " pat_to_string pats)

  and expr_to_string e =
    let pp = expr_to_string in
    let expr_str =
      match Node.elem e with
      | LetM (letm, e)     -> sprintf "letM {%s} in %s" (letm_to_string letm) (pp e) 
      | Case (e, branches) -> 
        let arrow (p, e) = pat_to_string p ^ " -> " ^ pp e in
        sprintf "case %s of {%s}" (pp e) (concat_map "|" arrow branches)
      | If (e1, (e2, e3))  -> sprintf "if %s then {%s} else {%s}" (pp e1) (pp e2) (pp e3)
      | Lambda (pats, e)   -> sprintf "fn %s = {%s}" (concat_map " " pat_to_string pats) (pp e)
      | App (e, es)        -> concat_map " " pp (e::es)
      | Infix (op, e1, e2) -> sprintf "%s %s %s" (pp e1) (op_to_string op) (pp e2)
      | OpFunc op          -> surround ("(", ")") (op_to_string op)
      | Unit               -> "()"
      | Tuple es           -> surround ("(", ")") @@ concat_map ", " pp es
      | List es            -> surround ("[", "]") @@ concat_map ", " pp es
      | Record field_es    -> 
        let record_field (f, e) = field_to_string f ^ " = " ^ pp e in
        surround ("<{", "}>") @@ (concat_map ";" record_field field_es)
      | Con (qdcon, es)    -> qdcon_to_string qdcon ^ concat_map " " pp es
      | Var qvar           -> qvar_to_string qvar
      | Literal lit        -> lit_to_string lit
    in 
    match snd (Node.attr e) with
    | Naked -> expr_str
    | Wrapped _ -> surround ("(", ")") expr_str
end
