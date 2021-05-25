(* ElAst.ml
 *
 * This file defines the AST syntax elements for the *external language*, the language that 
 * most closely resembles the surface syntax of Elm source code itself.  The EL AST is a
 * a *attribute-carrying* AST, with each AST node it also carries attributes associated with 
 * the AST.
 *
 * The attributes contains
 * 
 * - For all nodes, Source positional info
 * - For expressions, whether the expression is wrapped in parentheses
 *
 * This file is organized as follows:
 *
 * - module Node defines an attribute carrying AST ('elem, 'attr) node
 * - module VarId / ConId / FieldId astracts the identifier for corresponding identifier
 * - module Syntax defines a series of types using aforementioned modules and types
 *   that represents the actual shape of the AST. The top-level type being "m"
 * - module Alias serves no semantic purpose other than organizing and re-exporting elements
 *   of Syntax module into separate modules. This is mainly to support the generated parser.
 * - module ToString defines a series of functions that formats various AST elements.
 *)

module Node : 
sig
  type ('a, 'b) t
  val node : 'a -> 'b -> ('a, 'b) t
  val elem : ('a, 'b) t -> 'a
  val attr : ('a, 'b) t -> 'b
  val both : ('a, 'b) t -> 'a * 'b
  val map_elem  : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val map_attr  : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end = 
struct
  type ('a, 'b) t = 'a * 'b
  let node elem attr = (elem, attr) 
  let elem (elem, _) = elem 
  let attr (_, attr) = attr
  let both node = node
  let map_elem f (elem, attr) = (f elem, attr)
  let map_attr f (elem, attr) = (elem, f attr)
end 

module type IDENT = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

module Ident () =
struct 
  type t = string
  let of_string s = s
  let to_string t = t
  let compare = String.compare
end

module VarId : IDENT = Ident ()

module ConId : IDENT = Ident ()

module FieldId : IDENT = Ident ()

(* Definitions of external language syntax elements *)
module Syntax = 
struct
  type pos = Lexing.position * Lexing.position

  (* A regular AST node encodes source file location *)
  type 'a node     = ('a, pos) Node.t

  (* Special nodes for expressions and patterns also encodes 
  * parentheses depths in addition to source locations *)
  type paren =
    | Wrapped of int   (* counts number of parens wrapped *)
    | Naked

  type 'a par_node = ('a, pos * paren) Node.t

  type var = VarId.t
  type var' = var node

  type con = ConId.t
  type con' = con node

  type field = FieldId.t
  type field' = field node

  type lit = 
    | Int    of string
    | Float  of string
    | String of string
  type lit' = lit node

  type path =
    | Just of ConId.t
    | More of ConId.t * path
  type path' = path node

  type qvar = QVar of path' option * var'
  type qvar' = qvar node

  type qcon = QCon of path' option * con'
  type qcon' = qcon node

  type pat = 
    | Var        of var'
    | Any
    | Unit 
    | EmptyList
    | Literal    of lit'
    | List       of (pat node) list
    | Tuple      of (pat node) list
    | Ctor       of qcon' * (pat node list)
  type pat' = pat node

  type typ = 
    | Unit
    | Tuple of typ node list
  and typ' = typ node

  type op =
    | PLUS  | MINUS
    | TIMES | DIV
    | EQ
    | GT    | GEQ
    | LT    | LEQ

  (* decl depends on the yet provided definition of type expr *)
  type decl =
    | TyCon 
    | Annot of var' * typ'
    | Pat   of pat' * expr'
    | Fun   of (var' * (pat' list)) * expr'

  and expr = 
    (* Control flow constructs *)
    | Let        of decl' list * expr'
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
    | Con        of qcon' * (expr' list)
    (* Identifier refernce *)
    | Var        of qvar'
    (* Literals *)
    | Literal    of lit'

  and decl' = decl node
  and expr' = expr par_node

  type exposing_ident =
    | AbsTyCon of con'
    | TyCon    of con'
    | Var      of var'

  type exposing =
    | Any
    | Idents of exposing_ident list

  type mdecl = 
    | MDecl of con' * exposing option

  type import = 
    | Import of path' * con' option * exposing option

  (* The whole module *)
  type m = 
    | Mod of mdecl option * (import list) * (decl' list)
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
      | Var        of var'
      | Any
      | Unit 
      | EmptyList
      | Literal    of lit'
      | List       of pat' list
      | Tuple      of pat' list
      | Ctor       of qcon' * (pat' list)

    type pat' = Syntax.pat'
  end

  module Expr =
  struct 
    type op = Syntax.op =
      | PLUS  | MINUS
      | TIMES | DIV
      | EQ
      | GT    | GEQ
      | LT    | LEQ

    type expr = Syntax.expr =
      (* Control flow constructs *)
      | Let        of decl' list * expr'
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
      | Con        of qcon' * (expr' list)
      (* Identifier refernce *)
      | Var        of qvar'
      (* Literals *)
      | Literal    of lit'
    
    type expr' = Syntax.expr'
  end

  module Typ = 
  struct
    type typ = Syntax.typ =
      | Unit
      | Tuple of typ' list

    and typ' = Syntax.typ'
  end

  module Decl = 
  struct
    type decl = Syntax.decl = 
      | TyCon 
      | Annot of var' * typ'
      | Pat   of pat' * expr'
      | Fun   of (var' * (pat' list)) * expr'

    type decl' = Syntax.decl'
  end

  module Module =
  struct 
    type exposing = Syntax.exposing =
      | Any
      | Idents of exposing_ident list

    type mdecl = Syntax.mdecl =
      | MDecl of con' * exposing option

    type import = Syntax.import =
      | Import of path' * con' option * exposing option

    type m = Syntax.m =
      | Mod of mdecl option * (import list) * (decl' list)
  end
end

module ToString =
struct 
  open Printf
  open Syntax
  open Util.Format

  let pos_to_string (s, e) = 
    let pos2lc (s : Lexing.position) = 
      let line_num = s.pos_lnum in
      let col = s.pos_cnum - s.pos_bol in
      (line_num, col)
    in 
    let (l, c)   = pos2lc s in
    let (l', c') = pos2lc e in
    Printf.sprintf "%d:%d - %d:%d" l c l' c'

  let rec m_to_string (Mod (mdecl, imports, decls)) =
    let mdecl_str   = Option.map mdecl_to_string  mdecl   in
    let import_strs = List.map   import_to_string imports in 
    let decl_strs   = List.map   decl_to_string   decls   in
    sprintf ("%s {%s}") 
            (Option.value mdecl_str ~default:"module")
            (String.concat ";" (import_strs @ decl_strs))
  and exposing_to_string exposing = 
    let id_to_string (id : exposing_ident) =
      match id with
      | Var var    -> var_to_string var
      | TyCon con  -> con_to_string con ^ "(..)"
      | AbsTyCon con -> con_to_string con
    in
    match exposing with 
    | Any -> "(..)"
    | Idents ids -> surround ("(", ")") @@ concat_map ", " id_to_string ids 
    
  and mdecl_to_string (MDecl (con, exposing_opt)) = 
    match exposing_opt with 
    | Some exposing -> 
      sprintf "module %s exposing %s" (con_to_string con) (exposing_to_string exposing)
    | None -> 
      sprintf "module %s" (con_to_string con)

  and import_to_string (Import (path, as_opt, exposing_opt)) = 
    let exposing_str = 
      match Option.map exposing_to_string exposing_opt with 
      | Some s -> " exposing " ^ s 
      | None -> ""
    in
    let as_str = 
      match Option.map con_to_string as_opt with
      | Some s -> " as " ^ s
      | None -> ""
    in 
    (sprintf "import %s%s%s" (path_to_string path) exposing_str as_str)

  and decl_to_string decl = 
    match (Node.elem decl) with
    | TyCon -> "TyCon"
    | Annot (var, t) -> var_to_string var ^ " :: " ^ typ_to_string t
    | Pat   (pat, e) -> 
      sprintf "val %s = %s" 
                    (pat_to_string pat) 
                    (expr_to_string e)
    | Fun   ((var, pats), e) ->
      sprintf "fun %s %s = %s" 
                    (var_to_string var) 
                    (concat_map " " pat_to_string pats) 
                    (expr_to_string e)

  and pat_to_string pat = 
    match Node.elem pat with
    | Var var      -> var_to_string var
    | Any          -> "_"
    | Unit         -> "()"
    | EmptyList    -> "[]"
    | Literal lit  -> lit_to_string lit
    | List pats   -> surround ("[", "]") @@ concat_map ", " pat_to_string pats
    | Tuple pats  -> surround ("(", ")") @@ concat_map ", " pat_to_string pats
    | Ctor (qcon, pats) -> 
      match pats with 
      | [] -> qcon_to_string qcon
      | _ -> sprintf "%s of %s" (qcon_to_string qcon)  (concat_map " " pat_to_string pats)

  and op_to_string = function 
    | PLUS  -> "+"
    | MINUS -> "-"
    | TIMES -> "*"
    | DIV   -> "/"
    | EQ    -> "="
    | GT    -> ">"
    | GEQ   -> ">="
    | LT    -> "<"
    | LEQ   -> "<"

  and expr_to_string e =
    let pp = expr_to_string in
    let expr_str =
      match Node.elem e with
      | Let (decls, e)     -> 
        let decl_strs = "{" ^ concat_map "; " decl_to_string decls ^ "}" in
        sprintf "let %s in %s" decl_strs (pp e) 
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
      | Con (qcon, es)     -> qcon_to_string qcon ^ concat_map " " pp es
      | Var qvar           -> qvar_to_string qvar
      | Literal lit        -> lit_to_string lit
    in 
    match snd (Node.attr e) with
    | Naked -> expr_str
    | Wrapped n -> surround ("(", ")") expr_str
    

  and typ_to_string typ = 
    match Node.elem typ with 
    | Unit -> "unit"
    | Tuple typs -> surround ("(", ")") @@ concat_map ", " typ_to_string typs

  and qcon_to_string qcon =
    let QCon (path, con) = Node.elem qcon in
    match Option.map path_to_string path with 
    | Some path_str -> path_str ^ "." ^ con_to_string con
    | None -> con_to_string con

  and qvar_to_string qvar = 
    let QVar (path, var) = Node.elem qvar in
    match Option.map path_to_string path with 
    | Some path_str -> path_str ^ "." ^ var_to_string var
    | None -> var_to_string var

  and path_to_string path : string = 
    let rec to_string p = 
      match p with 
      | Just con -> ConId.to_string con
      | More (con, p) -> ConId.to_string con ^ "." ^ to_string p
    in to_string (Node.elem path) 

  and field_to_string field = 
    FieldId.to_string (Node.elem field)

  and lit_to_string lit = 
    match Node.elem lit with
    | Int s    -> s
    | Float s  -> s
    | String s -> surround ("\"", "\"") s

  and var_to_string var = 
    VarId.to_string (Node.elem var)

  and con_to_string con = 
    ConId.to_string (Node.elem con)
end