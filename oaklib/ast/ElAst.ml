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
  val fold_elem  : ('a -> 'c) -> ('a, 'b) t -> 'c
  val fold : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c

  val fold2 : f:('a -> 'b -> 'c) ->  ('a, 'c) t -> ('b, 'd) t -> 'c

  val compare      : ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'c) t -> int
  val eq           : ('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
  val eq_of_compare: ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'c) t -> bool
end = 
struct
  type ('a, 'b) t = 'a * 'b
  let node elem attr = (elem, attr) 
  let elem (elem, _) = elem 
  let attr (_, attr) = attr
  let both node = node
  let map_elem f (elem, attr) = (f elem, attr)
  let map_attr f (elem, attr) = (elem, f attr)
  let fold_elem f (e, _attr) = f e 
  let fold f (e, attr) = f e attr

  let fold2 ~f x y = f (elem x) (elem y)
  let compare f = fold2 ~f
  let eq = compare
  let eq_of_compare f x y = (compare f x y = 0)
end 

module type IDENT = sig
  type t 
  [@@deriving compare, sexp]
  val of_string : string -> t
  val to_string : t -> string

  include Core.Comparator.S with type t := t

  module Map : Core.Map.S with type Key.t = t
end

module Ident () =
struct 
  include Core.String 
end

module VarId : IDENT = Ident ()
module TVarId : IDENT = Ident ()
module MConId : IDENT = Ident () (* Path segement *)
module TyConId : IDENT = Ident ()
module DConId : IDENT = Ident ()
module FieldId : IDENT = Ident ()

module Path :
sig
  type t =
    | Just of MConId.t
    | More of MConId.t * t
  [@@deriving compare, sexp]

  val to_string : t -> string

  include Core.Comparable.S_plain with type t := t

  module Map : Core.Map.S with type Key.t = t
end = struct
  module T = 
  struct 
    module T_ = 
    struct 
      type t =
        | Just of MConId.t
        | More of MConId.t * t
      [@@deriving compare, sexp]

      let rec to_string = function 
        | Just id -> MConId.to_string id
        | More (id, path) -> MConId.to_string id ^ "." ^ to_string path
    end
    include T_
    include Core.Comparable.Make_plain(T_)
  end
  
  include T
  module Comparable = T
  module Map = Core.Map.Make(T)
end

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

  type var = VarId.t      (* func => func *)
  type var' = var node

  type tvar = TVarId.t
  type tvar' = tvar node  (* List a => a *)

  type mcon = MConId.t    (* module X => X *)
  type mcon' = mcon node

  type tycon = TyConId.t  (* type [alias] X => X *)
  type tycon' = tycon node

  type dcon = DConId.t    (* type X = A => A *)
  type dcon' = dcon node

  type field = FieldId.t  (* { field = 1 } => field *)
  type field' = field node

  type lit = 
    | Int    of string
    | Float  of string
    | String of string
  type lit' = lit node

  type path = Path.t =
    | Just of MConId.t
    | More of MConId.t * path
  type path' = path node

  (* "q" for "qualified", meaning "could be prefixed by Module.Path." *)

  type qvar = QVar of path' option * var'
  type qvar' = qvar node

  type qtycon = QTyCon of path' option * tycon'
  type qtycon' = qtycon node

  type qdcon = QDCon of path' option * dcon'
  type qdcon' = qdcon node

  type pat = 
    | Var        of var'
    | Any
    | Unit 
    | EmptyList
    | Literal    of lit'
    | Cons       of (pat node) * (pat node)
    | List       of (pat node) list
    | Tuple      of (pat node) list
    | Con        of qdcon' * (pat node list)
  type pat' = pat node

  type typ = 
    | TVar   of tvar'
    | Unit
    | TyCon  of qtycon'
    | Arrow  of typ' * typ'
    | TApp   of typ' * typ'
    | Record of row
    | Tuple  of typ' list

  (* This syntax affords much more flexibility compared to Elm *)
  and row = 
    | RVar      of tvar'
    | Extension of row * (field' * typ') list
    | Fields    of (field' * typ') list

  and typ' = typ node

  type op =
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

  (* decl depends on the yet provided definition of type expr  *)
  (* We tvar' for both typ variables and row variables
   *
   * Elm compiler mixes up type variables and row variables so that it's type system 
   * is inconsistent, and type checking crashes the compiler in some cases. Lack of 
   * syntatic distinctin between row variables and type variables creates a problem for 
   * us as we now need to disambiguous and fix them in a later phase. This also means
   * we WILL reject (incorrect) Elm code that is accepted by the Elm compiler. This is
   * one of the few cases that restricting the use of language makes more sense then not. 
   *
   * The difference of typ and row variable is not one of syntatic but one of semantic, 
   * that is, type and row variables both fall into the syntax category of "type variable", 
   * but they belongs to a different "kind" upon type checking. 
   *
   * Port is an annotation of ported js function. Since no function definition is attached
   * to a port declaration, it's treated as a unique decl.
   *)
  type decl =
    | TyCon of (tycon' * tvar' list) * ((dcon' * typ' list) list)
    | Alias of (tycon' * tvar' list) * typ'
    | Annot of var' * typ'
    (* Port annotation *)
    | Port  of var' * typ' 
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
    | Con        of qdcon' * (expr' list)
    (* Identifier refernce *)
    | Var        of qvar'
    (* Literals *)
    | Literal    of lit'

  and decl' = decl node
  and expr' = expr par_node

  type exposing_ident =
    | AbsTyCon of tycon'
    | TyCon    of tycon'
    | Var      of var'

  type exposing =
    | Any
    | Idents of exposing_ident list

  type mdecl = 
    | MDecl of MConId.t node * exposing option

  type import = 
    | Import of path' * MConId.t node option * exposing option

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
      | Cons       of pat' * pat'
      | List       of pat' list
      | Tuple      of pat' list
      | Con        of qdcon' * (pat' list)

    type pat' = Syntax.pat'
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

  module Decl = 
  struct
    type decl = Syntax.decl = 
      | TyCon of (tycon' * tvar' list) * ((dcon' * typ' list) list)
      | Alias of (tycon' * tvar' list) * typ'
      | Annot of var' * typ'
      | Port of var' * typ'
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
      | MDecl of MConId.t node * exposing option

    type import = Syntax.import =
      | Import of path' * MConId.t node option * exposing option

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

  let field_to_string field = 
    FieldId.to_string (Node.elem field)

  let lit_to_string lit = 
    match Node.elem lit with
    | Int s    -> s
    | Float s  -> s
    | String s -> surround ("\"", "\"") s

  let var_to_string v   = Node.fold_elem VarId.to_string v
  let tvar_to_string tv = Node.fold_elem TVarId.to_string tv
  let tycon_to_string t = Node.fold_elem TyConId.to_string t
  let dcon_to_string d  = Node.fold_elem DConId.to_string d
  let mcon_to_string m  = Node.fold_elem MConId.to_string m

  let path_to_string path : string = 
    let rec to_string p = 
      match p with 
      | Just con -> MConId.to_string con
      | More (con, p) -> MConId.to_string con ^ "." ^ to_string p
    in to_string (Node.elem path) 

  let qid_to_string ~extract ~to_string qid =
    let (path, id) = extract @@ Node.elem qid in
    match Option.map path_to_string path with 
    | Some path_str -> path_str ^ "." ^ to_string id 
    | None -> to_string id

  let qtycon_to_string qtycon =
    qid_to_string ~extract:(fun (QTyCon (p_opt, tycon)) -> (p_opt, tycon))
                  ~to_string:(Node.fold_elem TyConId.to_string)
                  qtycon

  let qdcon_to_string qdcon =
    qid_to_string ~extract:(fun (QDCon (p_opt, tycon)) -> (p_opt, tycon))
                  ~to_string:(Node.fold_elem DConId.to_string)
                  qdcon

  let qvar_to_string qvar =
    qid_to_string ~extract:(fun (QVar (p_opt, tycon)) -> (p_opt, tycon))
                  ~to_string:(Node.fold_elem VarId.to_string)
                  qvar

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
      | TyCon con  -> tycon_to_string con ^ "(..)"
      | AbsTyCon con -> tycon_to_string con
    in
    match exposing with 
    | Any -> "(..)"
    | Idents ids -> surround ("(", ")") @@ concat_map ", " id_to_string ids 
    
  and mdecl_to_string (MDecl (con, exposing_opt)) = 
    match exposing_opt with 
    | Some exposing -> 
      sprintf "module %s exposing %s" (mcon_to_string con) (exposing_to_string exposing)
    | None -> 
      sprintf "module %s" (mcon_to_string con)

  and import_to_string (Import (path, as_opt, exposing_opt)) = 
    let exposing_str = 
      match Option.map exposing_to_string exposing_opt with 
      | Some s -> " exposing " ^ s 
      | None -> ""
    in
    let as_str = 
      match Option.map mcon_to_string as_opt with
      | Some s -> " as " ^ s
      | None -> ""
    in 
    (sprintf "import %s%s%s" (path_to_string path) exposing_str as_str)

  and decl_to_string decl = 
    match (Node.elem decl) with
    | TyCon ((con, vars), dcons) ->
      let dcon_to_string (dcon, typs) = 
        sprintf "%s %s" (dcon_to_string dcon)
                        (concat_map " " typ_to_string typs)
      in
      sprintf "datatype %s %s = {%s}" 
              (tycon_to_string con)
              (concat_map " " tvar_to_string vars)
              (concat_map "|" dcon_to_string dcons)
    | Alias ((con, vars), typ) -> 
      sprintf "type %s %s = %s" 
              (tycon_to_string con)
              (concat_map " " tvar_to_string vars)
              (typ_to_string typ)
    | Annot (var, t) -> "annot " ^ var_to_string var ^ " :: " ^ typ_to_string t
    | Port  (var, t) -> "port " ^ var_to_string var ^ " :: " ^ typ_to_string t
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
    | Var var       -> var_to_string var
    | Any           -> "_"
    | Unit          -> "()"
    | EmptyList     -> "[]"
    | Literal lit   -> lit_to_string lit
    | Cons (p1, p2) -> pat_to_string p1 ^ " :: " ^ pat_to_string p2
    | List pats     -> surround ("[", "]") @@ concat_map ", " pat_to_string pats
    | Tuple pats    -> surround ("(", ")") @@ concat_map ", " pat_to_string pats
    | Con (qdcon, pats) -> 
      match pats with 
      | [] -> qdcon_to_string qdcon
      | _ -> sprintf "%s of %s" (qdcon_to_string qdcon)  (concat_map " " pat_to_string pats)

  and op_to_string = function 
    | APL     -> "<@"
    | APR     -> "@>"
    | OR      -> "or"
    | AND     -> "and"
    | EQU     -> "=="
    | NE      -> "/="
    | GT      -> ">"
    | LT      -> "<"
    | GEQ     -> ">="
    | LEQ     -> "<="
    | CONS    -> "::"
    | APPEND  -> "++"
    (* Arithematics *)
    | PLUS     -> "+"
    | MINUS    -> "-"
    | TIMES    -> "*"
    | FDIV     -> "/"
    | IDIV     -> "//"
    | POW      -> "^"
    | COMPOSEL -> "<<"
    | COMPOSER -> ">>"

  and expr_to_string e =
    let pp = expr_to_string in
    let expr_str =
      match Node.elem e with
      | Let (decls, e)     -> 
        let decl_strs = "{" ^ concat_map ";" decl_to_string decls ^ "}" in
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
      | Con (qdcon, es)    -> qdcon_to_string qdcon ^ concat_map " " pp es
      | Var qvar           -> qvar_to_string qvar
      | Literal lit        -> lit_to_string lit
    in 
    match snd (Node.attr e) with
    | Naked -> expr_str
    | Wrapped _ -> surround ("(", ")") expr_str
    

  and typ_to_string typ = 
    match Node.elem typ with 
    | TVar tvar -> Node.fold_elem TVarId.to_string tvar
    | Unit -> "()"
    | TyCon c -> qtycon_to_string c
    | Arrow (t1, t2) -> typ_to_string t1 ^ " -> " ^ typ_to_string t2
    | TApp (t1, t2) -> typ_to_string t1 ^ " " ^ typ_to_string t2
    | Tuple typs -> surround ("(", ")") @@ concat_map ", " typ_to_string typs
    | Record row -> surround ("<", ">") @@ row_to_string row

  and row_to_string row =
    let field_to_string (field, typ) =
       field_to_string field ^ ": " ^ typ_to_string typ
    in
    match row with 
    | RVar rvar -> Node.fold_elem TVarId.to_string rvar
    | Extension (r, fields) ->
      sprintf "%s & %s" (row_to_string r) (concat_map ", " field_to_string fields)
    | Fields fields -> concat_map ", " field_to_string fields
end