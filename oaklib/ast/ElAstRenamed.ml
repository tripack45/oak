(* Resolved ElAst - See PhaseResolveSymbols.ml
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

(* Type constructors and data constructors, though following the same syntax 
 * are in their own independent namespaces, therefore needs to be tracked separately. 
 * For example, the following code is valid Elm code:
 *     (repl)
 *     > type T = S () | T Int
 *     > T 1
 *     T 1 : T
 *)
module Var   = ElAstResolved.Var
module TVar  = ElAstResolved.TVar
module DCon  = ElAstResolved.DCon
module TyCon = ElAstResolved.TyCon

module Syntax =
struct

  module Shared = 
  struct 
    type pos = ElAstLexical.Syntax.pos

    type 'a node = 'a ElAstLexical.Syntax.node

    type var    = ElAstLexical.Syntax.var
    type var'   = var node

    type tvar   = ElAstLexical.Syntax.tvar
    type tvar'  = tvar node  (* List a => a *)

    type mcon   = ElAstLexical.Syntax.mcon
    type mcon'  = mcon node

    type tycon  = ElAstLexical.Syntax.tycon
    type tycon' = tycon node

    type dcon   = ElAstLexical.Syntax.dcon
    type dcon'  = dcon node

    type field  = ElAstLexical.Syntax.field
    type field' = field node

    type path = Path.t =
      | Just of MConId.t
      | More of MConId.t * path

    type lit = ElAst.Syntax.lit =
      | Int    of string
      | Float  of string
      | Char   of string
      | String of string
    type lit' = lit node

    type op = ElAstLexical.Syntax.op =
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
  end

  include Shared

  (* Path could be filled in by the compiler therefore does not have a source location *)
  type path'   = (path, pos option) Node.t

  (* Identifiers involves a few similar but different concepts.
   * 
   * - *id types represents the type of the identifier name, i.e, an abstracted 
   *   notion of string. Using this abstract notion allows us to introduce checks 
   *   on name validity, and prevents assigning names of tcons to dcons for example.contents
   *
   * - var/dcon/tycon represent a value/data ctor/type ctor variable. They are variables in the 
   *   mathematical/type theoretic sense, serving as references to their point of definitions. 
   *   The introduction of this type gives a fresh name to each definition, resolving all shadowing.
   *
   * - *ref types represents references this to variable/dcon/tycon-like things.
   *)
  type ('binder, 'id) twin = 'binder * 'id
  type ('binder, 'id) twin' =  ('binder, 'id) twin node
  type ('binder, 'id) ident_ref = 
      | Bind of ('binder, 'id) twin'
      | Free of path' * 'id node
      | Unresolved of 'id node
  type ('binder, 'id) ident_ref' = ('binder, 'id) ident_ref node

  type varid  = VarId.t
  type tvarid = TVarId.t
  type dconid = DConId.t
  type tyconid = TyConId.t

  type varid'  = varid node
  type tvarid' = tvarid node
  type dconid' = dconid node
  type tyconid' = tyconid node

  type var   = (Var.t, VarId.t) twin 
  type tvar  = (TVar.t, TVarId.t) twin 
  type dcon  = (DCon.t, DConId.t) twin
  type tycon = (TyCon.t, TyConId.t) twin

  type var'   = var node 
  type tvar'  = tvar node 
  type dcon'  = dcon node
  type tycon' = tycon node

  type varref   = (Var.t, VarId.t) ident_ref
  type dconref  = (DCon.t, DConId.t) ident_ref
  type tyconref = (TyCon.t, TyConId.t) ident_ref

  type varref'   = varref node
  type dconref'  = dconref node
  type tyconref' = tyconref node

  (* Row variables are just type variables of "row" kind 
   * Similar to int variables are just variables of int type *)
  type rvar' = tvar'

  type typ = 
    | TVar   of tvar'
    | Unit
    | TyCon  of tyconref'
    | Arrow  of typ' * typ'
    | TApp   of typ' * typ'
    | Record of row
    | Tuple  of typ' list

  and row = 
    | RVar      of rvar'
    | Extension of row * (field' * typ') list
    | Fields    of (field' * typ') list

  and typ' = typ node

  type annot = var' * typ' 

  type pat = 
    | Var        of var' * (annot option)
    | Any
    | Unit 
    | EmptyList
    | Literal    of lit'
    | Cons       of pat node * pat node
    | List       of pat node list
    | Tuple      of pat node list
    | Record     of field' list
    | DCon       of dconref' * (pat node list)

  type pat' = pat node

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
    (* Control flow constructs *)
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
    | DCon       of dconref' * (expr' list)
    (* Identifier refernce *)
    | Var        of varref'
    (* Literals *)
    | Literal    of lit'

  and expr' = expr node

  (* This module introduces "signature" ("sigt") and "signature mask" ("sigmask") 
   * 
   * - A signature describes the "shape" of a module, that is, what TyCons, DCons
   *   and values it exposes to the outside.
   * - A signature mask specifies a means to derive a signature based on another
   *   signature by picking items from the orignal signature. 
   *
   * Ideally signatures should also contain type information. That is, signature 
   * matching should be part of type checking. However by performing this step before
   * type checking this allows as to have an IR that is useable for Elf, without having
   * to figure out type-checking. This also enables sanity checks of proper project
   * dependencies. 
   *)
  module Sig = ElAstResolved.Syntax.Sig

  type sigt    = Sig.t
  type sigmask = Sig.mask

  type m = {
    modid   : path' option;
    exports : sigt;
    imports : (path' * sigmask) list;
    (* TopLevel decls needs to keep track of field name because they are exported, 
     * In particular, a single val definition can export multiple names *)
    letm    : let_m;
  }

  let rec annots_in_pat (pat : pat') =
    match Node.elem pat with 
    | Any
    | Unit 
    | EmptyList
    | Literal _           -> []
    | Var (_, None)       -> []
    | Var (_, Some annot) -> [annot]
    | Cons (p, pn)        -> annots_in_pat p @ annots_in_pat pn
    | List pats           -> Core.List.concat_map pats ~f:annots_in_pat
    | Tuple pats          -> Core.List.concat_map pats ~f:annots_in_pat
    | Record _            -> []
    | DCon (_, pats)      -> Core.List.concat_map pats ~f:annots_in_pat
end

module Alias = 
struct
  open Syntax

  module Pat =
  struct
    type pat = Syntax.pat =
      | Var        of var' * annot option
      | Any
      | Unit 
      | EmptyList
      | Literal    of lit'
      | Cons       of pat' * pat'
      | List       of pat' list
      | Tuple      of pat' list
      | Record     of field' list
      | DCon       of dconref' * (pat' list)
  end

  module LetM = 
  struct
    type let_m = Syntax.let_m =
      | LetUnit
      | Let       of val_decl'      * let_m
      | LetRec    of val_decl' list * let_m
      | LetTyp    of typ_decl'      * let_m
      | LetTypRec of typ_decl' list * let_m

    type typ_decl = Syntax.typ_decl =
      | TyCon of (tycon' * tvar' list) * ((dcon' * typ' list) list)
      | Alias of (tycon' * tvar' list) * typ'

    type typ_decl' = typ_decl node

    type val_decl = Syntax.val_decl =
      | Pat   of pat' * expr'
      | Fun   of ((var' * annot option) * (pat' list)) * expr'
  end

  module Expr = 
  struct
    type expr = Syntax.expr =
      (* Control flow constructs *)
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
      | DCon       of dconref' * (expr' list)
      (* Identifier refernce *)
      | Var        of varref'
      (* Literals *)
      | Literal    of lit'
  end

  module Typ =
  struct
    type typ = Syntax.typ =
      | TVar   of tvar'
      | Unit
      | TyCon  of tyconref'
      | Arrow  of typ' * typ'
      | TApp   of typ' * typ'
      | Record of row
      | Tuple  of typ' list

    type row = Syntax.row =
      | RVar      of rvar'
      | Extension of row * (field' * typ') list
      | Fields    of (field' * typ') list
  end
end

module ToString = 
struct
  open Syntax
  open Util.Format

  open Core

  module ToString = ElAstLexical.ToString 

  let pos_to_string   = ToString.pos_to_string
  let op_to_string    = ToString.op_to_string
  let lit_to_string   = ToString.lit_to_string
  let field_to_string = ToString.field_to_string

  let path_to_string path = 
    let open ElAst.Syntax in
    let rec to_string p = 
      match p with 
      | Just mcon -> ElAst.MConId.to_string mcon
      | More (mcon, p) -> ElAst.MConId.to_string mcon ^ "." ^ to_string p
    in to_string (Node.elem path) 

  let binded_to_string id_to_string binder_to_string binded_node =
    let (binder, id) = Node.elem binded_node in
    "~" ^ id_to_string id ^ ":" ^ binder_to_string binder

  let var_to_string (v : var') = 
    binded_to_string VarId.to_string Var.to_string_exn v
  let dcon_to_string (d : dcon') = 
    binded_to_string DConId.to_string DCon.to_string_exn d
  let tycon_to_string (t : tycon') = 
    binded_to_string TyConId.to_string TyCon.to_string_exn t
  let tvar_to_string (tv : tvar') = 
    binded_to_string TVarId.to_string TVar.to_string_exn tv

  let ref_to_string twin_to_string id_to_string ident : string =
    match Node.elem ident with 
    | Bind bvar -> twin_to_string bvar 
    | Free (path, id) -> path_to_string path ^ "." ^ id_to_string (Node.elem id)
    | Unresolved id -> "?." ^ id_to_string (Node.elem id)

  let varref_to_string (var : varref') = 
    ref_to_string var_to_string VarId.to_string var

  let dconref_to_string (dcon : dconref') = 
    ref_to_string dcon_to_string DConId.to_string dcon

  let tyconref_to_string (tycon : tyconref') = 
    ref_to_string tycon_to_string TyConId.to_string tycon

  let rec m_to_string ({ modid; exports; imports; letm} : m) =
    sprintf ("module %s : sig {%s} = let open {%s} in struct {%s}") 
            (Core.Option.value_map modid ~default:"?" ~f:path_to_string)
            (sigt_to_string exports)
            (String.concat ~sep:";" @@ imports_to_strings imports)
            (letm_to_string letm)
  
  and sigmask_to_string (sigmask : sigmask) = 
    match sigmask with
    | Sig.Any -> "(..)"
    | Sig.Enumerated (tycons, vals) -> 
      let tycon_strs = List.map tycons ~f:(
        function 
        | (Sig.Enumerated (id_node, dcons)) -> 
          let id_str = TyConId.to_string (Node.elem id_node) in
          let dcons_str = concat_map ", " (fun id -> DConId.to_string @@ Node.elem id) dcons
          in
          sprintf "Tycon %s (%s)" id_str dcons_str
        | Sig.Any id_node -> "TyCon " ^ TyConId.to_string (Node.elem id_node) ^ " (..)"
      ) in
      let vals = List.map vals ~f:(
        fun varid_node -> "Val " ^ VarId.to_string @@ Node.elem varid_node
      ) 
      in String.concat ~sep:";" (tycon_strs @ vals) 

  and sigt_to_string (Sig.Sig (tycons, vals)) = 
    let tycon_strs = List.map tycons ~f:(
      fun (id_node, dcons) -> 
        let id_str = TyConId.to_string (Node.elem id_node) in
        let dcons_str = concat_map ", " (fun id -> DConId.to_string @@ Node.elem id) dcons
        in
        sprintf "Tycon %s (%s)" id_str dcons_str
    ) in
    let vals = List.map vals ~f:(
      fun varid_node -> "Val " ^ VarId.to_string @@ Node.elem varid_node
    ) 
    in String.concat ~sep:";" (tycon_strs @ vals) 

  and imports_to_strings imports = 
    List.map imports ~f:(
      fun (path, sigmask) ->
        let path_str = path_to_string path in
        let sig_str = sigmask_to_string sigmask in 
        sprintf "%s : Opening {%s}" path_str sig_str
    )

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
        sprintf "%s of %s" (dcon_to_string dcon)
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

  and annot_to_string annot : string = 
    sprintf "%s :: %s" (var_to_string (fst annot)) (typ_to_string (snd annot))
      
  and pat_to_string ?(with_annot=false) (pat : pat') = 
    match Node.elem pat with
    | Var (var, None) -> var_to_string var
    | Var (var, Some _) when not with_annot -> var_to_string var
    | Var (var, Some annot) -> (var_to_string var ^ ": " ^ typ_to_string (snd annot))
    | Any          -> "_"
    | Unit         -> "()"
    | EmptyList    -> "[]"
    | Literal lit  -> lit_to_string lit
    | Cons (p, pn) -> pat_to_string p ^ " :: " ^ pat_to_string pn
    | List pats    -> surround ("[", "]") @@ concat_map ", " pat_to_string pats
    | Tuple pats   -> surround ("(", ")") @@ concat_map ", " pat_to_string pats
    | Record fs    -> surround ("{", "}") @@ concat_map ", " field_to_string fs
    | DCon (con, pats) -> 
      match pats with 
      | [] -> dconref_to_string con
      | _ -> sprintf "%s of %s" (dconref_to_string con)  (concat_map " " pat_to_string pats)

  and typ_to_string (typ : typ') : string =
    match Node.elem typ with 
    | TVar tvar -> tvar_to_string tvar
    | Unit -> "()"
    | TyCon c -> tyconref_to_string c
    | Arrow (t1, t2) -> typ_to_string t1 ^ " -> " ^ typ_to_string t2
    | TApp (t1, t2) -> typ_to_string t1 ^ " " ^ typ_to_string t2
    | Tuple typs -> surround ("(", ")") @@ concat_map ", " typ_to_string typs
    | Record row -> surround ("<", ">") @@ row_to_string row

  and row_to_string (row : row) : string =
    let field_to_string (field, typ) =
       field_to_string field ^ ": " ^ typ_to_string typ
    in
    match row with 
    | RVar rvar -> tvar_to_string rvar  
    | Extension (r, fields) ->
      sprintf "%s & %s" (row_to_string r) (concat_map ", " field_to_string fields)
    | Fields fields -> concat_map ", " field_to_string fields

  and expr_to_string (e : expr') =
    let pp = expr_to_string in
    match Node.elem e with
    | LetM (letm, e)     -> sprintf "letM {%s} in %s" (letm_to_string letm) (pp e) 
    (* | Let ((tycons, vals), e)     -> 
      let type_strings = List.map ~f:typdecl_to_string tycons 
                      |> List.map ~f:(fun s -> "TyCon " ^ s) in
      let vals_strings = 
        List.concat_map vals ~f:(
          fun val_decl -> 
            let (a_strs, v_str) = decl_annot_to_string val_decl in
            a_strs @ [ v_str ]
          )
      in
      sprintf "let {%s} in %s" 
              (String.concat ~sep:";" (type_strings @ vals_strings)) 
              (pp e)  *)
    | Case (e, branches) -> 
      let arrow (p, e) = pat_to_string p ^ " -> " ^ pp e in
      sprintf "case %s of {%s}" (pp e) (concat_map "|" arrow branches)
    | If (e1, (e2, e3))  -> sprintf "if %s then {%s} else {%s}" (pp e1) (pp e2) (pp e3)
    | Lambda (pats, e)   -> sprintf "fn %s = {%s}" (concat_map " " pat_to_string pats) (pp e)
    | App (e, es)        -> concat_map " " pp (e::es)
    | Infix (op, e1, e2) -> sprintf "%s %s %s" (pp e1) (op_to_string op) (pp e2)
    | Unit               -> "()"
    | Tuple es           -> surround ("(", ")") @@ concat_map ", " pp es
    | List es            -> surround ("[", "]") @@ concat_map ", " pp es
    | Record field_es    -> 
      let record_field (f, e) = field_to_string f ^ " = " ^ pp e in
      surround ("<{", "}>") @@ (concat_map ";" record_field field_es)
    | DCon (con, es)      -> dconref_to_string con ^ concat_map " " pp es
    | Var (var)          -> varref_to_string var
    | Literal lit        -> lit_to_string lit
end