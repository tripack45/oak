(* Resolved ElAst - See PhaseResolveSymbols.ml
 *)

module Node    = ElAst.Node

module VarId   = ElAst.VarId
module TVarId  = ElAst.TVarId
module RVarId  = ElAst.TVarId
module TyConId = ElAst.TyConId
module DConId  = ElAst.DConId
module MConId  = ElAst.MConId

module type BINDER = 
sig
  type t  (* Abstract type for the binder *)
  type id (* A associated human readable id for the binder *)
  val fresh : ?id:id -> unit -> t
  val id : t -> id option
  val to_cannonical_string : string -> t -> string
  val to_string_exn : t -> string
  val compare : t -> t -> int
end

(* We can have multiple variable "namespaces" if we want. 
 * Each instantiation of this functor gives us a separate, independent
 * namespace of variables. *)
module VarMake (M : ElAst.IDENT) () : BINDER with type id = M.t 
= struct 
  open Core

  type t = int
  type id = M.t

  module Map = Int.Map

  let map : (id Map.t) ref = ref Map.empty
  let count = ref 0

  let fresh ?id () = 
    let r = !count in 
      Option.iter id ~f:(fun id -> map := Map.add_exn (!map) ~key:r ~data:id);
      count := r + 1; 
      r
  let id t = Int.Map.find !map t

  let to_cannonical_string prefix t = prefix ^ Int.to_string t

  let to_string_exn t = 
    (Core.Option.value_exn (id t) |> M.to_string) ^ Int.to_string t

  let compare = Int.compare
end


(* Type constructors and data constructors, though following the same syntax 
 * are in their own independent namespaces, therefore needs to be tracked separately. 
 * For example, the following code is valid Elm code:
 *     (repl)
 *     > type T = S () | T Int
 *     > T 1
 *     T 1 : T
 *)
module Var   = VarMake (VarId) ()
module TVar  = VarMake (TVarId) ()
module DCon  = VarMake (DConId) ()
module TyCon = VarMake (TyConId) ()

module Syntax =
struct
  type pos = Lexing.position * Lexing.position

  (* A regular AST node encodes source file location *)
  type 'a node = 'a ElAst.Syntax.node

  type field  = ElAst.Syntax.field
  type lit    = ElAst.Syntax.lit

  type path   = ElAst.Syntax.path = 
    | Just of MConId.t
    | More of MConId.t * path

  type field'  = field node
  type lit'    = lit node
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
    | Con        of dconref' * (pat node list)

  type pat' = pat node

  type op   = ElAst.Syntax.op

  type expr = 
    (* Control flow constructs *)
    | Let        of (typdecl list * decl list) * expr'
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
    | Con        of dconref' * (expr' list)
    (* Identifier refernce *)
    | Var        of varref'
    (* Literals *)
    | Literal    of lit'

  and expr' = expr node

  and typdecl = 
    | TyCon of (tycon' * tvar' list) * ((dcon' * typ' list) list)
    | Alias of (tycon' * tvar' list) * typ'

  and decl =
    | Pat   of pat' * expr'
    | Fun   of (var' * annot option) * (pat' list) * expr'
    | Port  of var' * typ'

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
  module Sig =
  struct
    (* Signature type *)
    type sig_tycon = TyConId.t node * (DConId.t node list)
    type sig_val   = VarId.t node
    type t = Sig of (sig_tycon list) * (sig_val list)

    (* Signature mask *)
    type sigmask_tycon = 
      | Enumerated of TyConId.t node * (DConId.t node list)
      | Any        of TyConId.t node

    type sigmask_val = VarId.t node

    type mask = 
      | Any 
      | Enumerated of (sigmask_tycon list) * (sigmask_val list)
  end

  type sigt    = Sig.t
  type sigmask = Sig.mask

  type m = {
    modid   : path' option;
    exports : sigt;
    imports : (path' * sigmask) list;
    (* TopLevel decls needs to keep track of field name because they are exported, 
     * In particular, a single val definition can export multiple names *)
    tycons  : typdecl list;
    vals    : decl list;
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
    | Con (_, pats)       -> Core.List.concat_map pats ~f:annots_in_pat
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
      | Con        of dconref' * (pat' list)
  end

  module Expr = 
  struct
    type expr = Syntax.expr =
      (* Control flow constructs *)
      | Let        of (typdecl list * decl list) * expr'
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
      | Con        of dconref' * (expr' list)
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

  let pos_to_string   = ElAst.ToString.pos_to_string
  let op_to_string    = ElAst.ToString.op_to_string
  let lit_to_string   = ElAst.ToString.lit_to_string
  let field_to_string = ElAst.ToString.field_to_string

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

  let rec m_to_string ({ modid; exports; imports; tycons; vals } : m) =
    let tycons_strings = 
      List.map tycons ~f:(fun tycon -> sprintf "TyCon %s" (typdecl_to_string tycon))
    in
    let vals_strings = 
      List.concat_map vals ~f:(
        fun val_decl -> 
          let (a_strs, v_str) = decl_annot_to_string val_decl in
          a_strs @ [ v_str ]
        )
    in
    sprintf ("module %s : sig {%s} = let open {%s} in struct {tycons {%s};vals {%s}}") 
            (Core.Option.value_map modid ~default:"?" ~f:path_to_string)
            (sigt_to_string exports)
            (String.concat ~sep:";" @@ imports_to_strings imports)
            (String.concat ~sep:";" tycons_strings)
            (String.concat ~sep:";" vals_strings)
  
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

  and typdecl_to_string typdecl: string = 
    match typdecl with 
    | TyCon ((tycon, tvars), ctors) ->
      let ctor_to_string (dcon, typs) = 
        sprintf "%s of %s" (dcon_to_string dcon)  (concat_map " " typ_to_string typs)
      in
      sprintf "(%s) %s = {%s}" 
              (tycon_to_string tycon)
              (concat_map ", " tvar_to_string tvars)
              (concat_map "|" ctor_to_string ctors)
    | Alias ((tycon, tvars), typ) ->
      sprintf "(%s) %s = {%s}" 
              (tycon_to_string tycon)
              (concat_map ", " tvar_to_string tvars)
              (typ_to_string typ)

  and annot_to_string annot : string = 
    sprintf "%s :: %s" (var_to_string (fst annot)) (typ_to_string (snd annot))

  (* Annotations are to be put on their own lines *)
  and decl_annot_to_string valbind : string list * string = 
    (* Annotations are not supported yet *)
    match valbind with 
    | Pat (pat, e) -> 
      let a_strs  = List.map (annots_in_pat pat) ~f:annot_to_string in
      let e_str   = sprintf "Val %s = %s" (pat_to_string pat) (expr_to_string e) in
      let a_strs' = List.map a_strs ~f:(fun t -> "Annot " ^ t) in
      (a_strs', e_str)
    | Port (var, typ) -> 
      ([sprintf "Port %s %s" (var_to_string var) (typ_to_string typ)], "")
    | Fun ((f, annot_opt), pats, e) -> 
      let fun_str = 
        sprintf "Fun %s %s = %s" 
          (var_to_string f)
          (concat_map " " pat_to_string pats)
          (expr_to_string e);
      in
      match annot_opt with 
      | None       -> ([], fun_str)
      | Some annot -> (["Annot " ^ annot_to_string annot], fun_str)
      
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
    | Con (con, pats) -> 
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
    | Let ((tycons, vals), e)     -> 
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
              (pp e) 
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
    | Con (con, es)      -> dconref_to_string con ^ concat_map " " pp es
    | Var (var)          -> varref_to_string var
    | Literal lit        -> lit_to_string lit

  

end