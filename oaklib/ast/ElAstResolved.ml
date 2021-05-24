(* Resolved ElAst - See PhaseResolveSymbols.ml
 *)

module Node    = ElAst.Node

module VarId   = ElAst.VarId
module TyConId = ElAst.ConId
module DConId  = ElAst.ConId

(* We can have multiple variable "namespaces" if we want. 
 * Each instantiation of this functor gives us a separate, independent
 * namespace of variables. *)
module VarMake (M : sig type t end) () : 
sig
  type t 
  type id = M.t
  val fresh : ?id:id -> unit -> t
  val id : t -> M.t option
  val to_cannonical_string : string -> t -> string
  val compare : t -> t -> int
end =
struct 
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
module DCon  = VarMake (DConId) ()
module TyCon = VarMake (TyConId) ()

module Syntax =
struct
  type pos = Lexing.position * Lexing.position

  (* A regular AST node encodes source file location *)
  type 'a node     = ('a, pos option) Node.t

  type bvar   = Var.t node
  type bdcon  = DConId.t node
  type btycon = TyCon.t node

  type _lit   = ElAst.Syntax._lit
  type _path  = ElAst.Syntax._path

  type field = ElAst.Syntax.field
  type lit   = ElAst.Syntax.lit
  type path  = _path node

  type 'id free = 
     | Resolved   of path * 'id
     | Unresolved of 'id

  type fvar = (VarId.t node) free
  type var = 
     | BVar of bvar
     | FVar of fvar

  type fdcon = (DConId.t node) free
  type dcon = 
     | BDCon of bdcon
     | FDCon of fdcon

  type ftycon = (DConId.t node) free
  type tycon = 
     | BTyCon of btycon
     | FTyCon of ftycon

  type _pat = 
    | Var        of bvar
    | Any
    | Unit 
    | EmptyList
    | Literal    of lit
    | List       of pat list
    | Tuple      of pat list
    | Con        of dcon node * (pat list)
  and pat = _pat node

  (* TODO: implement TyCon resolution *)
  type typ = ElAst.Syntax.typ
  type op  = ElAst.Syntax.op

  type annot = var node * typ node 

  type typdecl = 
    | TyCon
    | Alias

  type _expr = 
    (* Control flow constructs *)
    | Let        of (typdecl list * decl list) * expr 
    | Case       of expr * ((pat * expr) list)
    | If         of expr * (expr * expr)
    | Lambda     of pat list * expr
    | App        of expr * (expr list)
    (* Operators *)
    | Infix      of op * expr * expr
    | OpFunc     of op
    (* Builtin Types *)
    | Unit
    | Tuple      of expr list  (* >= 2 elements *)
    | List       of expr list  (* >= 0 elements *)
    | Record     of (field * expr) list
    (* Data construction *)
    | Con        of dcon node * (expr list)
    (* Identifier refernce *)
    | Var        of var node
    (* Literals *)
    | Literal    of lit
  and expr = _expr node

  and decl =
    | Pat   of annot list * pat * expr
    | Fun   of annot option * (bvar * (pat list)) * expr

  type imported_dcon = 
    (* exposing (...,TyCon(..), ...  )*)
    | Unresolved 
    | DCons of DConId.t node list
    | Opaque

  type exposed_tycon_kind =
    | TyCon of TyConId.t node * imported_dcon
    | Alias of TyConId.t 

  type ids = {
    tycons : exposed_tycon_kind list;
    vals   : VarId.t node list;
  }

  type imported_exposed = 
    (* import M exposing (..) **)
    | Unresolved
    | Ids of ids

  type m = {
    modid   : path option;
    exports : ids;
    imports : (path * imported_exposed) list;
    (* TopLevel decls needs to keep track of field name because they are exported, 
     * In particular, a single val definition can export multiple names *)
    tycons  : ((TyConId.t * TyCon.t) * typdecl) list;
    vals    : ((VarId.t   * Var.t) list * decl) list;
  }
end

module ToString = 
struct
  open Printf
  open Syntax
  open Util.Format

  open Core

  let pos_to_string   = ElAst.ToString.pos_to_string
  let op_to_string    = ElAst.ToString.op_to_string
  let lit_to_string   = ElAst.ToString.lit_to_string
  let field_to_string = ElAst.ToString.field_to_string

  let path_to_string (path : path) = 
    let open ElAst.Syntax in
    let rec to_string p = 
      match p with 
      | Just con -> ElAst.ConId.to_string con
      | More (con, p) -> ElAst.ConId.to_string con ^ "." ^ to_string p
    in to_string (Node.elem path) 

  let rec m_to_string ({ modid; exports; imports; tycons; vals } : m) =
    let tycons_strings = 
      assert (List.is_empty tycons); 
      ["(* Unimplemented *)"] 
    in
    let vals_strings = 
      List.map vals ~f:(
        fun (ids, val_decl) ->
          let ids_strs = 
            List.map ids ~f:(
              fun (id, var) -> 
                "~" ^ VarId.to_string id ^ ":" ^ Var.to_cannonical_string "x" var
            )
            |> String.concat ~sep:", "
            |> surround ("(", ")") in
          sprintf "Val %s : %s" ids_strs (val_to_string val_decl)
      )
    in
    sprintf ("let {%s} in %s : sig {%s} = struct {tycons {%s};vals {%s}}") 
            (String.concat ~sep:";" @@ imports_to_strings imports)
            (Core.Option.value_map modid ~default:"?" ~f:path_to_string)
            (exports_to_string exports)
            (String.concat ~sep:";" tycons_strings)
            (String.concat ~sep:";" vals_strings)

  and imports_to_strings imports = 
    List.map imports ~f:(
      fun (path, exposed) ->
        let path_str = path_to_string path in
        let sig_str = 
          match exposed with 
          | Unresolved -> "(..)"
          | Ids {tycons; vals} -> 
            let tycon_strs = List.map tycons ~f:(
              function 
              | (TyCon (id_node, dcons)) -> 
                let id_str = TyConId.to_string (Node.elem id_node) in
                let dcons_str = 
                  match dcons with 
                  | Unresolved -> ".."
                  | Opaque -> ""
                  | DCons dcons -> 
                    concat_map ", " (fun id -> DConId.to_string @@ Node.elem id) dcons
                in
                sprintf "Tycon %s (%s)" id_str dcons_str
              | Alias id_node -> "TyCon " ^  TyConId.to_string id_node
            ) in
            let vals = List.map vals ~f:(
              fun varid_node -> "Val " ^ VarId.to_string @@ Node.elem varid_node
            ) 
            in String.concat ~sep:";" (tycon_strs @ vals)
        in 
        sprintf "%s : Opening {%s}" path_str sig_str
    )

  and exports_to_string {tycons; vals} =
    assert (List.is_empty tycons);
    assert (List.is_empty vals);
    "(* Unimplemented *)"

  and tycon_to_string _tycon : string = 
    assert false

  and val_to_string valbind : string = 
    (* Annotations are not supported yet *)
    match valbind with 
    | Pat (annots, pat, e) -> 
      assert (List.is_empty annots);
      sprintf "%s = %s" (pat_to_string pat) (expr_to_string e)
    | Fun (annot, (f, pats), e) -> 
      assert (Option.is_none annot);
      sprintf "Fun %s %s = %s" 
              (Var.to_cannonical_string "x" (Node.elem f))
              (concat_map " " pat_to_string pats)
              (expr_to_string e)
      
  and pat_to_string (pat : pat) = 
    match Node.elem pat with
    | Var var      -> Var.to_cannonical_string "x" (Node.elem var)
    | Any          -> "_"
    | Unit         -> "()"
    | EmptyList    -> "[]"
    | Literal lit  -> lit_to_string lit
    | List pats   -> surround ("[", "]") @@ concat_map ", " pat_to_string pats
    | Tuple pats  -> surround ("(", ")") @@ concat_map ", " pat_to_string pats
    | Con (con, pats) -> 
      match pats with 
      | [] -> dcon_to_string con
      | _ -> sprintf "%s of %s" (dcon_to_string con)  (concat_map " " pat_to_string pats)

  and expr_to_string (e : expr) =
    let pp = expr_to_string in
    match Node.elem e with
    | Let ((tycons, vals), e)     -> 
      let type_strings = List.map ~f:tycon_to_string tycons 
                      |> List.map ~f:(fun s -> "TyCon " ^ s) in
      let vals_strings = List.map ~f:val_to_string vals 
                      |> List.map ~f:(fun s -> "Val " ^ s) in
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
    | Con (con, es)      -> dcon_to_string con ^ concat_map " " pp es
    | Var (var)          -> var_to_string var
    | Literal lit        -> lit_to_string lit

  and var_to_string (var : var node) = 
    match Node.elem var with 
    | BVar bvar -> Var.to_cannonical_string "x" (Node.elem bvar)
    | FVar (Resolved (path, id)) -> path_to_string path ^ "." ^ VarId.to_string (Node.elem id)
    | FVar (Unresolved id) -> "?." ^ VarId.to_string (Node.elem id)

  and dcon_to_string con = 
    match Node.elem con with 
    | BDCon dconid -> DConId.to_string (Node.elem dconid)
    | FDCon (Resolved (path, id)) -> path_to_string path ^ "." ^ DConId.to_string (Node.elem id)
    | FDCon (Unresolved id) -> "?." ^ DConId.to_string (Node.elem id)

end