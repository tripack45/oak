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
    modid   : path node option;
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

end