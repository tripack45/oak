(*
 * This pass resolves references to external dependencies. Specifically it performs the following transformation:
 *
 * - Collects all import declarations
 *   - Resolves imported module Paths to full paths
 *   - Registers imported identifiers 
 *   - Collects modules with unspecified imports "exposing (..)"
 * 
 * - For each refrence to a Type/Data constructor, it classifies it as one of the following
 *   - Con,  such that the constructor is binded inside this module.
 *   - FCon, such that the constructor is known to reference a resolved constructor in another module  
 *   - Unresolved, such that the constructor is either undefined, or references a constructor of another module
 *
 * - For each reference to a variable, it classifies it as one of the following
 *   - Var, the variable is binded locally. Such variables are alpha-varied to an internal abstract representation, 
 *     This makes sure that variables are always fresh, and avoids accidental captures down the stream.
 *   - FVar, the variable is referenes a field of a known module whose path,
 *   - Unresolved, such that the variable may be undefined.contents
 *
 * - Resolves type annotation to their corresponding identifier
 *
 * - Collects the static portion of this module: the values, types, data constructors this module exposes. 
 *
 * The analysis can be run either in separate compilation (as described) or in whole-programe mode. With whole program
 * mode, an export dictionary of depended modules must be supplied, and unresolved references will be reported as 
 * errors. Separately compiled module can be later "linked" with export dictionaries. 
 *)

open ElAstResolved
open Core

module P = ElAst.Syntax
module R = ElAstResolved.Syntax

type tycon_exposure = 
  | Opaque
  | Transparent

module Map = Core.Map.Poly
module Set = Core.Set.Poly

module ConId = ElAst.ConId

let as_resolved_node node = 
  let (elem, attr) = Node.both node in
  Node.node elem (Some attr)

let as_dconid con = 
  let (elem, attr) = Node.both con in
  Node.node (DConId.of_string (ConId.to_string elem)) (Some attr)

let as_tyconid con = 
  let (elem, attr) = Node.both con in
  Node.node (TyConId.of_string (ConId.to_string elem)) (Some attr)


type path_dict = (P._path, R._path) Map.t
type dcon_map  = (DConId.t, R._path) Map.t
type fvar_map  = (VarId.t, R._path) Map.t
type dicts = path_dict * dcon_map * fvar_map

(* tctx : Type Constructor Context
 * dctx : Data Constructor Context
 * vctx : Variable Context
 *  
 * Resolution of data constructors must be deferred to until typechecking. Although Elm prevents
 * shadowing of data constructors (at least shadowing between locally defined data constructors), such
 * restrictions does makes it harder to code therefore we might want to relax such restrictions. 
 * Such design would require the compiler to disambiguous data constructors of same name at type-checking
 * based on the type of the constructor inferred (or defaults to a choice). Therefore we must maintain 
 * name even for locally defined data constructors.
 *)
type tctx = (TyConId.t, TyCon.t) Map.t
type dctx = (DConId.t) Set.t
type vctx = (VarId.t, Var.t) Map.t

let resolve_imports ~export_dict (imports : P.import list) : path_dict * (R.path * R.imported_exposed) list =
  let f (path_map : path_dict) (P.Import (path, as_con_opt, exposing_opt)) = 
    let path_map' = 
      match as_con_opt with
      | None -> Map.add_exn path_map ~key:(Node.elem path) ~data:(Node.elem path)
      | Some con -> 
        let as_path = P.Just (Node.elem con) in
        Map.add_exn path_map ~key:as_path ~data:(Node.elem path)
    in
    let import' : R.path * R.imported_exposed = 
      match exposing_opt with 
      | None -> (as_resolved_node path, R.Ids {tycons=[]; vals=[]})
      | Some P.Any -> (as_resolved_node path, R.Unresolved)
      | Some (P.Idents ids) ->
        let fcon exposed_tycon = 
          match exposed_tycon with 
          | P.AbsTyCon con -> Option.some @@ R.TyCon (as_tyconid con, R.Opaque)
          | P.TyCon con ->    Option.some @@ R.TyCon (as_tyconid con, R.Unresolved)
          | P.Var _ ->        None
        in
        let fvar exposed_var =
          match exposed_var with 
          | P.AbsTyCon _ | P.TyCon _  -> None
          | P.Var v -> Some (as_resolved_node v)
        in
        let tycons = List.filter_map ids ~f:fcon
        and vals   = List.filter_map ids ~f:fvar in
        (as_resolved_node path, R.Ids { tycons; vals })
    in 
    (path_map', import')
  in
  List.fold_map imports ~init:Map.empty ~f:f

let imports_to_maps (imports : (R.path * R.imported_exposed) list) =
  let f ((dcon_map, fvar_map) as maps) (path, exposed) =
    match exposed with 
    | R.Unresolved -> maps
    | R.Ids {tycons; vals} -> 
      let dcon_map' = List.fold tycons ~init:dcon_map ~f:(
        fun map tycon -> 
          match tycon with 
          | R.Alias _  
          | R.TyCon (_, R.Unresolved) 
          | R.TyCon (_, R.Opaque) -> map
          | R.TyCon (_, R.DCons conids) -> 
            List.fold conids ~init:map ~f:(
              fun (map : dcon_map) conid -> Map.add_exn map ~key:(Node.elem conid) ~data:(Node.elem path)
            )
      )
      in 
      let fvar_map' = List.fold vals ~init:fvar_map ~f:(
        fun map var -> Map.add_exn map ~key:(Node.elem var) ~data:(Node.elem path)
      )
      in
      (dcon_map', fvar_map')
  in
  List.fold imports ~init:(Map.empty, Map.empty) ~f

let resolve_var ((path_map, _, fvar_map): dicts) (vctx : vctx) (qvar : P.qvar) =
  let (P.QVar (path_opt, var)) = Node.elem qvar in
  match path_opt with 
  | Some path -> 
    let path' = Map.find path_map (Node.elem path) 
              (* Standard libraries can be accessed without imports
               * Ideally we would maintain a whitelist instead of accepting everythin
               *)
              |> Option.value ~default:(Node.elem path)
    in
    let path_node' = Node.node path' (Some (Node.attr path)) in
    let fvar = R.FVar (R.Resolved (path_node', as_resolved_node var)) in
    Node.node fvar (Some (Node.attr qvar))
  | None -> 
    match Map.find vctx (Node.elem var) with 
    | Some bvar -> 
      let var' = R.BVar (Node.node bvar (Some (Node.attr var))) in
      Node.node var' (Some (Node.attr qvar))
    | None ->
      match Map.find fvar_map (Node.elem var) with
      | Some path' -> 
        let path_node' = Node.node path' None in
        let fvar = R.FVar (R.Resolved (path_node', as_resolved_node var)) in
        Node.node fvar (Some (Node.attr qvar))
      | None -> 
        let fvar = R.FVar (R.Unresolved (as_resolved_node var)) in
        Node.node fvar (Some (Node.attr qvar))

let resolve_dcon ((path_map, dcon_map, _) : dicts) (dctx : dctx) qcon =
  let (P.QCon (path_opt, con)) = Node.elem qcon in
  match path_opt with 
  | Some path -> 
    let path' = Map.find_exn path_map (Node.elem path) in
    let path_node' = Node.node path' (Some (Node.attr path)) in
    let fcon = R.FDCon (R.Resolved (path_node', as_resolved_node con)) in
    Node.node fcon (Some (Node.attr qcon))
  | None -> 
    if Set.mem dctx (Node.elem con) then
      let con' = R.BDCon (Node.node (Node.elem con) (Some (Node.attr con))) in
      Node.node con' (Some (Node.attr qcon))
    else
      match Map.find dcon_map (Node.elem con) with
      | Some path' -> 
        let path_node' = Node.node path' None in
        let fdcon = R.FDCon (R.Resolved (path_node', as_resolved_node con)) in
        Node.node fdcon (Some (Node.attr qcon))
      | None -> 
        let fdcon = R.FDCon (R.Unresolved (as_resolved_node con)) in
        Node.node fdcon (Some (Node.attr qcon))

let rec collect_binders (pat : P.pat) : VarId.t list =
  match Node.elem pat with 
  | P.Any | P.Unit | P.EmptyList | P.Literal _ -> []
  | P.List pats  -> List.concat (List.map pats ~f:collect_binders)
  | P.Tuple pats -> List.concat (List.map pats ~f:collect_binders)
  | P.Ctor (_, pats) -> List.concat (List.map pats ~f:collect_binders)
  | P.Var v -> [Node.elem v]

let bind_binders vctx varids =
  List.fold_map varids ~init:vctx ~f:(
    fun vctx varid ->
      let bvar = Var.fresh ~id:varid () in
      let vctx' = Map.add_exn vctx ~key:varid ~data:bvar in
      (vctx', (varid, bvar))
  )

let rec translate_pat ((_tctx, dctx, vctx) as ctx) (dicts : dicts) (pat : P.pat) : R.pat =
  let (elem, pos) = Node.both pat in
  let pat_node (pat : R._pat) = Node.node pat (Some pos) in
  let tr = translate_pat ctx dicts in
  match elem with 
  | P.Any         -> pat_node R.Any
  | P.Unit        -> pat_node R.Unit
  | P.EmptyList   -> pat_node R.EmptyList
  | P.Literal lit -> pat_node @@ R.Literal lit
  | P.List pats   -> pat_node @@ R.List  (List.map pats ~f:tr)
  | P.Tuple pats  -> pat_node @@ R.Tuple (List.map pats ~f:tr)
  | P.Ctor (qcon, pats) -> 
    let con' = resolve_dcon dicts dctx qcon in
    pat_node @@ R.Con (con', List.map pats ~f:tr)
  | P.Var v -> 
    let (id, pos) = Node.both v in
    let var : Var.t = Map.find_exn vctx id in
    pat_node @@ R.Var (Node.node var (Some pos))

let rec translate_expr ((tctx, dctx, vctx) as ctx) dicts (expr : P.expr) : R.expr = 
  let tr e = translate_expr ctx dicts e in
  let tr_vctx vctx' e = translate_expr (tctx, dctx, vctx') dicts e in
  let (expr, (pos, _)) = Node.both expr in
  let expr' =
    match expr with 
    | P.Let (decls, e) -> 
      let (ctx', (ids_tycons', ids_vals')) = translate_decls ctx dicts decls in
      (* Translated decls includes list of binded names for toplevel. Need to project out.*)
      let tycons' = List.map ~f:snd ids_tycons' in
      let vals'   = List.map ~f:snd ids_vals' in
      let e'      = translate_expr ctx' dicts e in
      R.Let ((tycons', vals'), e')
    | P.Case (e, branches)   -> 
      let branches' = List.map branches ~f:(
        fun (pat, expr) -> 
          let (vctx', _) = bind_binders vctx (collect_binders pat) in
          let pat' = translate_pat (tctx, dctx, vctx') dicts pat in
          (pat', tr_vctx vctx' expr)
      )
      in R.Case (tr e, branches')
    | P.Lambda (pats, e)     -> 
      let ids = List.concat @@ List.map pats ~f:collect_binders in 
      let (vctx', _) = bind_binders vctx ids in
      let pats' = List.map pats ~f:(translate_pat (tctx, dctx, vctx') dicts) in
      R.Lambda (pats', tr_vctx vctx' e)
    | P.If (e0, (e1, e2))    -> R.If (tr e0, (tr e1, tr e2))
    | P.App (e0, es)         -> R.App (tr e0, List.map es ~f:tr)
    | P.Infix (op, e1, e2)   -> R.Infix (op, tr e1, tr e2)
    | P.OpFunc op            -> R.OpFunc op
    | P.Unit                 -> R.Unit
    | P.Literal l            -> R.Literal l
    | P.Tuple es             -> R.Tuple (List.map es ~f:tr)
    | P.List es              -> R.List (List.map es ~f:tr)
    | P.Record fields        -> R.Record (List.map fields ~f:(fun (field, e) -> (field, tr e)))
    | P.Var v                -> R.Var (resolve_var dicts vctx v)
    | P.Con (qcon, es)       -> 
      let con' = resolve_dcon dicts dctx qcon in
      let es' = List.map es ~f:tr in
      R.Con (con', es')
  in
  Node.node expr' (Some pos)

(* Declarations in Elm presents a wide range of semantics issues that needs to be
 * carefully taken care of.contents
 *
 * Elm allows type declarations at toplevel scope. This restriction can be relaxed 
 * to allow type definitions at local scope. Relaxing this restriction does create 
 * what is known as "the avoidence problem" as types defined may escape its defining
 * scope. The possiblity of relaxing this restriction was left in place in case we
 * want to revisit it in the future. 
 *
 * In Elm, definistions can be created out of order, and they are (like it or not)
 * all mutually recursive. In addition, all identifiers defined in the same scope 
 * are simutaneously available to all subexpressions, type and value defintions alike.
 *
 * On the other hand, lack of order also renders shadowing among defintions at same 
 * scope impossible. As a corollary this allows us to collect type definitions and 
 * value defintions into two separate bins without needing to worry about accidental
 * captures.
 *
 * On the other hand, local definitions are up to alpha variance therefore
 *)
and translate_decls (tctx, dctx, vctx) ((path_dict, dcon_map, fvar_map) as dicts) (decls : P.decl list) =

  let extract_tycons decls = 
    List.filter_map decls ~f:(
      fun decl ->
        match Node.elem decl with 
        | P.TyCon -> assert false 
        | _ -> None
    )
  in
  
  let translate_tycons (tctx, dctx) tycons : tctx * dctx * (((TyConId.t * TyCon.t) * R.typdecl) list) =
    let ((tctx', dctx'), tycons') = 
      List.fold_map tycons ~init:(tctx, dctx) ~f:(
        fun (tctx, dctx) tycon -> 
          (* TODO: Support translation of type constructors 
           * Current type constructors don't parse hence list would be empty *)
          assert false
      )
    in (tctx', dctx', tycons')
  in

  (* Returns 
   * - The contexts for expression under the declared scope 
   * - The translated declarations
   *)
  let translate_vals (tctx, dctx, vctx) (decls : P.decl list) = 
    (* Extract a context and a list of varid-binder pairs from decls 
     * Right now the list is not that useful though *)
    let (vctx', _) = 
      (* We first start off by collecting all toplevel binders *)
      List.map decls ~f: (
        fun decl ->  
          match Node.elem decl with 
          | P.Fun ((f, _), e) -> [Node.elem f]
          | P.Pat (pat, e) -> collect_binders pat
          | _ -> []
      ) |> List.concat |> bind_binders vctx
      (* Create a variable for every binder *)
    in
    let translate_decl vctx decl =
      match Node.elem decl with 
      | P.Fun ((var_node, pats), e) -> 
        let argids = List.concat (List.map ~f:collect_binders pats) in
        let (vctx', _) = bind_binders vctx argids in
        let pats' = List.map pats ~f:(translate_pat (tctx, dctx, vctx') dicts) in
        let e'    = translate_expr (tctx, dctx, vctx') dicts e in
        let (fid, pos) = Node.both var_node in
        let fbind = Map.find_exn vctx fid in 
        let decl' = R.Fun (None, ((Node.node fbind (Some pos)), pats'), e') in
        Some ([(fid, fbind)], decl')
      | P.Pat (pat, e) -> 
        (* The input has already binded pat *)
        let e'   = translate_expr (tctx, dctx, vctx) dicts e in
        let pat' = translate_pat (tctx, dctx, vctx) dicts pat in 
        (* We need to recover the binded ids for toplevel *)
        let binds = 
          List.map (collect_binders pat) ~f:(
            fun varid -> (varid, Map.find_exn vctx varid)
          )
        in
        Some (binds, R.Pat ([], pat', e'))
      |_ -> None
    in 
    (vctx', List.filter_map decls ~f:(translate_decl vctx'))
  
  in
  let (tctx', dctx', tycons') = translate_tycons (tctx, dctx) (extract_tycons decls) in
  let (vctx', decls') = translate_vals (tctx, dctx, vctx) decls in
  (* TODO: Add an extra step to attach type annotations to their coresponding defintions
   * Right now this translation simply throws away all type annotations, because the parse
   * does not support it for now. *)
  ((tctx', dctx', vctx'), (tycons', decls'))

(* TODO: perform imported dictionary lookup *)
let resolve ~modpath ~export_dict (t : P.m) : R.m = 
  let (P.Mod (mdecl, imports, decls)) = t in
  let (path_map, imports) = resolve_imports ~export_dict imports in
  let (dcon_map, fvar_map) = imports_to_maps imports in
  let (_, (ids_tycons', ids_vals')) = 
    translate_decls (Map.empty, Set.empty, Map.empty) (path_map, dcon_map, fvar_map) decls in
  (* Now we filter the organized tycons and values with the module export spec
   * to generate the list of exported identifiers. *)
  {
    modid   = modpath;
    exports = {tycons=[]; vals=[]};
    imports = imports; 
    tycons  = ids_tycons';
    vals    = ids_vals';
  }

