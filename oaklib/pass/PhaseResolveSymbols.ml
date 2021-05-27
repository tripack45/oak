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

let as_resolved_path (path : P.path') : R.path' =
  Node.map_attr Option.some path

let as_dconid conid = DConId.of_string (ConId.to_string conid)
let as_tyconid conid = TyConId.of_string (ConId.to_string conid)

(* TyCons and DCons must use separate map because they live in different 
 * namespaces, hence name collisions between them would be bad. Consider this:
 * 
 * import ModuleX exposing ( Con(..) )
 * import ModuleY exposing ( TyCon(Con) )
 *
 * In this example uses of Con can either be ModuleX.Con or ModuleY.Con, depending
 * whether Con is being used as a TyCon or a DCon
 *)
type tycon_map = (TyConId.t, R.path) Map.t
type dcon_map  = (DConId.t, R.path) Map.t
type fvar_map  = (VarId.t, R.path) Map.t
type dicts = tycon_map * dcon_map * fvar_map

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

(* Applies a sigmask to a signature *)
let apply_sigmask (sigmask : R.sigmask) (sigt : R.sigt) : R.sigt =
  match sigmask with 
  | R.Sig.Any -> sigt
  | R.Sig.Enumerated (mask_tycons, mask_vals) ->
    let (R.Sig.Sig (sig_tycons, sig_vals)) = sigt in
    (* Values are pretty simple: every value appears in the mask must appear
     * in the source signatures, otherwise it's a failure *)
    let vals = 
      let sig_val_map = List.fold sig_vals ~init:Map.empty ~f:(
        fun map node -> Map.add_exn map ~key:(Node.elem node) ~data:node
      ) in
      List.map mask_vals ~f:(fun varid_node -> Map.find_exn sig_val_map (Node.elem varid_node))
    in
    (* Type constructors are more complicated. In addition to previous requirement,
     * if the mask enumerates type constructors, the enumerated type construct most precisely
     * match the list of type constructors in the sig: it is not legal to expose only a subset
     * of type constructors of a type. *)
    let tycons = 
      let sig_tycon_map = List.fold sig_tycons ~init:Map.empty ~f:(
        fun map node -> Map.add_exn map ~key:(Node.elem @@ fst @@ node) ~data:node
      ) in
      List.map mask_tycons ~f:(
        function 
        | R.Sig.Any conid_node -> Map.find_exn sig_tycon_map (Node.elem conid_node)
        | R.Sig.Enumerated (conid_node, mask_dcons) -> 
          let compare cnode1 cnode2 = DConId.compare (Node.elem cnode1) (Node.elem cnode2) in
          let (sig_tycon, sig_dcons) = Map.find_exn sig_tycon_map (Node.elem conid_node) in
          if List.is_empty mask_dcons then
            (sig_tycon, [])
          else
            let sig_dcons'  = List.sort sig_dcons ~compare in
            let mask_dcons' = List.sort mask_dcons ~compare in
            assert (List.equal (fun x y -> compare x y = 0) sig_dcons' mask_dcons');
            (sig_tycon, sig_dcons')
      )
    in
    R.Sig.Sig (tycons, vals)

let sig_empty : R.sigt = R.Sig.Sig ([], [])

let sigmask_any : R.sigmask = R.Sig.Any
let sigmask_none : R.sigmask = R.Sig.Enumerated ([], [])

(* Obtain a minimum signature that satisfies the sigmask *)
let sigmask_minimum_sig (sigmask : R.sigmask) : R.sigt = 
  match sigmask with 
  | R.Sig.Any -> sig_empty
  | R.Sig.Enumerated (tycon_masks, val_masks) ->
    let vals = val_masks in
    let tycons = List.map tycon_masks ~f:(
      function R.Sig.Enumerated (tycon, dcons) -> (tycon, dcons)
             | R.Sig.Any tycon -> (tycon, [])
    )
    in 
    R.Sig.Sig (tycons, vals)


(* Missing an exposing clause have different semantics depending where the exposing clause exists:
 *
 * - For mdecl  , missing "exposing" is equivalent to "exposing (..)"
 * - For imports, missing "exposing" is equivalent ot "exposing ()"
 * 
 * This deisgn makes sense for import statement but arguably makes much less sense for module decl
 * Without exposing statement a better design would be treating it as exposing nothing 
 * instead of everything, especially because we have an explicit exposing(..) for doing it.
 *)
let exposing_to_sigmask ~default (exposing_opt : P.exposing option) : R.sigmask = 
  match exposing_opt with
  | None -> default
  | Some exposing -> 
    match exposing with 
    | P.Any -> sigmask_any
    | P.Idents ids ->
      let fcon exposed_tycon = 
        match exposed_tycon with 
        | P.AbsTyCon con 
        | P.TyCon con ->    
          let tycon : R.Sig.sigmask_tycon = 
            R.Sig.Enumerated (Node.map_elem as_tyconid con, []) 
          in Some tycon
        | P.Var _ -> None
      in
      let fvar exposed_var =
        match exposed_var with 
        | P.AbsTyCon _ | P.TyCon _  -> None
        | P.Var v -> Some v
      in
      let tycons = List.filter_map ids ~f:fcon
      and vals   = List.filter_map ids ~f:fvar in
      R.Sig.Enumerated (tycons, vals)


let resolve_imports ~export_dict (imports : P.import list) : (R.path' * R.sigmask) list =
  List.map imports ~f:(
    fun (P.Import (path, _, exposing_opt)) ->
      (as_resolved_path path, exposing_to_sigmask ~default:sigmask_none exposing_opt)
  )

let sigt_to_dicst (tycon_map, dcon_map, fvar_map) (path, sigt)  =
  let (R.Sig.Sig (tycons, dcons)) = sigt in
  let (tycon_map', dcon_map') = 
    List.fold tycons ~init:(tycon_map, dcon_map) ~f:(
      fun (tycon_map, dcon_map) (conid, dcons) -> 
        let tycon_map' = Map.add_exn tycon_map ~key:(Node.elem conid) ~data:(Node.elem path) in
        let dcon_map' = 
          List.fold dcons ~init:dcon_map ~f:(
            fun (map : dcon_map) conid -> 
              Map.add_exn map ~key:(Node.elem conid) ~data:(Node.elem path)
          )
        in (tycon_map', dcon_map')
      )
  in 
  let fvar_map' = List.fold dcons ~init:fvar_map ~f:(
    fun map var -> Map.add_exn map ~key:(Node.elem var) ~data:(Node.elem path)
  )
  in
  (tycon_map', dcon_map', fvar_map')

let imports_to_maps (imports : (R.path' * R.sigmask) list) =
  let f dicts (path, sigmask) = 
    (* If whole program is available, this step would be replaced by looking up signature of other modules
     * then apply the signature mask to those signatures *)
    let sigt = sigmask_minimum_sig sigmask in
    sigt_to_dicst dicts (path, sigt)
  in
  List.fold imports ~init:(Map.empty, Map.empty, Map.empty) ~f

let resolve_var ((_,  _, fvar_map): dicts) (vctx : vctx) (qvar : P.qvar') =
  let (P.QVar (path_opt, var)) = Node.elem qvar in
  match path_opt with 
  | Some path_node -> 
    let fvar = R.FVar (R.Resolved (as_resolved_path path_node, var)) in
    Node.node fvar (Node.attr qvar)
  | None -> 
    match Map.find vctx (Node.elem var) with 
    | Some bvar -> 
      let var' = R.BVar (Node.node bvar (Node.attr var)) in
      Node.node var' (Node.attr qvar)
    | None ->
      match Map.find fvar_map (Node.elem var) with
      | Some path' -> 
        let path_node' = Node.node path' None in
        let fvar = R.FVar (R.Resolved (path_node', var)) in
        Node.node fvar (Node.attr qvar)
      | None -> 
        let fvar = R.FVar (R.Unresolved var) in
        Node.node fvar (Node.attr qvar)

let resolve_dcon ((_, dcon_map, _) : dicts) (dctx : dctx) qcon =
  let (P.QCon (path_opt, con)) = Node.elem qcon in
  match path_opt with 
  | Some path_node -> 
    let fcon = R.FDCon (R.Resolved (as_resolved_path path_node, Node.map_elem as_dconid con)) in
    Node.node fcon (Node.attr qcon)
  | None -> 
    if Set.mem dctx (as_dconid @@ Node.elem con) then
      let con' = R.BDCon (Node.map_elem as_dconid con) in
      Node.node con' (Node.attr qcon)
    else
      match Map.find dcon_map (as_dconid @@ Node.elem con) with
      | Some path' -> 
        let path_node' = Node.node path' None in
        let fdcon = R.FDCon (R.Resolved (path_node', Node.map_elem as_dconid con)) in
        Node.node fdcon (Node.attr qcon)
      | None -> 
        let fdcon = R.FDCon (R.Unresolved (Node.map_elem as_dconid con)) in
        Node.node fdcon (Node.attr qcon)

(* Parser does not support parsing tycons yet *)
let resolve_tycon ((tycon_map, _, _) : dicts) (tctx : tctx) qcon =
  assert false

let rec collect_binders_node (pat : P.pat') : (VarId.t R.node) list =
  match Node.elem pat with 
  | P.Any | P.Unit | P.EmptyList | P.Literal _ -> []
  | P.List pats  -> List.concat (List.map pats ~f:collect_binders_node)
  | P.Tuple pats -> List.concat (List.map pats ~f:collect_binders_node)
  | P.Ctor (_, pats) -> List.concat (List.map pats ~f:collect_binders_node)
  | P.Var v -> [v]

let collect_binders (pat : P.pat') : VarId.t list =
  collect_binders_node pat |> List.map ~f:Node.elem

let bind_binders vctx varids =
  List.fold_map varids ~init:vctx ~f:(
    fun vctx varid ->
      let bvar = Var.fresh ~id:varid () in
      let vctx' = Map.add_exn vctx ~key:varid ~data:bvar in
      (vctx', (varid, bvar))
  )

let rec translate_pat ((_tctx, dctx, vctx) as ctx) (dicts : dicts) (pat : P.pat') =
  let (elem, pos) = Node.both pat in
  let pat_node (pat : R.pat) = Node.node pat pos in
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
    pat_node @@ R.Var (Node.node var pos)

let rec translate_expr ((tctx, dctx, vctx) as ctx) dicts (expr : P.expr') = 
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
  Node.node expr' pos

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
 *)
and translate_decls (tctx, dctx, vctx) ((tycon_map, dcon_map, fvar_map) as dicts) decls =

  let extract_tycons decls = 
    List.filter_map decls ~f:(
      fun (decl : P.decl') ->
        match Node.elem decl with 
        | P.TyCon _ -> assert false 
        | P.Alias _ -> assert false
        | _ -> None
    )
  in
  
  let translate_tycons (tctx, dctx) tycons =
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
  let translate_vals (tctx, dctx, vctx) decls = 
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
        let decl' = R.Fun (None, ((Node.node fbind pos), pats'), e') in
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

let infersig_from_decls (decls : P.decl' list) = 
  let (sig_tycons, sig_vals) = 
    List.fold_right decls ~init:([], []) ~f:(
      fun decl (sig_tycons, sig_vals) -> 
        match Node.elem decl with 
        | P.TyCon _ -> assert false
        | P.Alias _ -> assert false
        | P.Annot _ -> (sig_tycons, sig_vals)
        | P.Pat (pat, _) -> (sig_tycons, collect_binders_node pat @ sig_vals)
        | P.Fun ((var, _), _) -> (sig_tycons, var::sig_vals)
    )
  in R.Sig.Sig (sig_tycons, sig_vals)

(* TODO: perform imported dictionary lookup *)
let resolve ~modpath ~export_dict (t : P.m) : R.m = 
  let (P.Mod (mdecl, imports, decls)) = t in
  let imports = resolve_imports ~export_dict imports in
  let (tycon_map, dcon_map, fvar_map) = imports_to_maps imports in
  let (_, (ids_tycons', ids_vals')) = 
    translate_decls (Map.empty, Set.empty, Map.empty) (tycon_map, dcon_map, fvar_map) decls in
  let m_sigmask = 
    match mdecl with 
    | None -> sigmask_any
    | Some (P.MDecl (_, exposing_opt)) -> exposing_to_sigmask ~default:sigmask_any exposing_opt
  in
  (* Now we filter the organized tycons and values with the module export spec
   * to generate the list of exported identifiers. *)
  {
    modid   = modpath;
    exports = apply_sigmask m_sigmask (infersig_from_decls decls);
    imports = imports; 
    tycons  = ids_tycons';
    vals    = ids_vals';
  }

