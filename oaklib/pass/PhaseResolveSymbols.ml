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
 * - Type variables also needs to be resolved as they may appear in type annotations
 * - Row variables needs to be resolved for similar reasons
 *
 * A potential fix for this is to elaborate T a to T { a } when ever we see a row variable at type variable location.
 * This approach does prevent syntatic inference of rvar/tvar variable kind, because f can be applied at one place a 
 * row variable and some other places a type variable. 
 *
 * The analysis can be run either in separate compilation (as described) or in whole-programe mode. With whole program
 * mode, an export dictionary of depended modules must be supplied, and unresolved references will be reported as 
 * errors. Separately compiled module can be later "linked" with export dictionaries. 
 *)

open ElAstResolved
open Core

module P = ElAst.Syntax
module R = ElAstResolved.Syntax
module A = ElAstResolved.Alias

module Path = ElAst.Path

module Map = Core.Map
module Set = Core.Set

module TVarId  = ElAstResolved.TVarId
module DConId  = ElAstResolved.DConId
module TyConId = ElAstResolved.TyConId
module MConId  = ElAstResolved.MConId

module Rst = Util.PassResult.WarnListErrorList
open Rst.Pervasive

type warn = 
  | MissingModuleSig     of R.path'
  | UnresolvedVarId      of R.varid'
  | UnresolvedDConId     of R.dconid'
  | UnresolvedTyConId    of R.tyconid'
  | VarShadowingInArgs   of R.varid' * R.varid'
  | ImportShadowingVar   of (R.varid'   * R.path) * R.path
  | ImportShadowingDCon  of (R.dconid'  * R.path) * R.path
  | ImportShadowingTyCon of (R.tyconid' * R.path) * R.path

type err = 
  | RepeatedBinder        of R.varid'   * (R.varid' * P.pat')
  | VarIdRedefined        of R.varid'   * R.varid' 
  | TyConIdRedefined      of R.tyconid' * R.tyconid'
  | DConIdRedefined       of R.dconid'  * R.dconid'
  | RepeatedAnnotation     of (P.var' * P.typ') * (P.var' * P.typ')
  | DanglingTypeAnnotation of (P.var' * P.typ')

type 'a rslt = ('a, warn, err) Rst.t

(* TyCons and DCons must use separate map because they live in different 
 * namespaces, hence name collisions between them would be bad. Consider this:
 * 
 * import ModuleX exposing ( Con(..) )
 * import ModuleY exposing ( TyCon(Con) )
 *
 * In this example uses of Con can either be ModuleX.Con or ModuleY.Con, depending
 * whether Con is being used as a TyCon or a DCon
 *)
module CtxOpen =
struct
  type tctx = R.path TyConId.Map.t
  type dctx = R.path DConId.Map.t
  type vctx = R.path VarId.Map.t

  type t = tctx * dctx * vctx

  let empty = (TyConId.Map.empty, DConId.Map.empty, VarId.Map.empty)
end

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

module Ctx =
struct
  type tctx = TyCon.t TyConId.Map.t
  type dctx = DCon.t  DConId.Map.t
  type vctx = Var.t   VarId.Map.t

  type t = tctx * dctx * vctx

  let empty_tctx = TyConId.Map.empty
  let empty_dctx = DConId.Map.empty
  let empty_vctx = VarId.Map.empty
  let empty : t = (empty_tctx, empty_dctx, empty_vctx)
end

type amap = (R.varid' * P.typ') VarId.Map.t

let lookup_binder ctx id_node : ('binder, 'id) R.twin' rslt =
  let (id, pos) = Node.both id_node in
  let binder = Map.find_exn ctx id in
  ok @@ Node.node (binder, id) pos

let as_resolved_path (path : P.path') : R.path' =
  Node.map_attr Option.some path

let resolve_ident ~unpack ~fwarn map ctx qid =
  let ((path_opt, id), pos) = Node.both qid |> Tuple.T2.map_fst ~f:unpack in
  match path_opt with 
  | Some path_node -> 
    let fvar = R.Free (as_resolved_path path_node, id) in
    ok @@ Node.node fvar pos
  | None -> 
    let (idid, pos') = Node.both id in
    match Map.find ctx idid with 
    | Some var -> 
      let twin = Node.node (var, idid) pos' in
      ok @@ Node.node (R.Bind twin) pos
    | None ->
      match Map.find map idid with
      | Some path' -> 
        let path_node' = Node.node path' None in
        ok @@ Node.node (R.Free (path_node', id)) pos
      | None -> 
        warn (Node.node (R.Unresolved id) pos) (fwarn id)
             

let resolve_var ((_,  _, fvar_map): CtxOpen.t) vctx (qvar : P.qvar') =
  resolve_ident fvar_map vctx qvar ~unpack:(fun (P.QVar (p, v)) -> (p, v)) 
                                   ~fwarn:(fun varid' -> UnresolvedVarId varid')

let resolve_dcon ((_,  dcon_map, _): CtxOpen.t) dctx (qcon : P.qdcon') =
  resolve_ident dcon_map dctx qcon ~unpack:(fun (P.QDCon (p, c)) -> (p, c)) 
                                   ~fwarn:(fun dconid' -> UnresolvedDConId dconid')

let resolve_tycon ((tycon_map,  _, _): CtxOpen.t) tctx (qtycon : P.qtycon') =
  resolve_ident tycon_map tctx qtycon ~unpack:(fun (P.QTyCon (p, c)) -> (p, c)) 
                                      ~fwarn:(fun tyconid' -> UnresolvedTyConId tyconid')

(* Applies a sigmask to a signature *)
let apply_sigmask (sigmask : R.sigmask) (sigt : R.sigt) : R.sigt =
  match sigmask with 
  | R.Sig.Any -> sigt
  | R.Sig.Enumerated (mask_tycons, mask_vals) ->
    let (R.Sig.Sig (sig_tycons, sig_vals)) = sigt in
    (* Values are pretty simple: every value appears in the mask must appear
     * in the source signatures, otherwise it's a failure *)
    let vals = 
      let sig_val_map = List.fold sig_vals ~init:Ctx.empty_vctx ~f:(
        fun map node -> Map.add_exn map ~key:(Node.elem node) ~data:node
      ) in
      List.map mask_vals ~f:(fun varid_node -> Map.find_exn sig_val_map (Node.elem varid_node))
    in
    (* Type constructors are more complicated. In addition to previous requirement,
     * if the mask enumerates type constructors, the enumerated type construct most precisely
     * match the list of type constructors in the sig: it is not legal to expose only a subset
     * of type constructors of a type. *)
    let tycons = 
      let sig_tycon_map = List.fold sig_tycons ~init:Ctx.empty_tctx ~f:(
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
 *
 * TODO: verify validity of sigmasks
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
            R.Sig.Enumerated (con, []) 
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


let resolve_imports (imports : P.import list) : (R.path' * R.sigmask) list =
  let mod_imports = 
    List.map imports ~f:(
      fun (P.Import (path, _, exposing_opt)) ->
        (as_resolved_path path, exposing_to_sigmask ~default:sigmask_none exposing_opt)
    )
  in 
  let preambles = 
    List.map ~f:(fun (p, sigmask) -> (Node.node p None, sigmask)) ElmCore.Preamble.preambles in
  preambles @ mod_imports

(* TODO:
 * This function assumes a valid signature. Signatures derived from modules are guaranteed to be valid, 
 * while signatures extracted from sigmasks does not guaranteed to be valid. *)
let sigt_to_dicst (tycon_map, dcon_map, fvar_map) (path, sigt)  =
  let (R.Sig.Sig (tycons, vals)) = sigt in
  let set_find map ~key ~data =
    let prev = Map.find map key in
    let map' = Map.set map ~key:key ~data:data in
    match prev with 
    | Some d -> `Duplicate (map', d)
    | None   -> `Ok map'
  in
  let* (tycon_map', dcon_map') = 
    Rst.Seq.fold tycons ~init:(ok @@ (tycon_map, dcon_map)) ~f:(
      fun (tycon_map, dcon_map) (conid, dcons) -> 
        let* tycon_map' =
          match set_find tycon_map ~key:(Node.elem conid) ~data:(Node.elem path) with
          | `Ok m' -> ok m'
          | `Duplicate (m', dup) -> 
            warn m' (ImportShadowingTyCon ((conid, Node.elem path), dup))
        and* dcon_map' = 
          Rst.Seq.fold dcons ~init:(ok dcon_map) ~f:(
            fun (map : CtxOpen.dctx) conid -> 
              match set_find map ~key:(Node.elem conid) ~data:(Node.elem path) with
              | `Ok m' -> ok m'
              | `Duplicate (m', dup) -> 
                warn m' (ImportShadowingDCon ((conid, Node.elem path), dup))
          )
        in
        ok (tycon_map', dcon_map')
      )
  and* fvar_map' = 
    Rst.Seq.fold vals ~init:(ok fvar_map) ~f:(
      fun map var -> 
        match set_find map ~key:(Node.elem var) ~data:(Node.elem path) with
        | `Ok m' -> ok m'
        | `Duplicate (m', dup) -> warn m' (ImportShadowingVar ((var, Node.elem path), dup))
    )
  in
  ok (tycon_map', dcon_map', fvar_map')

let imports_to_sigs mctx (imports : (R.path' * R.sigmask) list) =
  Rst.Seq.fold imports ~init:(ok CtxOpen.empty) ~f:(
    fun dicts (path, sigmask) -> 
      let* sigt = 
        match Map.find mctx (Node.elem path) with 
        | Some sigt -> ok @@ apply_sigmask sigmask sigt
        | None -> 
          let r = (sigmask_minimum_sig sigmask) in
          let w = MissingModuleSig path in
          warn r w
      in
      sigt_to_dicst dicts (path, sigt)
    )

let collect_binders (pat : P.pat') : R.varid' list rslt =
  let rec f acc p =
    match (Node.elem p : P.pat) with 
    | P.Any 
    | P.Unit 
    | P.EmptyList 
    | P.Literal _     -> ok acc
    | P.List pats     -> Rst.Seq.fold pats ~init:(ok acc) ~f
    | P.Tuple pats    -> Rst.Seq.fold pats ~init:(ok acc) ~f
    | P.Con (_, pats) -> Rst.Seq.fold pats ~init:(ok acc) ~f
    | P.Var v -> 
      let eq v1 v2 = ElAst.VarId.compare (Node.elem v1) (Node.elem v2) = 0 in
      match List.find acc ~f:(eq v) with 
      | None     -> ok  @@ v::acc
      | Some dup -> err @@ RepeatedBinder (v, (dup, pat))
  in
  f [] pat

(* General shadowing does not trigger any warning. Although Elm prevents shadowing at all 
 * we allow them in Oak. Shadowing between functions arguments triggers a warning, as it is 
 * probably a user oversight and result in non-intuitive behavior unless the reader carefully
 * reason the semantics. *)
let bind_pats ?prefix vctx pats =
  let var_prefix = VarId.of_string "x" in
  let* varids' = Rst.Par.map_then pats ~fmap:collect_binders ~fthen:List.concat in
  (* This is evaluated only for the side effects, i.e. the warnings *)
  let* _ = Rst.Seq.fold varids' ~init:(ok []) ~f:(
    fun seen varid' -> 
      let eq v1 v2 = VarId.compare (Node.elem v1) (Node.elem v2) = 0 in
      match List.find seen ~f:(eq varid') with 
      | None     -> ok   @@ (varid' :: seen)
      | Some dup -> warn (varid'::seen) (VarShadowingInArgs (varid', dup))
  )
  in
  let ctx' =
    List.fold varids' ~init:vctx ~f:(
      fun vctx varid' ->
        let varid = Node.elem varid' in
        let bvar  = Var.fresh ~id:(Option.value prefix ~default:var_prefix) () in
        (* Silently shadows previous definitions *)
        Map.set vctx ~key:varid ~data:bvar
    ) 
  in ok @@ (ctx', varids')

let rec translate_pat (amap : amap) ((_tctx, dctx, vctx) as ctx) dicts (pat : P.pat') =
  let (elem, pos) = Node.both pat in
  let ok' pat = ok @@ Node.node pat pos in
  let tr  = translate_pat amap ctx dicts in
  let trs = Rst.Par.map ~f:tr  in
  match elem with 
  | P.Any         -> ok' R.Any
  | P.Unit        -> ok' A.Pat.Unit
  | P.EmptyList   -> ok' R.EmptyList
  | P.Literal lit -> ok' @@ A.Pat.Literal lit
  | P.List pats   -> let* pats' = trs pats in ok' @@ A.Pat.List pats' 
  | P.Tuple pats  -> let* pats' = trs pats in ok' @@ A.Pat.Tuple pats'
  | P.Var v       -> 
    let* twin = lookup_binder vctx v in (
      match Map.find amap (Node.elem v) with
      | None -> ok' @@ A.Pat.Var (twin, None)
      | Some (var, typ) -> 
        (* This should always succeed since we have checked all annotations are placed on
         * valid binders of this scope *)
        let* twin'  = lookup_binder vctx var
        and* typ'  = translate_typ ctx dicts typ in
        ok' @@ A.Pat.Var (twin, Some (twin', typ'))
    )
  | P.Con (qcon, pats) -> 
    let* con'  = resolve_dcon dicts dctx qcon
    and* pats' = Rst.Par.map pats ~f:tr in
    ok' @@ A.Pat.Con (con', pats')

and translate_typ ((tctx, _, _) as ctx) dicts (typ : P.typ') =
  let (elem, pos) = Node.both typ in
  let ok' typ' = ok @@ Node.node typ' pos in
  let tr = translate_typ ctx dicts in
  match elem with 
  | P.TVar v         -> ok' @@ R.TVar v
  | P.Unit           -> ok' A.Typ.Unit
  | P.TyCon qcon     -> let* qcon' = resolve_tycon dicts tctx qcon in ok' (A.Typ.TyCon qcon')
  | P.Arrow (t1, t2) -> let* t1' = tr t1 and* t2' = tr t2 in ok' @@ R.Arrow (t1', t2')
  | P.TApp  (t1, t2) -> let* t1' = tr t1 and* t2' = tr t2 in ok' @@ R.Arrow (t1', t2')
  | P.Tuple ts       -> let* ts' = Rst.Par.map ts ~f:tr   in ok' @@ A.Typ.Tuple ts'
  | P.Record row     -> 
    let translate_field (field, typ) = 
      let+ typ' = tr typ in (field, typ') 
    in
    let rec translate_row row = 
      match row with 
      | P.RVar rvar' -> ok @@ R.RVar rvar'
      | P.Extension (row, fields) -> 
        let+ row' = translate_row row 
        and+ fields' = Rst.Par.map ~f:translate_field fields in
        R.Extension (row', fields')
      | P.Fields fields ->
        let+ fields' = Rst.Par.map ~f:translate_field fields in
        R.Fields fields'
    in
    let* row' = translate_row row in 
    ok' @@ A.Typ.Record row'

let rec translate_expr (((tctx, dctx, vctx) as ctx) : Ctx.t) dicts (expr : P.expr') : R.expr' rslt = 
  let tr e = translate_expr ctx dicts e in
  let (expr, (pos, _)) = Node.both expr in
  let ok' e = ok @@ Node.node e pos in
  match expr with 
  | P.Let (decls, e) ->
    let* (ctx', (tycons', vals')) = translate_decls ctx dicts decls in
    let* e' = translate_expr ctx' dicts e in
    ok' @@ R.Let ((tycons', vals'), e')
  | P.Case (e, branches)   ->
    let* e' = tr e in
    let* branches' = Rst.Par.map branches ~f:(
      fun (pat, expr) -> 
        let* (vctx', _) = bind_pats vctx [pat] in
        (* There is no syntax for Elm to annotate types for binders in cases *)
        let* pat'  = translate_pat  VarId.Map.empty (tctx, dctx, vctx') dicts pat 
        and* expr' = translate_expr (tctx, dctx, vctx') dicts expr in
        ok (pat', expr')
    ) in 
    ok' @@ R.Case (e', branches')
  | P.Lambda (pats, e) -> 
    let* (vctx', _) = bind_pats vctx pats in
    (* Elm does not have syntax to annotate type for lambda arguments *)
    let* pats' = Rst.Par.map pats ~f:(translate_pat VarId.Map.empty (tctx, dctx, vctx') dicts)
    and* e' = translate_expr (tctx, dctx, vctx') dicts e in
    ok' @@ R.Lambda (pats', e')
  | P.If (e0, (e1, e2)) ->
    let* e0' = tr e0 
    and* e1' = tr e1
    and* e2' = tr e2 in
    ok' @@ R.If (e0', (e1', e2'))
  | P.App (e0, es) ->
    let* e0' = tr e0
    and* es' = Rst.Par.map es ~f:tr in 
    ok' @@ R.App (e0', es')
  | P.Infix (op, e1, e2) ->
    let* e1' = tr e1 
    and* e2' = tr e2 in 
    ok' @@ R.Infix (op, e1', e2')
  | P.OpFunc op            -> ok' @@ R.OpFunc op
  | P.Unit                 -> ok' @@ R.Unit
  | P.Literal l            -> ok' @@ R.Literal l
  | P.Tuple es             -> let* es' = Rst.Par.map es ~f:tr in ok' @@ A.Expr.Tuple es'
  | P.List es              -> let* es' = Rst.Par.map es ~f:tr in ok' @@ A.Expr.List es'
  | P.Record fields        -> 
    let* fields' = Rst.Par.map fields ~f:(
      fun (field, e) -> let+ e' = tr e in (field, e')
    ) in
    ok' @@ R.Record fields'
  | P.Var v                -> let* v' = resolve_var dicts vctx v in ok' @@ R.Var v'
  | P.Con (qcon, es)       -> 
    let* con' = resolve_dcon dicts dctx qcon
    and* es'  = Rst.Par.map es ~f:tr in
    ok' @@ R.Con (con', es')

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
and translate_decls (tctx, dctx, vctx) dicts decls =

  let tycon_prefix = TyConId.of_string "T" in
  let dcon_prefix  = DConId.of_string  "D" in
  let f_prefix     = VarId.of_string   "f" in
  let v_prefix     = VarId.of_string   "v" in

  (* Uniqueness check
   * Out-of-order mutual recursive definitions forbids shadowing between definitions of the 
   * same scope, but there exists no semantic reason preventing definition of sub-scope to 
   * shadow definitions from outer scope. Therefore, defintions are processed in 2 steps:
   *
   * - First, all defintions of same scope are collected and checked for uniqueness. 
   * - Definitions are then added to the context in one go, where conflictions with existing
   *   definitions are resolved by new definitions shadowing previous ones.
   *)
  let* (ts, ds, vs, (annots : amap)) = 
    let eq' f n1 n2  = f (Node.elem n1) (Node.elem (fst n2)) = 0 in
    (* They should ideally be abstracted out but type inference are honestly broken for them *)
    Rst.Seq.fold_right decls ~init:(ok ([], [], [], VarId.Map.empty)) ~f:(
      fun (decl : P.decl') (ts, ds, vs, annots) ->
        match Node.elem decl with 
        | P.Alias ((tcon, _), _) -> 
          let* ts' = 
            match List.find ~f:(eq' TyConId.compare tcon) ts with
            | None     -> ok  @@ (tcon, TyCon.fresh ~id:tycon_prefix ())::ts
            | Some dup -> err @@ TyConIdRedefined (tcon, fst dup)
          in 
          ok (ts', ds, vs, annots)
        | P.TyCon ((tcon, _), ctors) ->
          let* ts' = 
            match List.find ~f:(eq' TyConId.compare tcon) ts with
            | None     -> ok  @@ (tcon, TyCon.fresh ~id:tycon_prefix ())::ts
            | Some dup -> err @@ TyConIdRedefined (tcon, fst dup)
          and* ds' = 
            Rst.Seq.fold_right ctors ~init:(ok ds) ~f:(
              fun (dcon, _) ds -> 
                match List.find ~f:(eq' DConId.compare dcon) ds with 
                | None     -> ok  @@ (dcon, DCon.fresh ~id:dcon_prefix ())::ds 
                | Some dup -> err @@ DConIdRedefined (dcon, fst dup)
            ) 
          in
          ok (ts', ds', vs, annots)
        | P.Annot (varid, typ) -> 
          begin 
            match Map.add annots ~key:(Node.elem varid) ~data:(varid, typ) with
            | `Ok annots' -> ok (ts, ds, vs, annots')
            | `Duplicate  -> 
              let existing = Map.find_exn annots (Node.elem varid) in
              err @@ RepeatedAnnotation ((varid, typ), existing)
          end
        | P.Fun ((f, _), _) -> 
          let* vs' = 
            match List.find ~f:(eq' VarId.compare f) vs with
            | None     -> ok  @@ (f, Var.fresh ~id:f_prefix ())::vs
            | Some dup -> err @@ VarIdRedefined (f, fst dup)
          in
          ok (ts, ds, vs', annots)
        | P.Pat (pat, _) -> 
          let* xs  = collect_binders pat in
          let* vs' = 
            Rst.Seq.fold_right xs ~init:(ok vs) ~f:(
              fun v vs -> 
                match List.find ~f:(eq' VarId.compare v) vs with 
                | None     -> ok  @@ (v, Var.fresh ~id:v_prefix ())::vs
                | Some dup -> err @@ VarIdRedefined (v, fst dup)
            ) 
          in
          ok (ts, ds, vs', annots)
    )
  in

  (* Checks that type annotations annotates some binder of current scope and only binder
   * of the current scope. If a binder references a non-existing binder this would not be a
   * fatal flaw as it does not impact program dynamic behavior. However, the type may be malformed
   * and we won't know that until type checking. Since we match type annotations to thier binders 
   * we have to throw a way unmatched type annotations, leaving us the possibility of accepting a
   * program with malformed type annotations. To prevent this from happening, we reject dangling 
   * annotations immediately.
   * 
   * This would not be a problem for Elm programs as Elm compiler requires type annotaitions to directly
   * precede their respective definitions. *)
  let* _ = 
    Rst.Par.map (Map.keys annots) ~f:(fun k -> 
      if List.exists vs ~f:(fun (id, _) -> VarId.compare (Node.elem id) k = 0) then 
        ok true
      else
        err @@ DanglingTypeAnnotation (Map.find_exn annots k)
    )
  in

  let ((tctx', dctx', vctx') as ctx') = 
    let set_list ctx rs = 
      let set ctx twin = 
        Map.set ctx ~key:(Node.elem (fst twin)) ~data:(snd twin) 
      in
      List.fold rs ~init:ctx ~f:set
    in
    (set_list tctx ts, set_list dctx ds, set_list vctx vs)
  in

  let* (tycons', decls') = 
    Rst.Par.map_then decls 
      ~fthen:(
        List.fold_right ~init:([], []) ~f:(
          (* TODO: Add an extra step to attach type annotations to their coresponding defintions
          * Right now this translation simply throws away all type annotations, because the parse
          * does not support it for now. *)
          fun decl (tycons, vals) ->
            match decl with 
            | `TyCon t  -> (t::tycons, vals)
            | `Val   v  -> (tycons, v::vals) 
            | `Annot    -> (tycons, vals) 
        )
      )
      ~fmap:(
        fun decl ->
          match Node.elem decl with
          | P.Alias ((con, tvars), typ) ->
            let* con' = lookup_binder tctx' con
            and* typ' = translate_typ ctx' dicts typ in
            ok @@ `TyCon (R.Alias ((con', tvars), typ'))
          | P.TyCon ((con, tvars), ctors) ->
            let* con' = lookup_binder tctx' con
            and* ctors' = Rst.Par.map ctors ~f:(
              fun (dcon, typs) -> 
                let* dcon' = lookup_binder dctx' dcon
                and* typs' = Rst.Par.map ~f:(translate_typ ctx' dicts) typs in
                ok @@ (dcon', typs')
            ) in
            ok @@ `TyCon (R.TyCon ((con', tvars), ctors'))
          | P.Annot (_id, _typ) -> 
            (* Type annotations have been processe in their respective binder locations. *)
            ok `Annot
          | P.Fun ((var_node, pats), e) -> 
            let* (vctx'', _) = bind_pats vctx' pats in
            let ctx'' = (tctx', dctx', vctx'') in
            let* pats' = Rst.Par.map pats ~f:(translate_pat annots ctx'' dicts)
            and* f'    = lookup_binder vctx'' var_node 
            and* e'    = translate_expr ctx'' dicts e in (
              match Map.find annots (Node.elem var_node) with
              | None -> 
                ok @@ `Val (R.Fun ((f', None), pats', e'))
              | Some (f_annot, typ) -> 
                let* f_annot' = lookup_binder vctx'' f_annot 
                and* typ' = translate_typ ctx' dicts typ in
                ok @@ `Val (R.Fun ((f', Some (f_annot', typ')), pats', e'))
            )
          | P.Pat (pat, e) -> 
            (* The input has already binded pat *)
            let* e'   = translate_expr ctx' dicts e
            and* pat' = translate_pat  annots ctx' dicts pat in 
            ok @@ `Val (R.Pat (pat', e'))
      ) 
  in
  (* Returns 
   * - The contexts for expression under the declared scope 
   * - The translated declarations
   *)
  ok (ctx', (tycons', decls'))

let infersig_from_decls (decls : P.decl' list) = 
  let (sig_tycons, sig_vals) = 
    List.fold_right decls ~init:([], []) ~f:(
      fun decl (sig_tycons, sig_vals) -> 
        match Node.elem decl with 
        | P.TyCon ((tyconid, _), ctors) -> 
          let dconids' = List.map ctors ~f:fst in
          let sig_tycons' = (tyconid, dconids')::sig_tycons in
          (sig_tycons', sig_vals)
        | P.Alias ((con, _), _) -> 
          let sig_tycons' = (con, [])::sig_tycons in
          (sig_tycons', sig_vals)
        | P.Annot _ -> (sig_tycons, sig_vals)
        | P.Fun ((var, _), _) -> (sig_tycons, var::sig_vals)
        | P.Pat (pat, _) -> 
          (* When we extract sigs from decls, decls would have been 
           * translated therefore checked for repeated bindings. *)
          match run @@ collect_binders pat with
          | Ok (binders, _) ->  (sig_tycons, binders @ sig_vals)
          | Error _ -> assert false
    )
  in R.Sig.Sig (sig_tycons, sig_vals)

let resolve_mod ~modpath mctx (t : P.m) : R.m rslt = 
  let (P.Mod (mdecl, imports, decls)) = t in
  let imports = resolve_imports imports in
  let* (tycon_map, dcon_map, fvar_map) = imports_to_sigs mctx imports in
  let* (_, (tycons', vals')) = translate_decls Ctx.empty (tycon_map, dcon_map, fvar_map) decls in
  let m_sigmask = 
    match mdecl with 
    | None -> sigmask_any
    | Some (P.MDecl (_, exposing_opt)) -> exposing_to_sigmask ~default:sigmask_any exposing_opt
  in
  (* Now we filter the organized tycons and values with the module export spec
   * to generate the list of exported identifiers. *)
  ok {
    R.modid   = Option.map modpath ~f:(fun m -> Node.node m None);
    R.exports = apply_sigmask m_sigmask (infersig_from_decls decls);
    R.imports = imports; 
    R.tycons  = tycons';
    R.vals    = vals';
  }

let run mods = 
  Rst.run 
    begin
      let mctx = Path.Map.of_alist_exn (ElmCore.SigResolved.sigs) in
      Rst.Seq.folding_map mods ~init:(ok mctx) ~f:(
        fun mctx (path, m) ->
          let+ m' = resolve_mod ~modpath:(Some path) mctx m in
          (* Multiple definitions of the same mod should have been caught by module path 
           * resolution phase *)
          let mctx' = Map.add_exn mctx ~key:path ~data:m'.R.exports in
          (mctx', m')
      )
    end