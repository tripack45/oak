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

open ElAstRenamed
open Core

module P = ElAstLexical.Syntax
module R = ElAstRenamed.Syntax
module A = ElAstRenamed.Alias

module Path = ElAst.Path

module Map = Core.Map
module Set = Core.Set

module TVarId  = ElAstRenamed.TVarId
module DConId  = ElAstRenamed.DConId
module TyConId = ElAstRenamed.TyConId
module MConId  = ElAstRenamed.MConId

module Rst = Util.PassResult.WarnListErrorList
open Rst.Pervasive

type warn = 
  | MissingModuleSig     of R.path'
  | UnresolvedVarId      of R.varid'
  | UnresolvedDConId     of R.dconid'
  | UnresolvedTyConId    of R.tyconid'
  | VarShadowingInArgs   of VarId.t * (P.pat' * P.pat')
  | ImportShadowingVar   of (R.varid'   * R.path) * R.path
  | ImportShadowingDCon  of (R.dconid'  * R.path) * R.path
  | ImportShadowingTyCon of (R.tyconid' * R.path) * R.path
  | RepeatedTypeVar        of P.tvar' * P.tycon' 

type err =
  | UndefinedTVar        of R.tvarid'

type 'a rslt = ('a, warn, err) Rst.t

let twin id bid =
  Node.node (bid, Node.elem id) (Node.attr id)

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

  type t = 
  {
    tctx : tctx;
    dctx : dctx;
    vctx : vctx;
  }

  let empty : t = 
  {
    tctx  = TyConId.Map.empty;
    dctx  = DConId.Map.empty;
    vctx  = VarId.Map.empty;
  }
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
  type tctx  = TyCon.t TyConId.Map.t
  type dctx  = DCon.t  DConId.Map.t
  type vctx  = Var.t   VarId.Map.t
  type tvctx = TVar.t  TVarId.Map.t

  type t = 
  {
    tctx  : tctx;
    dctx  : dctx;
    vctx  : vctx;
    tvctx : tvctx;
  }

  let empty_tctx = TyConId.Map.empty
  let empty_dctx = DConId.Map.empty
  let empty_vctx = VarId.Map.empty
  let empty_tvctx = TVarId.Map.empty

  let empty : t = 
  {
    tctx  = TyConId.Map.empty;
    dctx  = DConId.Map.empty;
    vctx  = VarId.Map.empty;
    tvctx = TVarId.Map.empty;
  }

  let bind ctx fresh id prefix = 
    let t = fresh ~id:prefix () in
    let id' = Node.node (t, Node.elem id) (Node.attr id) in
    let ctx' = Map.set ctx ~key:id ~data:id' in
    (ctx', id')
end

type amap = (R.varid' * P.typ') VarId.Map.t

let lookup_binder ctx id_node : ('binder, 'id) R.twin' =
  let (id, pos) = Node.both id_node in
  let binder = Map.find_exn ctx id in
  Node.node (binder, id) pos

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
             

let resolve_var fvar_map vctx (qvar : P.qvar') =
  resolve_ident fvar_map vctx qvar ~unpack:(fun (P.QVar (p, v)) -> (p, v)) 
                                   ~fwarn:(fun varid' -> UnresolvedVarId varid')

let resolve_dcon dcon_map dctx (qcon : P.qdcon') =
  resolve_ident dcon_map dctx qcon ~unpack:(fun (P.QDCon (p, c)) -> (p, c)) 
                                   ~fwarn:(fun dconid' -> UnresolvedDConId dconid')

let resolve_tycon tycon_map tctx (qtycon : P.qtycon') =
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
let sigt_to_dicst ({tctx=tycon_map; dctx=dcon_map; vctx=fvar_map} : CtxOpen.t) (path, sigt)  =
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
  ok {
    CtxOpen.tctx = tycon_map'; 
    CtxOpen.dctx = dcon_map'; 
    CtxOpen.vctx = fvar_map';
  }

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

let collect_binders' (pat : P.pat') =
  let rec f acc p =
    match (Node.elem p : P.pat) with 
    | P.Any 
    | P.Unit 
    | P.EmptyList 
    | P.Literal _      -> acc
    | P.List pats      -> List.fold pats ~init:acc ~f
    | P.Tuple pats     -> List.fold pats ~init:acc ~f
    | P.DCon (_, pats) -> List.fold pats ~init:acc ~f
    | P.Var (v, _)     -> Map.add_exn acc ~key:(Node.elem v) ~data:v
  in
  f VarId.Map.empty pat
let collect_binders (pat : P.pat') =
  collect_binders' pat |> Map.key_set

(* General shadowing does not trigger any warning. Although Elm prevents shadowing at all 
 * we allow them in Oak. Shadowing between functions arguments triggers a warning, as it is 
 * probably a user oversight and result in non-intuitive behavior unless the reader carefully
 * reason the semantics. *)
let bind_pats ?prefix vctx pats =
  let var_prefix =  VarId.of_string "x" in
  let varids = List.map pats ~f:(fun pat -> (collect_binders pat, pat)) in
  (* This is evaluated only for the side effects, i.e. the warnings *)
  let* _ = 
    Rst.Seq.fold varids ~init:(ok VarId.Map.empty) ~f:(
      fun seen (varids, pat) -> 
        Rst.Seq.fold (Set.to_list varids) ~init:(ok seen) ~f:(
          fun seen varid ->
            match VarId.Map.find seen varid with 
            | None           -> ok @@ VarId.Map.add_exn seen ~key:varid ~data:pat
            | Some pat' -> 
              let seen' = VarId.Map.add_exn seen ~key:varid ~data:pat in
              warn seen' (VarShadowingInArgs (varid, (pat, pat')))
        )
    )
  in
  let varids' = varids |> List.unzip |> fst |> VarId.Set.union_list in
  let ctx' =
    VarId.Set.fold varids' ~init:vctx ~f:(
      fun vctx varid ->
        let bvar  = Var.fresh ~id:(Option.value prefix ~default:var_prefix) () in
        (* Silently shadows previous definitions *)
        Map.set vctx ~key:varid ~data:bvar
    ) 
  in ok @@ (ctx', varids')

let rec ftv_in_typ typ =
  match (Node.elem typ : P.typ) with 
  | P.Unit 
  | P.TyCon _         -> TVarId.Set.empty 
  | P.TVar tv         -> TVarId.Set.singleton (Node.elem tv)
  | P.Arrow (t1, t2)  -> TVarId.Set.union (ftv_in_typ t1) (ftv_in_typ t2)
  | P.TApp  (t1, t2)  -> TVarId.Set.union (ftv_in_typ t1) (ftv_in_typ t2)
  | P.Tuple ts        -> TVarId.Set.union_list @@ List.map ts ~f:ftv_in_typ 
  | P.Record row      ->
    let ftv_in_fields fields =
        fields |> List.map ~f:snd |> List.map ~f:ftv_in_typ |> TVarId.Set.union_list
    in
    let rec ftv_in_row = function
      | P.RVar rv               -> TVarId.Set.singleton (Node.elem rv)
      | P.Extension (r, fields) -> TVarId.Set.union (ftv_in_row r) (ftv_in_fields fields)
      | P.Fields fields         -> ftv_in_fields fields
    in 
    ftv_in_row row

(* Oak supports type binding towards a pattern instead of just a variable. This creates 
 * ambiguities when we handle type annotations like the following:
 *
 * f0 : a -> Int
 * f0 x = 
 *   let f : a -> a
 *       f t = x
 *    in 0
 * 
 * Code above typechecks for Elm but not for Haskell. Haskell assigns `f` with generalized
 * type "forall a. a -> a" instead of the concrete type variable "a" binded `f0`. Elm on the 
 * other hand believes otherwise. This is a significant deviation in semantics compared to 
 * Haskell (Haskell: all free type variables are considered forall bounded in type annotation).
 * 
 * The Elm approach creates issues when we consider multiple type annotations inside same binder 
 * examples like this:
 * 
 * type T a = V (a -> Int) (a -> Int)
 *
 * f0 : a -> Int
 * g0 : a -> Int
 * 
 * (f0, g0) = ...
 *
 * Specificially, does type variable `a` in both type annotations refers to the same type or 
 * generalized independently. 
 *
 * The sensible thing to do here is to take the approach of OCaml, that we consider all type 
 * variables of the same name appeared in the type annotations for the pattern as the same type 
 * variables. This removes ambiguities when considering which "a" should be put under the scope:
 * 
 * f0 : a -> Int
 * x  : a -> a
 * (x, f0) = ( \x -> x, 
 *             \x -> 
 *                let f :: t -> a
 *                    f _ = x
 *                 in 0
 *           )
 *
 * This piece of code can fail to type check if we assign different type variables to the annotation
 * of x and f0, then added the binding for a in x into the scope instead of that of f0.
 * 
 * In the meantime, there doesn't seem a way to type annotate this thing in Haskell even with 
 * language extention "ScopedTypeVariables"
 *)
let ftv_in_pat pat = 
  let rec f acc pat =
    match (Node.elem pat : P.pat) with 
    | P.Any 
    | P.Unit 
    | P.EmptyList 
    | P.Literal _      -> acc
    | P.List pats      -> List.fold pats ~init:acc ~f
    | P.Tuple pats     -> List.fold pats ~init:acc ~f
    | P.DCon (_, pats) -> List.fold pats ~init:acc ~f
    | P.Var (_, annot) -> 
      match annot with 
      | Some (_, typ)  -> TVarId.Set.union (ftv_in_typ typ) acc
      | None -> acc
  in
  f TVarId.Set.empty pat

let rec translate_pat (ctx : Ctx.t) dicts (pat : P.pat') =
  let (elem, pos) = Node.both pat in
  let ok' pat = ok @@ Node.node pat pos in
  let tr  = translate_pat ctx dicts in
  let trs = Rst.Par.map ~f:tr  in
  match elem with 
  | P.Any            -> ok' R.Any
  | P.Unit           -> ok' A.Pat.Unit
  | P.EmptyList      -> ok' R.EmptyList
  | P.Literal lit    -> ok' @@ A.Pat.Literal lit
  | P.List pats      -> let* pats' = trs pats in ok' @@ A.Pat.List pats' 
  | P.Tuple pats     -> let* pats' = trs pats in ok' @@ A.Pat.Tuple pats'
  | P.Var (v, annot) -> 
    let twin = lookup_binder ctx.vctx v in 
    begin
      match annot with
      | None -> ok' @@ A.Pat.Var (twin, None)
      | Some (var, typ) -> 
        (* This should always succeed since we have checked all annotations are placed on
         * valid binders of this scope *)
        let twin'  = lookup_binder ctx.vctx var in
        let* typ'  = translate_typ (ctx.tctx, ctx.tvctx) dicts typ in
        ok' @@ A.Pat.Var (twin, Some (twin', typ'))
    end
  | P.DCon (qcon, pats) -> 
    let* con'  = resolve_dcon dicts.CtxOpen.dctx ctx.dctx qcon
    and* pats' = Rst.Par.map pats ~f:tr in
    ok' @@ A.Pat.DCon (con', pats')

and translate_typ (tctx, tvctx) dicts (typ : P.typ') =
  let (elem, pos) = Node.both typ in
  let ok' typ' = ok @@ Node.node typ' pos in
  let tr = translate_typ (tctx, tvctx) dicts in
  match elem with 
  | P.TVar v -> 
    begin
      match Map.find tvctx (Node.elem v) with 
      | Some tvar' -> ok' @@ R.TVar (Node.map_elem (fun v -> (tvar', v)) v) 
      | None       -> err @@ UndefinedTVar v
    end
  | P.Unit           -> ok' A.Typ.Unit
  | P.TyCon qcon     -> let* qcon' = resolve_tycon dicts.tctx tctx qcon in ok' (A.Typ.TyCon qcon')
  | P.Arrow (t1, t2) -> let* t1' = tr t1 and* t2' = tr t2 in ok' @@ R.Arrow (t1', t2')
  | P.TApp  (t1, t2) -> let* t1' = tr t1 and* t2' = tr t2 in ok' @@ R.Arrow (t1', t2')
  | P.Tuple ts       -> let* ts' = Rst.Par.map ts ~f:tr   in ok' @@ A.Typ.Tuple ts'
  | P.Record row     -> 
    let translate_field (field, typ) = 
      let+ typ' = tr typ in (field, typ') 
    in
    let rec translate_row row = 
      match row with 
      | P.RVar rvar' -> 
        let rvar' = lookup_binder tvctx rvar' in 
        ok @@ R.RVar rvar' 
      | P.Extension (row, fields) -> 
        let+ row' = translate_row row 
        and+ fields' = Rst.Par.map ~f:translate_field fields in
        A.Typ.Extension (row', fields')
      | P.Fields fields ->
        let+ fields' = Rst.Par.map ~f:translate_field fields in
        R.Fields fields'
    in
    let* row' = translate_row row in 
    ok' @@ A.Typ.Record row'

let rec translate_expr (ctx : Ctx.t) dicts (expr : P.expr') : R.expr' rslt = 
  let tr e = translate_expr ctx dicts e in
  let (expr, (pos, _)) = Node.both expr in
  let ok' e = ok @@ Node.node e pos in
  match expr with 
  | P.LetM (letm, e) ->
    let* (ctx', letm') = translate_letm ctx dicts letm in
    let* e' = translate_expr ctx' dicts e in
    ok' @@ R.LetM (letm', e')
  | P.Case (e, branches)   ->
    let* e' = tr e in
    let* branches' = Rst.Par.map branches ~f:(
      fun (pat, expr) -> 
        let* (vctx', _) = bind_pats ctx.vctx [pat] in
        (* There is no syntax for Elm to annotate types for binders in cases *)
        let* pat'  = translate_pat  {ctx with vctx=vctx'} dicts pat 
        and* expr' = translate_expr {ctx with vctx=vctx'} dicts expr in
        ok (pat', expr')
    ) in 
    ok' @@ R.Case (e', branches')
  | P.Lambda (pats, e) -> 
    let* (vctx', _) = bind_pats ctx.vctx pats in
    (* Elm does not have syntax to annotate type for lambda arguments *)
    let* pats' = Rst.Par.map pats ~f:(translate_pat {ctx with vctx=vctx'} dicts)
    and* e' = translate_expr {ctx with vctx=vctx'} dicts e in
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
  | P.Var v                -> let* v' = resolve_var dicts.vctx ctx.vctx v in ok' @@ R.Var v'
  | P.DCon (qcon, es)      -> 
    let* con' = resolve_dcon dicts.dctx ctx.dctx qcon
    and* es'  = Rst.Par.map es ~f:tr in
    ok' @@ R.DCon (con', es')

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
and translate_letm (ctx : Ctx.t) dicts letm : (Ctx.t * R.let_m) rslt =
  let open Ctx in

  let tycon_prefix = TyConId.of_string "T" in
  let dcon_prefix  = DConId.of_string  "D" in
  let f_prefix     = VarId.of_string   "f" in
  let v_prefix     = VarId.of_string   "v" in
  let tv_prefix    = TVarId.of_string   "α" in

  (* Bind only free type variagles. If a type variable is already in the context, 
    * then it considered bounded and should be skipped. *)
  let bind_ftvs (tvctx : Ctx.tvctx) tvars : Ctx.tvctx = 
    List.fold tvars ~init:tvctx ~f:(
      fun tvctx tvar -> 
        let binder = TVar.fresh ~id:tv_prefix () in
        match Map.add tvctx ~key:tvar ~data:binder with 
        | `Ok tvctx' -> tvctx'
        | `Duplicate -> tvctx
    )
  in

  let bind_typdecl ctx typdecl = 
    match (Node.elem typdecl) with
    | ElAstLexical.Alias.LetM.Alias ((tycon, _), _) ->
      let binder = TyCon.fresh ~id:tycon_prefix () in
      let tctx'  = Map.set ctx.tctx ~key:(Node.elem tycon) ~data:binder in
      { ctx with tctx=tctx' }
    | ElAstLexical.Alias.LetM.TyCon ((tycon, _), ctors) ->
      let binder = TyCon.fresh ~id:tycon_prefix () in
      let tctx'  = Map.set ctx.tctx ~key:(Node.elem tycon) ~data:binder in
      let dctx'  = 
        List.fold ctors ~init:ctx.dctx ~f:(
          fun dctx (dcon, _) ->
            let binder = DCon.fresh ~id:dcon_prefix () in
            Map.set dctx ~key:(Node.elem dcon) ~data:binder
        ) in
      { ctx with tctx=tctx'; dctx=dctx' }
  in

  let bind_valdecl ctx valdecl = 
    match (Node.elem valdecl) with
    | ElAstLexical.Alias.LetM.Pat (pat, _) ->
      let varids = collect_binders pat in
      let vctx' = 
        Set.fold varids ~init:ctx.vctx ~f:(
          fun vctx varid -> 
            let binder = Var.fresh ~id:v_prefix () in
            Map.set vctx ~key:varid ~data:binder
        ) 
      in
      { ctx with vctx=vctx' }
    | ElAstLexical.Alias.LetM.Fun (((f, _), _), _) ->
      let binder = Var.fresh ~id:f_prefix () in
      let vctx'  = Map.set ctx.vctx ~key:(Node.elem f) ~data:binder in
      { ctx with vctx=vctx' }
  in

  let translate_typdecl ctx ctx' typdecl = 
    let ok' v = ok @@ Node.map_elem (Fun.const v) typdecl in
    match (Node.elem typdecl) with
    | ElAstLexical.Alias.LetM.Alias ((tycon, tvars), typ) ->
      let  tvctx' = bind_ftvs ctx'.tvctx (List.map ~f:Node.elem tvars) in
      let tycon' = lookup_binder ctx'.tctx tycon in
      let tvars' = List.map tvars ~f:(lookup_binder tvctx') in
      let* typ'   = translate_typ (ctx.tctx, tvctx') dicts typ in
      ok' @@  A.LetM.Alias ((tycon', tvars'), typ') 
    | ElAstLexical.Alias.LetM.TyCon ((tycon, tvars), ctors) -> 
      let  tvctx' = bind_ftvs ctx'.tvctx (List.map ~f:Node.elem tvars) in
      let  tycon' = lookup_binder ctx'.tctx tycon in
      let  tvars' = List.map tvars ~f:(lookup_binder tvctx') in
      let* ctors' = Rst.Par.map ctors ~f:(
        fun (dcon, typs) -> 
          let dcon' = lookup_binder ctx'.dctx dcon in
          let* typs' = Rst.Par.map ~f:(translate_typ (ctx.tctx, tvctx') dicts) typs in
          ok @@ (dcon', typs')
      ) in
      ok' @@ (R.TyCon ((tycon', tvars'), ctors'))
  in

  let translate_valdecl ctx ctx' valdecl = 
    let ok' v = ok @@ Node.map_elem (Fun.const v) valdecl in
    match (Node.elem valdecl) with
    | ElAstLexical.Alias.LetM.Pat (pat, expr) ->
      let tvctx' = bind_ftvs ctx'.tvctx (ftv_in_pat pat |> Set.to_list) in
      (* The input has already binded pat *)
      let* pat'  = translate_pat  {ctx' with tvctx=tvctx'} dicts pat in
      let* expr' = translate_expr {ctx  with tvctx=tvctx'} dicts expr in
      ok' (R.Pat (pat', expr'))
    | ElAstLexical.Alias.LetM.Fun (((f, annot_opt), pats), expr) ->
      let tvctx' = 
        match annot_opt with 
        | None -> ctx.tvctx
        | Some (_, typ) ->
          bind_ftvs ctx.tvctx (ftv_in_typ typ |> Set.to_list)
      in
      let  f'    = lookup_binder ctx'.vctx f in
      let* (vctx', _) = bind_pats ctx'.vctx pats in
      let* pats' = Rst.Par.map pats ~f:(translate_pat {ctx' with vctx=vctx'; tvctx=tvctx'} dicts)
      and* e'    = translate_expr {ctx with vctx=vctx'; tvctx=tvctx'} dicts expr in 
      match annot_opt with
      | None -> 
        ok' (R.Fun (((f', None), pats'), e'))
      | Some (f_annot, typ) -> 
        let  f_annot' = lookup_binder ctx'.vctx f_annot in
        let* typ' = translate_typ (ctx.tctx, tvctx') dicts typ in
        ok' (R.Fun (((f', Some (f_annot', typ')), pats'), e'))
  in

  let rec loop ctx = function
    | P.Let (vdecl, letm)  -> 
      let ctx' = bind_valdecl ctx vdecl in 
      let* vdecl' = translate_valdecl ctx ctx' vdecl
      and* (ctx', letm') = loop ctx' letm in 
      ok (ctx', R.Let (vdecl', letm'))
    | P.LetRec (vdecls, letm) -> 
      let ctx' = List.fold ~init:ctx ~f:bind_valdecl vdecls in 
      let* vdecls' = Rst.Par.map vdecls  ~f:(translate_valdecl ctx' ctx')
      and* (ctx', letm') = loop ctx' letm in 
      ok (ctx', R.LetRec (vdecls', letm'))
    | P.LetTyp (typdecl, letm) ->
      let ctx' = bind_typdecl ctx typdecl in 
      let* typdecl' = translate_typdecl ctx ctx' typdecl
      and* (ctx', letm') = loop ctx' letm in 
      ok (ctx', R.LetTyp (typdecl', letm'))
    | P.LetTypRec (typdecls, letm) ->
      let ctx' = List.fold ~init:ctx ~f:bind_typdecl typdecls in 
      let* typdecls' = Rst.Par.map typdecls ~f:(translate_typdecl ctx' ctx') 
      and* (ctx', letm') = loop ctx' letm in 
      ok (ctx', R.LetTypRec (typdecls', letm'))
    | P.LetUnit ->
      ok (ctx, R.LetUnit)

  in
  loop ctx letm

let infersig_from_letm (letm : P.let_m) = 
  let rec loop letm = 
    let sig_valdecl val_decl = 
      match (Node.elem val_decl) with
      | P.Fun (((f, _), _), _) -> [f]
      | P.Pat (pat, _) -> collect_binders' pat |> Map.data
    in
    let sig_typdecl typ_decl = 
      match (Node.elem typ_decl) with
      | P.Alias ((tycon, _), _) -> (tycon, [])
      | P.TyCon ((tycon, _), ctors) -> (tycon, List.map ctors ~f:fst)
    in
    match letm with 
    | P.Let (valdecl, letm) -> 
      let (sig_tycon, sig_vals) = loop letm in 
      let sigval' = sig_valdecl valdecl in
      (sig_tycon, sigval' @ sig_vals)
    | P.LetRec (valdecls, letm) -> 
      let (sig_tycon, sig_vals) = loop letm in 
      let sigval' = List.concat_map ~f:sig_valdecl valdecls in
      (sig_tycon, sigval' @ sig_vals)
    | P.LetTyp (typdecl, letm) ->
      let (sig_tycon, sig_vals) = loop letm in 
      let sigtyp' = sig_typdecl typdecl in
      (sigtyp'::sig_tycon, sig_vals)
    | P.LetTypRec (typdecls, letm) -> 
      let (sig_tycon, sig_vals) = loop letm in 
      let sigtyps' = List.map ~f:sig_typdecl typdecls in
      (sigtyps' @ sig_tycon, sig_vals)
    | P.LetUnit -> ([], [])
  in 
  let (sig_tycons, sig_vals) = loop letm in
  R.Sig.Sig (sig_tycons, sig_vals)

let resolve_mod ~modpath mctx (t : P.m) : R.m rslt = 
  let (P.Mod (mdecl, imports, letm)) = t in
  let imports = resolve_imports imports in
  let* dicts = imports_to_sigs mctx imports in
    let* (_, letm') = translate_letm Ctx.empty dicts letm in
    let m_sigmask = 
    match mdecl with 
    | None -> sigmask_any
    | Some (P.MDecl (_, exposing_opt)) -> exposing_to_sigmask ~default:sigmask_any exposing_opt
  in
  (* Now we filter the organized tycons and values with the module export spec
   * to generate the list of exported identifiers. *)
  ok {
    R.modid   = Option.map modpath ~f:(fun m -> Node.node m None);
    R.exports = apply_sigmask m_sigmask (infersig_from_letm letm);
    R.imports = imports; 
    R.letm    = letm';
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