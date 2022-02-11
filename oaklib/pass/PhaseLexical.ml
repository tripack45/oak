(* PhaseLexical.ml
 * 
 * This phase restores the lexical relations between definitions. In particular:
 * 
 * - It analyzes dependencies between definitions of same scope. This includes dependencies between
 *   values, functions, types, type alias and type annotations of the same scope.
 * - It re-arrange them according to dependencies, putting dependent difinitions under the scope of
 *   other definitons. 
 * - It detects both self and mutual recursiveness, and generate either "let" or "letrec" bindings
 *   accordingly. 
 *
 * Restoring lexical relations greatly simplifies symbol resolution and is required by type checking
 *
 * While performing said analysis/transformation this phase also attaches type annotations to their 
 * corresponding binders. 
 *
 * The file is organized as follows:
 *
 * - annotate_* functions inserts annotations into binders, and collects free variables mentioned in
 *   those type annotations. 
 * - extract_* functions extract free identifiers (free variables, free tycons, free dcons) from argument.
 * - lexical_* function performs the lexical transformation to its argument, while extracting free ids 
 *
 * The most important work is done in "lexical_decls". This functions is organzied as follows:
 * 
 * - It performs a linear scan through all definitions and collects type annotations into a map. 
 * - It preforms a secon linear scan through all definitions, giving each definition a vertex in dependency 
 *   graph, and generate two maps:
 *   - A map (decl_map) from the vertex to 1) the original definition for error reporting 2) the 
 *     set of free ids its definitions relies on, and 3) the "transformed" sub expresssion.
 *   - A map (binder_map) that maps free idenfifiers to the vertex of the defining defintions. 
 *
 * Free identifiers are called "uses" as in "uses of a definition". This is needed because one single definition
 * can define a number of identifier (e.g. type definition defines both the TyCon and a few DCons, pattern def 
 * defines a range of variables).
 *
 * With both maps we reconstructs the dependencies between definitions and solves it with a strongly connected 
 * component algorithm, which we use to construct the lexical letm. 
 *)

open Core

module E  = ElAst.Syntax
module EA = ElAst.Alias
module L  = ElAstLexical.Syntax
module LA = ElAstLexical.Alias

(* Defintions that are identical between ElAst and ElAstLexical *)
open ElAstLexical.Shared
open ElAstLexical.Syntax.Shared

(* Result *)
module R = Util.PassResult.ErrorList
open R.Pervasive

type err = 
  | RepeatedBinder         of E.pat' * var
  | VarIdRedefined         of var'   * var' 
  | TyConIdRedefined       of tycon' * tycon'
  | DConIdRedefined        of (dcon' * tycon') * tycon'
  | RepeatedAnnotation     of (var'  * typ') * (var' * typ')
  | DanglingTypeAnnotation of (var'  * typ')
  | RepeatedPortAnnotation of var'   * var'

type 'a rslt = ('a, err) R.t

(* Use.t allows us to treat all 3 types of identifiers uniformly, allowing us
 * to have single "unified" context. *) 
module Use = 
struct
  open! struct 
    let compare_var   = VarId.compare
    let sexp_of_var   = VarId.sexp_of_t
    let var_of_sexp   = VarId.t_of_sexp

    let compare_tycon = TyConId.compare
    let sexp_of_tycon = TyConId.sexp_of_t
    let tycon_of_sexp = TyConId.t_of_sexp

    let compare_dcon = DConId.compare
    let sexp_of_dcon = DConId.sexp_of_t
    let dcon_of_sexp = DConId.t_of_sexp
  end

  module T = 
  struct
    type t = 
      | IdVar   of var
      | IdTyCon of tycon
      | IdDCon  of dcon
    [@@deriving compare, sexp]
  end

  include T
  include Core.Comparable.Make(T)
end

let empty_fis     = Use.Set.empty
let merge_fis     = Use.Set.union_list

(* Written in this fashion allows all sets to share those two operators *)
let (<+>) fix fiy = Set.union fix fiy
let (<->) fix fiy = Set.diff  fix fiy

let vars_as_fis fvars     = Use.Set.map fvars   ~f:(fun v -> Use.IdVar v)
let tycons_as_fis ftycons = Use.Set.map ftycons ~f:(fun v -> Use.IdTyCon v)
let dcons_as_fis fdcons   = Use.Set.map fdcons  ~f:(fun v -> Use.IdDCon v)

open Util.Map

(* Defintions for lexical_decls *)
module LexicalDecl =
struct
  open Util.Graph

  module Decl = 
  struct
    type t = Vertex.t
    module Map = Vertex.Map
    module Set = Vertex.Set
  end

  (* This  *)
  type decl_data = 
  {
    deps  : Use.Set.t; (* Dependencies of a declaration *)
    decl  : E.decl;   (* The source declaration *)
    (* The lexical declaration *)
    decl' : [ `TyCon of L.typ_decl' 
            | `Val   of L.val_decl'
            ];
  }

  type acc = (decl_data Decl.Map.t) * (Decl.t Use.Map.t)

  let acc_empty : acc = (Decl.Map.empty, Use.Map.empty)

  let dup_var_in_pat pat varid = 
    let rec find_opt pat = 
      match Node.elem pat with
      | EA.Pattern.Any
      | EA.Pattern.Unit
      | EA.Pattern.Literal _ 
      | EA.Pattern.EmptyList     -> None
      | EA.Pattern.Var v         -> Option.some_if (VarId.equal (Node.elem v) varid) v
      | EA.Pattern.DCon (_, pats) 
      | EA.Pattern.Tuple pats  
      | EA.Pattern.List  pats    -> List.map ~f:find_opt pats |> List.filter_opt |> List.hd
      | EA.Pattern.Cons _
      | EA.Pattern.Record _      -> failwith "Unimplemented"
    in
    Option.value_exn (find_opt pat)

  let scc_component_to_letm dep_graph component letm' =  
    let to_val_decl = function
       | `Val val_decl -> val_decl
       | `TyCon _      -> failwith "Must be val decl"
    in
    let to_typ_decl = function 
       | `TyCon typ_decl -> typ_decl
       | `Val _          -> failwith "Must be typ decl"
    in
    match component with 
    | []     -> failwith "Empty decl component."
    | [decl_v] -> 
      let deps       = Directed.V.adj_exn dep_graph decl_v in
      let {decl'; _} = Directed.V.attr_exn dep_graph decl_v in
      begin
        if Decl.Set.mem deps decl_v then
          match decl' with 
          | `Val decl'   -> L.LetRec ([decl'], letm')
          | `TyCon decl' -> L.LetTypRec ([decl'], letm') 
        else
          match decl' with 
          | `Val decl'   -> L.Let (decl', letm')
          | `TyCon decl' -> L.LetTyp (decl', letm') 
      end
    | decls_vs -> 
      let decls' = 
        List.map decls_vs ~f:(fun v -> (Directed.V.attr_exn dep_graph v).decl') 
      in
      (* It is not possible syntatically to have types that depends on expressions, 
       * therefore no scc component may countain both type and expression definitions. *)
      match List.hd_exn decls' with 
      | `Val _   -> L.LetRec    (List.map decls' ~f:to_val_decl, letm')
      | `TyCon _ -> L.LetTypRec (List.map decls' ~f:to_typ_decl, letm')
end

(* Functions that preforms only identifier extraction *)
let rec extract_typ (typ : typ') : TyConId.Set.t =
  match Node.elem typ with 
  | EA.Typ.Unit 
  | EA.Typ.TVar _         -> TyConId.Set.empty
  | EA.Typ.TApp  (t1, t2) 
  | EA.Typ.Arrow (t1, t2) -> extract_typ t1 <+> extract_typ t2
  | EA.Typ.Tuple ts       -> TyConId.Set.union_list (List.map ts ~f:extract_typ)
  | EA.Typ.Record row     -> extract_row row
  | EA.Typ.TyCon qtycon -> 
    match Node.elem qtycon with 
    | QTyCon (None, c)    -> TyConId.Set.singleton (Node.elem c)
    | QTyCon (Some _, _)  -> TyConId.Set.empty

and extract_row row : TyConId.Set.t = 
  match row with 
  | EA.Typ.RVar _        -> TyConId.Set.empty
  | EA.Typ.Fields fields -> 
    TyConId.Set.union_list (List.map ~f:snd fields |> List.map ~f:extract_typ)
  | EA.Typ.Extension (r, fields) ->
    let fi_fields = List.map ~f:snd fields |> List.map ~f:extract_typ in
    let fi_r      = extract_row r in
    TyConId.Set.union_list (fi_r :: fi_fields)

let rec extract_pat (pat : E.pat') : (DConId.Set.t * VarId.Set.t) rslt =
  let extract_pats pats =
    let* (fis, binds) = R.Par.map pats ~f:extract_pat >>| List.unzip in
    let* binded : VarId.Set.t = 
      R.Seq.fold binds ~init:(ok VarId.Set.empty) ~f:(
        fun binded to_bind -> 
          match VarId.Set.inter binded to_bind |> VarId.Set.choose with
          | None     -> ok  @@ VarId.Set.union binded to_bind
          | Some var -> err @@ RepeatedBinder (pat, var)
      )
    in
    ok (DConId.Set.union_list fis, binded)
  in
  match Node.elem pat with 
  | E.Any           
  | E.Unit          
  | E.EmptyList     
  | E.Literal _     -> ok (DConId.Set.empty, VarId.Set.empty)
  | E.List pats     -> extract_pats pats
  | E.Tuple pats    -> extract_pats pats
  | E.Var v         -> ok (DConId.Set.empty, VarId.Set.singleton (Node.elem v))
  | E.Cons _
  | E.Record _      -> failwith "Unimplemented"
  | E.DCon (c, pats) ->
    let* (dcons, bvs) = extract_pats pats in 
    match Node.elem c with 
    | QDCon (None, dcon) -> ok (DConId.Set.add dcons (Node.elem dcon), bvs)
    | QDCon (Some _, _)  -> ok (dcons, bvs)

(* Functions that annotates letm when provided a map of annotations *)
let annotate_var annots var =
  match Map.find annots (Node.elem var) with 
  | Some (v, t) -> (Some (v, t), extract_typ t)
  | None        -> (None, TyConId.Set.empty)

let annotate_qvar annots qvar =
  match Node.elem qvar with 
  | QVar (None, v)   -> annotate_var annots v
  | QVar (Some _, _) -> (None, TyConId.Set.empty)

let rec annotate_pat annots pat_node = 
  let (pat, pos) = Node.both pat_node in
  let node v = Node.node v pos in
  let annotate_pats pats f = 
    let (pats', ftycons) = List.map ~f:(annotate_pat annots) pats |> List.unzip in
    let ftycon = TyConId.Set.union_list ftycons in
    f pats' ftycon
  in
  match pat with 
  | EA.Pattern.Any            -> (node @@ LA.Pattern.Any,       TyConId.Set.empty)
  | EA.Pattern.Unit           -> (node @@ LA.Pattern.Unit,      TyConId.Set.empty)
  | EA.Pattern.Literal l      -> (node @@ LA.Pattern.Literal l, TyConId.Set.empty)
  | EA.Pattern.EmptyList      -> (node @@ LA.Pattern.EmptyList, TyConId.Set.empty)
  | EA.Pattern.List pats      -> 
    annotate_pats pats (fun pats' ftycons' -> (node @@ LA.Pattern.List pats', ftycons'))
  | EA.Pattern.Tuple pats     -> 
    annotate_pats pats (fun pats' ftycons' -> (node @@ LA.Pattern.Tuple pats', ftycons'))
  | EA.Pattern.DCon (c, pats) ->
    annotate_pats pats (fun pats' ftycons' -> (node @@ LA.Pattern.DCon (c, pats'), ftycons'))
  | EA.Pattern.Var qv         -> 
    let (annot_opt, ftycons) = annotate_var annots qv in 
    (node @@ LA.Pattern.Var (qv, annot_opt), ftycons)
  | EA.Pattern.Cons _
  | EA.Pattern.Record _       -> failwith "Unimplemented"

let annotate_extract_pat annots pat =
  let (pat', ftycons) = annotate_pat annots pat in
  let* (fdcons, binded) = extract_pat pat  in
  ok (pat', dcons_as_fis fdcons <+> tycons_as_fis ftycons, binded)

(* Functions that performs lexical transformations *)
let rec lexical_m (E.Mod (mdecl_opt, imports, decls)) =
  let* (letm, _fi, _binded) = lexical_decls decls in
  ok (L.Mod (mdecl_opt, imports, letm))

and lexical_decls decls =
  let open Util.Graph in
  let open LexicalDecl in
  let* annots = 
    R.Seq.fold decls ~init:(ok VarId.Map.empty) ~f:
      begin
        fun annots decl' ->
          match Node.elem decl' with
          | E.Annot (var, typ) -> 
            begin
              match add_or_dup annots ~key:(Node.elem var) ~data:(var, typ) with
              | `Ok annots'    -> ok annots'
              | `Duplicate dup -> err @@ RepeatedAnnotation ((var, typ), dup)
            end
          | _ -> ok annots 
      end
  in
  let* (decl_map, binder_map) = 
    (* They should ideally be abstracted out but type inference are honestly broken for them *)
    R.Seq.fold decls ~init:(ok acc_empty) ~f:(
      fun ((decl_map, binder_map) : acc) (decl' : E.decl') ->
        let idx = Vertex.fresh () in
        let (decl, pos) = Node.both decl' in
        let node decl'  = Node.node decl' pos in
        match decl with 
        | E.Alias ((tcon, tvars), typ) -> 
          let deps = tycons_as_fis @@ extract_typ typ in
          let decl' = `TyCon (node @@ L.Alias ((tcon, tvars), typ)) in
          let decl_map' = Map.add_exn decl_map ~key:idx ~data:{deps; decl; decl'} in
          let* binder_map = 
            let key = Use.IdTyCon (Node.elem tcon) in 
            match add_or_dup binder_map ~key ~data:idx with
            | `Ok decl_map'  -> ok decl_map'
            | `Duplicate dup -> 
              match (Map.find_exn decl_map dup).decl with 
              | E.Alias ((tcon', _), _)
              | E.TyCon ((tcon', _), _) -> err @@ TyConIdRedefined (tcon, tcon')
              | _ -> failwith "Unreachable."
          in
          ok (decl_map', binder_map)
        | E.TyCon ((tycon, tvars), ctors) ->
          let* (binder_map', ctor_deps) = 
            R.Seq.fold_map ctors ~init:(ok binder_map) ~f:
            begin
              fun binder_map (dcon, typs) -> 
                let ftycons = List.map typs ~f:extract_typ |> TyConId.Set.union_list in
                let key  = Use.IdDCon (Node.elem dcon) in
                match add_or_dup binder_map ~key ~data:idx with 
                | `Ok binder_map' -> ok  @@ (binder_map', ftycons)
                | `Duplicate dup  -> 
                  if Vertex.equal dup idx then
                    (* The defintion originates in current function *)
                    err @@ DConIdRedefined ((dcon, tycon), tycon)
                  else
                    match (Map.find_exn decl_map dup).decl with 
                    | E.TyCon ((tcon', _), _) -> err @@ DConIdRedefined ((dcon, tycon), tcon')
                    | _ -> failwith "Unreachable."
            end
          in
          let deps  = TyConId.Set.union_list ctor_deps |> tycons_as_fis in
          let decl' = `TyCon (node @@ L.TyCon ((tycon, tvars), ctors)) in
          let decl_map' = Map.add_exn decl_map ~key:idx ~data:{deps; decl; decl'} in
          let* binder_map'' =
            let key = Use.IdTyCon (Node.elem tycon) in
            match add_or_dup binder_map' ~key ~data:idx with
            | `Ok binder_map'' -> ok binder_map''
            | `Duplicate dup   -> 
              match (Map.find_exn decl_map dup).decl with 
              | E.Alias ((tcon', _), _)
              | E.TyCon ((tcon', _), _) -> err @@ TyConIdRedefined (tycon, tcon')
              | _ -> failwith "Unreachable."
          in
          ok (decl_map', binder_map'')
        | E.Annot _ -> ok (decl_map, binder_map) (* This has been handled earlier *)
        | Port (v, _typ) -> 
          begin
            match fst @@ annotate_var annots v with
            | Some annot -> err @@ RepeatedPortAnnotation (v, fst annot)
            | None -> failwith "Unimplemented"
          end
        | E.Fun ((f, pats), e) ->
          let (f_annot, f_ftycons) = annotate_var annots f in
          let* (pats', pat_fis, binded_vars) = R.Par.map pats ~f:(annotate_extract_pat VarId.Map.empty) >>| List.unzip3 in
          let* (e', e_fis) = lexical_expr e in
          let deps = 
            let pat_fis = merge_fis pat_fis in
            let f_annot_fis  = tycons_as_fis f_ftycons in
            let binded_fis = vars_as_fis  (VarId.Set.union_list binded_vars) in
            (e_fis <-> binded_fis) <+> pat_fis <+> f_annot_fis
          in
          let decl' = `Val (node @@ L.Fun (((f, f_annot), pats'), e')) in
          let decl_map' = Map.add_exn decl_map ~key:idx ~data:{deps; decl; decl'} in
          let* binder_map' = 
            let key = Use.IdVar (Node.elem f) in
            match add_or_dup binder_map ~key ~data:idx with
            | `Ok decl_map'  -> ok decl_map'
            | `Duplicate dup -> 
              match (Map.find_exn decl_map dup).decl with 
              | E.Fun ((f', _), _) -> err @@ VarIdRedefined (f, f')
              | E.Pat (pat, _)     -> err @@ VarIdRedefined (f, dup_var_in_pat pat (Node.elem f))
              | E.Port (v', _)     -> err @@ VarIdRedefined (f, v')
              | _ -> failwith "Unreachable."
          in
          ok (decl_map', binder_map')
        | E.Pat (pat, e) -> 
          let* (e', e_fi) = lexical_expr e
          and* (pat', pat_fis, binded_vars) = annotate_extract_pat annots pat in
          let deps  = (e_fi <-> vars_as_fis binded_vars) <+> pat_fis in
          let decl' = `Val (node @@ L.Pat (pat', e')) in
          let decl_map' = Map.add_exn decl_map ~key:idx ~data:{deps; decl; decl'} in
          let* binder_map' =
            R.Seq.fold (VarId.Set.to_list binded_vars) ~init:(ok binder_map) ~f:(
              fun binder_map var -> 
                let key = Use.IdVar var in
                match add_or_dup binder_map ~key ~data:idx with 
                | `Ok binder_map' -> ok binder_map'
                | `Duplicate dup  -> 
                  let v = dup_var_in_pat pat var in
                  match (Map.find_exn decl_map dup).decl with 
                  | E.Fun ((f', _), _) -> err @@ VarIdRedefined (v, f')
                  | E.Pat (pat, _)     -> err @@ VarIdRedefined (v, dup_var_in_pat pat var)
                  | E.Port (v', _)     -> err @@ VarIdRedefined (v, v')
                  | _ -> failwith "Unreachable."
            ) 
          in
          ok (decl_map', binder_map')
    )
  in
  let dep_graph = 
    Decl.Map.map decl_map ~f:(
      fun ({deps; _} as attr) -> 
        let deps = 
          Decl.Set.filter_map deps ~f:(
            fun use -> Use.Map.find binder_map use
          )
        in 
        (attr, deps)
      ) 
    |> Directed.of_adj_set_exn
  in
  let solved_decls =
    dep_graph
    |> Directed.StronglyConnectedComponents.scc_topsort
    |> Sequence.to_list
  in
  let letm = List.fold_right solved_decls ~init:L.LetUnit ~f:(scc_component_to_letm dep_graph) in
  let binded_fis = Use.Map.key_set binder_map in
  let fis = 
    let uses = Decl.Map.to_alist decl_map 
               |> List.map ~f:snd
               |> List.map ~f:(fun {deps; _} -> deps)
               |> merge_fis
    in
    uses <-> binded_fis
  in
  (* Check that all type annotations have been assigned *)
  let* _ = 
    let binded_vars = VarId.Set.filter_map binded_fis ~f:(function Use.IdVar v -> Some v | _ -> None) in
    let annotated = VarId.Map.key_set annots in
    let undefined = annotated <-> binded_vars in
    match Set.choose undefined with 
    | None     -> ok ()
    | Some var -> err @@ DanglingTypeAnnotation (Map.find_exn annots var)
  in
  ok (letm, fis, binded_fis)

and lexical_expr expr : (L.expr' * Use.Set.t) rslt = 
  let (expr, pos) = Node.both expr in
  let ok' v fis = ok @@ (Node.node v pos, fis) in
  match expr with 
  | EA.Expr.Let (decls, e) ->
    let* (let_m, fi_m, fi_decl) = lexical_decls decls
    and* (e', fi_e) = lexical_expr e in
    let fi' = (fi_m <+> fi_e) <-> fi_decl in
    ok' (LA.Expr.LetM (let_m, e')) fi'
  | EA.Expr.Case (e, branches) ->
    let* (e', fi) = lexical_expr e 
    and* (branches', fis) = 
      R.Par.map branches ~f:(
        fun (pat, e) -> 
          let* (pat', fi_pat, binded) = annotate_extract_pat VarId.Map.empty pat 
          and* (e', fi_e) = lexical_expr e in
          ok ((pat', e'), (fi_e <+> fi_pat) <-> vars_as_fis binded)
      ) 
      >>| List.unzip
    in
    ok' (LA.Expr.Case (e', branches')) (merge_fis @@ fi::fis)
  | EA.Expr.Lambda (pats, e) ->
    let* (e', e_fi) = lexical_expr e
    (* We cannot call explict_pats because lexical_pats enforces the ristriction 
     * that the same variable is never bound twice in the pattern list. *)
    and* (pats', pat_fis, binds) = R.Par.map pats ~f:(annotate_extract_pat VarId.Map.empty) >>| List.unzip3 in
    let binded = VarId.Set.union_list binds in
    ok' (LA.Expr.Lambda (pats', e')) ((e_fi <-> vars_as_fis binded) <+> merge_fis pat_fis)
  (* Cases that does not need to handle binders *)
  | EA.Expr.If (e0, (e1, e2)) ->
    let* (e0', fi0) = lexical_expr e0
    and* (e1', fi1) = lexical_expr e1
    and* (e2', fi2) = lexical_expr e2 in
    ok' (LA.Expr.If (e0', (e1', e2'))) (fi0 <+> fi1 <+> fi2)
  | EA.Expr.App (e, es) ->
    let* (e', fi) = lexical_expr e
    and* (es', es_fis) = R.Par.map es ~f:lexical_expr >>| List.unzip in
    ok' (LA.Expr.App (e', es')) (merge_fis @@ fi::es_fis)
  | EA.Expr.Infix (op, e1, e2) ->
    let* (e1', fi1) = lexical_expr e1  
    and* (e2', fi2) = lexical_expr e2 in
    ok' (LA.Expr.Infix (op, e1', e2')) (fi1 <+> fi2)
  | EA.Expr.Unary _   -> failwith "Unimplemented"
  | EA.Expr.OpFunc op -> ok' (LA.Expr.OpFunc op) empty_fis
  | EA.Expr.Unit      -> ok' LA.Expr.Unit        empty_fis
  | EA.Expr.Tuple es ->
    let* (es', es_fis) = R.Par.map es ~f:lexical_expr >>| List.unzip in
    ok' (LA.Expr.Tuple es') (merge_fis es_fis)
  | EA.Expr.List es ->
    let* (es', es_fis) = R.Par.map es ~f:lexical_expr >>| List.unzip in
    ok' (LA.Expr.List es') (merge_fis es_fis)
  | EA.Expr.Record fields ->
    let* (fields', fis) =
      R.Par.map fields ~f:(
        fun (field, e) -> 
          let* (e', fis) = lexical_expr e in ok ((field, e'), fis)
      ) >>| List.unzip
    in
    ok' (LA.Expr.Record fields') (merge_fis fis)
  | EA.Expr.Extension _ -> failwith "Unimplemented"
  | EA.Expr.ProjFunc _  -> failwith "Unimplemented"
  | EA.Expr.Project _   -> failwith "Unimplemented"
  | EA.Expr.DCon (qdcon, es) -> 
    let* (expr', es_fis) = R.Par.map es ~f:lexical_expr >>| List.unzip in
    let q_fi = 
      match Node.elem qdcon with 
      | E.QDCon (None, q)   -> Use.Set.singleton @@ Use.IdDCon (Node.elem q)
      | E.QDCon (Some _, _) -> empty_fis
    in
    let fis' = merge_fis (q_fi :: es_fis) in
    ok' (LA.Expr.DCon (qdcon, expr')) fis'
  | EA.Expr.Literal l -> ok' (LA.Expr.Literal l) empty_fis
  | EA.Expr.Var qvar -> 
    let expr' = LA.Expr.Var qvar in
    match Node.elem qvar with 
    | E.QVar (None, v)   -> ok' expr' (Use.Set.singleton @@ Use.IdVar (Node.elem v))
    | E.QVar (Some _, _) -> ok' expr' empty_fis
    
let run (mods : (E.path * E.m) list) =
  R.run 
    begin
      R.Par.map mods ~f:(
        fun (path, m) -> 
          let* m' = lexical_m m in
          ok (path, m')
      )
    end
    
let dump_errors cot errors = 
  let open ElAstLexical in
  let open OTarget in
  let dump_entry err =
    match cot with 
    | DirectSourced.DirectSourced ot -> 
      let printf format = DirectSourced.eprintf ot format in
      let print_src = DirectSourced.wprint_src ot in 
      begin
        match err with 
        | RepeatedBinder (pat, var) ->
          printf (format_of_string "Variable '%s' binded more than once in pattern '%s'.\n")
            (VarId.to_string var) 
            (ElAst.ToString.pat_to_string pat);
          print_src (Node.attr pat)
        | VarIdRedefined (var_new, var_old) ->
          printf "Variable '%s' redefined at '%s':\n"
            (ToString.var_to_string var_new) 
            (ToString.pos_to_string (Node.attr var_new));
          print_src (Node.attr var_new);
          printf "Previous at %s:\n"
            (ToString.pos_to_string (Node.attr var_old));
          print_src (Node.attr var_old)
        | TyConIdRedefined (tycon_new, tycon_old) ->
          printf "TyCon '%s' redefined at '%s':\n"
            (ToString.tycon_to_string tycon_new) 
            (ToString.pos_to_string (Node.attr tycon_new));
          print_src (Node.attr tycon_new);
          printf "Previous at %s:\n"
            (ToString.pos_to_string (Node.attr tycon_old));
          print_src (Node.attr tycon_old);
        | DConIdRedefined ((dcon, tycon_new), tycon_old) ->
          printf "Data constructor '%s' redefined in type '%s' at %s:\n" 
            (ToString.dcon_to_string dcon) 
            (ToString.tycon_to_string tycon_new)
            (ToString.pos_to_string (Node.attr tycon_new));
          print_src (Node.attr tycon_new);
          printf "Previous in TyCon '%s' at %s:\n"
            (ToString.tycon_to_string tycon_old)
            (ToString.pos_to_string (Node.attr tycon_old));
          print_src (Node.attr tycon_old)
        | RepeatedAnnotation ((var_old, _), (var_new, _)) ->
          printf "Variable '%s' associated with more than one annoations. First annotation at %s:\n" 
            (ToString.var_to_string var_old)
            (ToString.pos_to_string (Node.attr var_new));
          print_src (Node.attr var_new);
          printf "Then at %s:\n" 
            (ToString.pos_to_string (Node.attr var_old));
          print_src (Node.attr var_old);
        | DanglingTypeAnnotation (var, _) ->
          printf "Type annotation of variable '%s' at '%s' does not have an associated definition:\n"
            (ToString.var_to_string var)
            (ToString.pos_to_string (Node.attr var));
          print_src (Node.attr var)
        | RepeatedPortAnnotation (var, var_annot) -> 
          printf "Port variable '%s' explicitly annoated. Defined at %s:\n"
            (ToString.var_to_string var)
            (ToString.pos_to_string (Node.attr var));
          print_src (Node.attr var);
          printf "Annotated at %s:\n"
            (ToString.pos_to_string (Node.attr var_annot));
          print_src (Node.attr var_annot)
      end
    | _ -> ()
  in
  List.iteri errors ~f:(
    fun _idx entry -> dump_entry entry
  )
