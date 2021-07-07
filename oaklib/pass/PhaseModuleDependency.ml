(* This phase extracts depencies between modules and resolves module import alias *)

open Core
open ElAst
open ElAst.Syntax


module Map = ElAst.Path.Map
type path_dict = path Path.Map.t

module R = Util.PassResult.ErrorList
open R.Pervasive

type error = 
  | ModAliasCollision      of (path' * mcon') * path
  | DuplicatedImport       of path'
  | UndefinedModReference  of path'

type 'a rslt = ('a, error) R.t

let lift_node f node = 
  let* elem' = f (Node.elem node) in
  ok @@ Node.node elem' (Node.attr node)

let expand_path_alias (Mod (mdecl, imports, decl_nodes)) = 
  let preamble_dict = Map.of_alist_exn ElmCore.PathAlias.aliases in
  let* (imported_paths, path_dict) = 
    R.Seq.fold imports ~init:(ok ([], preamble_dict)) ~f:(
      fun (paths, path_map) (Import (path_node, as_con_opt, _)) -> 
        let path = Node.elem path_node in
        let path_map_add map ~key ~data ~on_dup =
          match Map.add map ~key ~data with
          | `Ok map    -> ok map
          | `Duplicate -> on_dup ()
        in
        let+ path_map' = 
          match as_con_opt with
          | Some con_node -> 
            let as_path = Just (Node.elem con_node) in
            let* map' = 
              path_map_add path_map ~key:path ~data:path ~on_dup:(
                fun () -> err @@ DuplicatedImport path_node
              )
            in
            path_map_add map' ~key:as_path ~data:path ~on_dup:(
              fun () ->
                let path' = Map.find_exn path_map as_path in
                err @@ ModAliasCollision ((path_node, con_node), path')
            )
          | None -> 
            path_map_add path_map ~key:path ~data:path ~on_dup:(
              fun () -> err @@ DuplicatedImport path_node
            )
        in 
        ((path :: paths), path_map')
    ) 
  in

  let rec resolve_decl (decl : decl) =
    let resolve_typ'  = lift_node resolve_typ in
    match decl with 
    | Alias ((con_node, var_nodes), typ_node) ->
      let+ typ_node' = resolve_typ' typ_node in
      Alias ((con_node, var_nodes), typ_node')
    | Annot (var_node, typ_node) ->
      let+ typ_node' = resolve_typ' typ_node in
      Annot (var_node, typ_node')
    | Port (var_node, typ_node) ->
      let+ typ_node' = resolve_typ' typ_node in
      Port (var_node, typ_node')
    | TyCon ((con_node, var_nodes), ctors) ->
      let+ ctors' = R.Par.map ctors ~f:(
        fun (con_node, typ_nodes) -> 
          let+ typ_nodes' = R.Par.map ~f:resolve_typ' typ_nodes in
          (con_node, typ_nodes')
      )
      in Alias.Decl.TyCon ((con_node, var_nodes), ctors')
    | Pat (pat_node, expr_node) ->
      let+ pat_node'  = (lift_node resolve_pat) pat_node
      and+ expr_node' = (lift_node  resolve_expr) expr_node in
      Pat (pat_node', expr_node')
    | Fun ((var_node, pat_nodes), expr_node) ->
      let+ pat_nodes' = R.Par.map ~f:(lift_node resolve_pat) pat_nodes
      and+ expr_node' = (lift_node resolve_expr) expr_node in
      Fun ((var_node, pat_nodes'), expr_node')

  and resolve_typ (typ : typ) =
    let resolve_typ' = lift_node resolve_typ in
    match typ with
    | Unit 
    | TVar _ -> ok typ
    | TyCon qtycon_node -> 
      let+ qtycon_node' = (lift_node resolve_qtycon) qtycon_node in
      Alias.Typ.TyCon qtycon_node' 
    | Arrow (t1_node, t2_node) -> 
      let+ t1_node' = resolve_typ' t1_node
      and+ t2_node' = resolve_typ' t2_node in
      Arrow (t1_node', t2_node')
    | TApp (t1_node, t2_node)  -> 
      let+ t1_node' = resolve_typ' t1_node
      and+ t2_node' = resolve_typ' t2_node in
      TApp (t1_node', t2_node')
    | Tuple typ_nodes -> 
      let+ typ_nodes' = R.Par.map typ_nodes ~f:resolve_typ' in
      Alias.Typ.Tuple typ_nodes'
    | Record row -> 
      (resolve_row row) >>| fun row' -> Alias.Typ.Record row'

  and resolve_row (row : row) = 
    match row with 
    | RVar _ -> ok row
    | Extension (r_node, fields) -> 
      let+ r_node' = resolve_row r_node
      and+ fields' = R.Par.map fields ~f:(
        fun (f_node, t_node) -> 
          (lift_node resolve_typ) t_node >>| fun t_node' -> (f_node, t_node')
      ) in
      Extension (r_node', fields')
    | Fields fields ->
      let+ fields' = R.Par.map fields ~f:(
        fun (f_node, t_node) -> 
          (lift_node resolve_typ) t_node >>| fun t_node' -> (f_node, t_node')
      ) in
      Fields fields'

  and resolve_expr (expr : expr) = 
    let resolve_expr'  = lift_node resolve_expr in
    let resolve_exprs' = R.Par.map ~f:resolve_expr' in
    match expr with 
    | OpFunc _
    | Unit
    | Literal _ -> ok expr
    | Let (decl_nodes, e) -> 
      let+ decl_nodes' = R.Par.map ~f:(lift_node resolve_decl) decl_nodes
      and+ e' = resolve_expr' e in
      Let (decl_nodes', e')
    | Case (e, branches)   -> 
      let+ e' = resolve_expr' e
      and+ branches' = R.Par.map branches ~f:(
        fun (pat, expr) -> 
          let+ pat' = (lift_node resolve_pat) pat 
          and+ expr' = resolve_expr' expr in
          (pat', expr')
      )
      in Case (e', branches')
    | Lambda (pats, e)     -> 
      let+ pats' = R.Par.map ~f:(lift_node resolve_pat) pats 
      and+ e' = resolve_expr' e in
      Lambda (pats', e')
    | If (e0, (e1, e2))    -> 
      let+ e0' = resolve_expr' e0
      and+ e1' = resolve_expr' e1
      and+ e2' = resolve_expr' e2 in 
      If (e0', (e1', e2'))
    | App (e0, es)         -> 
      let+ e0' = resolve_expr' e0
      and+ es' = resolve_exprs' es in
      App (e0', es')
    | Infix (op, e1, e2)   -> 
      let+ e1' = resolve_expr' e1 
      and+ e2' = resolve_expr' e2 in 
      Infix (op, e1', e2')
    | Unary (uop, e)   -> 
      let+ e' = resolve_expr' e in 
      Unary (uop, e')
    | Tuple es             -> let+ es' = resolve_exprs' es in Tuple es'
    | List es              -> let+ es' = resolve_exprs' es in List es'
    | Record fields        -> 
      let+ fields' = R.Par.map fields ~f:(
          fun (fid, e) -> let+ e' = resolve_expr' e in (fid, e')
      ) in
      Record fields'
    | Var qvar             -> let+ qv' = (lift_node resolve_qvar) qvar in Alias.Expr.Var qv'
    | Con (qdcon, es)      -> 
      let+ qdcon' = (lift_node resolve_qdcon) qdcon 
      and+ es' = resolve_exprs' es in
      Con (qdcon', es')

  and resolve_pat (pat : pat) =
    let resolve_pat'  = lift_node resolve_pat in
    let resolve_pats' = R.Par.map ~f:resolve_pat' in
    match pat with 
    | Any        
    | Unit       
    | EmptyList  
    | Literal _ 
    | Var _ -> ok pat
    | Cons (p, pn) -> let+ (p', pn') = resolve_pat' p ** resolve_pat' pn 
                      in Alias.Pattern.Cons (p', pn')
    | List pats    -> let+ pats' = resolve_pats' pats in Alias.Pattern.List pats'
    | Tuple pats   -> let+ pats' = resolve_pats' pats in Alias.Pattern.Tuple pats'
    | Record _     -> failwith "Unimplemented: record pat resolve"
    | Con (qdcon, pats) -> 
      let+ pats'  = resolve_pats' pats 
      and+ qdcon' = (lift_node resolve_qdcon) qdcon in
      Alias.Pattern.Con (qdcon', pats')

  and resolve_path_opt path_node_opt =
    match path_node_opt with
    | None -> ok None 
    | Some path' -> 
      let (path, pos) = Node.both path' in
        match Map.find path_dict path with 
      | Some path -> ok  @@ Some (Node.node path pos)
      | None      -> err @@ UndefinedModReference path'

  and resolve_qtycon (QTyCon (path_node_opt, con_node)) =
    let+ path' = resolve_path_opt path_node_opt in
    QTyCon (path', con_node)

  and resolve_qdcon (QDCon (path_node_opt, con_node)) =
    let+ path' = resolve_path_opt path_node_opt in
    QDCon (path', con_node)

  and resolve_qvar (QVar (path_node_opt, var_node)) =
    let+ path' = resolve_path_opt path_node_opt in
    QVar (path', var_node)

  in
  let+ decl_nodes' = R.Par.map ~f:(lift_node resolve_decl) decl_nodes in
  (imported_paths, Mod (mdecl, imports, decl_nodes'))

(* Implement algorithm to sort modules by reversed post order.
 * If the mod dependencies does not form a cycle, an list of mods orded by rpo 
 * is returned, otherwise at least one example of cyclic dependency is found are reporetd. 
 *)
let mods_dep_rpo mods = 
  ok mods

let run (mods : (Path.t * m) list) =
  R.run 
    begin
      let* path_expanded_mods = 
        R.Par.map mods ~f:(fun (path, m) -> let+ m' = expand_path_alias m in (path, m'))
      in 
      mods_dep_rpo path_expanded_mods 
    end
  
let dump_errors src errors = 
  let dump_entry err =
    match err with 
    | UndefinedModReference path' -> 
      printf "Module %s referenced at %s has not been imported.\n%s\n\n"
        (ToString.path_to_string path')
        (ToString.pos_to_string (Node.attr path'))
        (Src.Source.lines src (Node.attr path'))
    | ModAliasCollision ((path', mcon'), path) ->
      printf "Unable to alias module %s as %s at %s. Alias already used for %s.\n%s\n\n"
        (ToString.path_to_string path')
        (ToString.mcon_to_string mcon')
        (ToString.pos_to_string (Node.attr path'))
        (ToString.path_to_string (Node.node path (Lexing.dummy_pos, Lexing.dummy_pos)))
        (Src.Source.lines src (Node.attr path'))
    | DuplicatedImport path' ->
      printf "Module %s at %s has been imported earlier.\n%s\n\n"
        (ToString.path_to_string path')
        (ToString.pos_to_string (Node.attr path'))
        (Src.Source.lines src (Node.attr path'))
  in
  List.iteri errors ~f:(
    fun _idx entry -> dump_entry entry
  )
