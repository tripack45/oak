(* This phase extracts depencies between modules and resolves module import alias *)

open Core
open ElAst
open ElAst.Syntax

module Map = Map.Poly

type path_dict = (path, path) Map.t

let expand_path_alias (Mod (mdecl, imports, decl_nodes)) = 
  let (imported_paths, path_dict) = 
    List.fold imports ~init:([], Map.empty) ~f:(
      fun (paths, path_map) (Import (path_node, as_con_opt, _)) -> 
        let path = Node.elem path_node in
        let path_map' = 
          match as_con_opt with
          | None -> Map.add_exn path_map ~key:path ~data:path
          | Some con_node -> 
            let as_path = Just (Node.elem con_node) in
            Map.add_exn path_map ~key:as_path ~data:path
        in 
        ((path :: paths), path_map')
    ) 
  in

  let rec resolve_decl (decl : decl) =
    match decl with 
    | Alias ((con_node, var_nodes), typ_node) ->
      Alias ((con_node, var_nodes), Node.map_elem resolve_typ typ_node)
    | Annot (var_node, typ_node) ->
      Annot (var_node, Node.map_elem resolve_typ typ_node)
    | TyCon ((con_node, var_nodes), ctors) ->
      let ctors' = List.map ctors ~f:(
        fun (con_node, typ_nodes) -> 
          (con_node, List.map ~f:(Node.map_elem resolve_typ) typ_nodes)
      )
      in TyCon ((con_node, var_nodes), ctors')
    | Pat (pat_node, expr_node) ->
      let pat_node'  = Node.map_elem resolve_pat pat_node in
      let expr_node' = Node.map_elem resolve_expr expr_node in
      Pat (pat_node', expr_node')
    | Fun ((var_node, pat_nodes), expr_node) ->
      let pat_nodes' = List.map ~f:(Node.map_elem resolve_pat) pat_nodes in
      let expr_node' = Node.map_elem resolve_expr expr_node in
      Fun ((var_node, pat_nodes'), expr_node')

  and resolve_typ (typ : typ) =
    let rslv = Node.map_elem resolve_typ in
    match typ with
    | Unit 
    | TVar _ -> typ
    | TyCon qtycon_node -> TyCon (Node.map_elem resolve_qtycon qtycon_node)
    | Arrow (t1_node, t2_node) -> Arrow (rslv t1_node, rslv t2_node)
    | TApp (t1_node, t2_node)  -> TApp (rslv t1_node, rslv t2_node)
    | Tuple typ_nodes -> Tuple (List.map typ_nodes ~f:rslv)
    | Record row -> Record (resolve_row row)

  and resolve_row (row : row) = 
    match row with 
    | RVar _ -> row
    | Extension (r_node, fields) -> 
      let r_node' = resolve_row r_node in
      let fields' = List.map fields ~f:(
        fun (f_node, t_node) -> (f_node, Node.map_elem resolve_typ t_node)
      ) in
      Extension (r_node', fields')
    | Fields fields ->
      let fields' = List.map fields ~f:(
        fun (f_node, t_node) -> (f_node, Node.map_elem resolve_typ t_node)
      ) in
      Fields fields'

  and resolve_expr (expr : expr) = 
    let rslv = Node.map_elem resolve_expr in
    match expr with 
    | OpFunc _
    | Unit
    | Literal _ -> expr
    | Let (decl_nodes, e) -> 
      let decl_nodes' = List.map ~f:(Node.map_elem resolve_decl) decl_nodes in
      Let (decl_nodes', rslv e)
    | Case (e, branches)   -> 
      let branches' = List.map branches ~f:(
        fun (pat, expr) -> 
          (Node.map_elem resolve_pat pat, Node.map_elem resolve_expr expr)
      )
      in Case (rslv e, branches')
    | Lambda (pats, e)     -> 
      let pats' = List.map ~f:(Node.map_elem resolve_pat) pats in
      Lambda (pats', rslv e)
    | If (e0, (e1, e2))    -> If (rslv e0, (rslv e1, rslv e2))
    | App (e0, es)         -> App (rslv e0, List.map es ~f:rslv)
    | Infix (op, e1, e2)   -> Infix (op, rslv e1, rslv e2)
    | Tuple es             -> Tuple (List.map es ~f:rslv)
    | List es              -> List (List.map es ~f:rslv)
    | Record fields        -> Record (List.map fields ~f:(Tuple.T2.map_snd ~f:rslv))
    | Var v                -> Var (Node.map_elem resolve_qvar v)
    | Con (qdcon, es)      -> Con (Node.map_elem resolve_qdcon qdcon, List.map ~f:rslv es)

  and resolve_pat (pat : pat) =
    let rslv = Node.map_elem resolve_pat in
    match pat with 
    | Any        
    | Unit       
    | EmptyList  
    | Literal _ 
    | Var _ -> pat
    | List pats   -> List (List.map ~f:rslv pats)
    | Tuple pats  -> Tuple (List.map ~f:rslv pats)
    | Con (qdcon, pats) -> 
      Con (Node.map_elem resolve_qdcon qdcon, List.map ~f:rslv pats)

  and resolve_path_opt path_node_opt =
    let path_node_opt' = 
      Option.map path_node_opt ~f:( 
        Node.map_elem 
          (* Standard library modules are implicitly imported for all modules 
           * hence the missing path could either reference a module in the stdlib, 
           * or could signal missing modules. We take the first option for granted but
           * we need to be able to reject the latter so that type checking down 
           * the line won't fail *)
          (fun path -> Map.find path_dict path |> Option.value ~default:path) 
      )
    in
    path_node_opt'

  and resolve_qtycon (QTyCon (path_node_opt, con_node)) =
    QTyCon (resolve_path_opt path_node_opt, con_node)

  and resolve_qdcon (QDCon (path_node_opt, con_node)) =
    QDCon (resolve_path_opt path_node_opt, con_node)

  and resolve_qvar (QVar (path_node_opt, var_node)) =
    QVar (resolve_path_opt path_node_opt, var_node)

  in
  let decl_nodes' = List.map ~f:(Node.map_elem resolve_decl) decl_nodes in
  (imported_paths, Mod (mdecl, imports, decl_nodes'))