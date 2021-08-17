(* WarnOverArity.ml
 *
 * An analysis pass that checks all values' dimension. Only traverses value space.
 *
 * The following will be checked for their "width":
 * - function application of its argument num
 * - tuple                of its length
 * - list                 of its length
 * - record               of its field num
 * - data constructor     of its binder num
 *)

open Core
open Oaklib
open PassUtil

module R = Util.PassResult.WarnList
open R.Pervasive

open ElAst.Syntax
open ElAst.Node

type site =
  | App    of expr'
  | Tuple  of expr'
  | List   of expr'
  | Record of expr'
  | DCon   of expr'

type violation = (binder * pos) list * site * int

type result = (unit, violation) R.t

module OverDim = struct

  type spec = { 
    fapp   : int;
    tuple  : int;
    list   : int;
    record : int;
    dcon   : int;
  }

  let dim_spec : spec = { 
    fapp   = 8;
    tuple  = 6;
    list   = 12; 
    record = 12; 
    dcon   = 12;
  }

  let run ?(spec=dim_spec) (Mod (mdecl, _imports, decls)) : result = 
    let rec decl_dim ctx decl : result =
      let (decl, pos) = both decl in
      match decl with 
      | Annot _  
      | Port _  
      | TyCon _ 
      | Alias _ -> ok ()
      | Pat (pat, e) -> 
        let ctx' = BindingContext.add ctx (Val pat, pos) in
        expr_dim ctx' e
      | Fun ((var, _), e) -> 
        let ctx' = BindingContext.add ctx (Fun var, pos) in
        expr_dim ctx' e
  
    and expr_dim ctx expr' : result = 
      let check ~lim ~ctx site d =
        if lim < d then warn () (BindingContext.dump ctx, site, d)
        else ok ()
      in
      let check_fapp   ctx es = check ~lim:spec.fapp ~ctx (App expr') (List.length es)
      and check_tuple  ctx es = check ~lim:spec.tuple ~ctx (Tuple expr') (List.length es)
      and check_list   ctx es = check ~lim:spec.list ~ctx (List expr') (List.length es)
      and check_record ctx es = check ~lim:spec.record ~ctx (Record expr') (List.length es)
      and check_dcon   ctx es = check ~lim:spec.dcon ~ctx (DCon expr') (List.length es) in
      let dim = expr_dim ctx in
      let dim_iter = R.Par.iter ~f:dim in
      let (expr, (pos, _)) = both expr' in
      match expr with 
      (* Context altering cases *)
      | Case (e, branches) -> 
        let branch_depth (pat, expr') : result = 
          let ctx' = BindingContext.add ctx (Val pat, pos) in
          expr_dim ctx' expr'
        in 
        let* () = dim e
        and* () = R.Par.iter branches ~f:branch_depth in
        ok ()
      | Lambda (_, e)      ->
        let ctx' = BindingContext.add ctx (Lam, pos) in
        expr_dim ctx' e
      | Let (decls, e)     -> 
        (* Context updates were folded inside decl_dim *)
        let* () = dim e
        and* () = R.Par.iter decls ~f:(decl_dim ctx) in
        ok ()
      (* Context preserving cases *)
      | If (e0, (e1, e2))  -> 
        R.Par.iter ~f:dim [ e0; e1; e2 ]
      | App (e, es)        -> 
        let* () = check_fapp ctx es
        and* () = dim e
        and* () = dim_iter es in
        ok ()
      | Infix (_, e0, e1)  -> 
        R.Par.iter ~f:dim [ e0; e1 ]
      | Unary (_, e)       -> 
        dim e
      | Tuple es           -> 
        let* () = check_tuple ctx es
        and* () = dim_iter es in
        ok ()
      | List es            -> 
        let* () = check_list ctx es
        and* () = dim_iter es in
        ok ()
      | Record field_exprs -> 
        let* () = check_record ctx field_exprs;
        and* () = R.Par.iter field_exprs ~f:(fun fe -> dim @@ snd fe) in
        ok ()
      | ProjFunc _         -> ok ()
      | Project (e, _)     -> dim e
      | Extension  (e, fs) -> 
        let* () = dim e
        and* () = R.Par.iter fs ~f:(fun fe -> dim @@ snd fe) in
        ok ()
      | DCon (_, es)       -> 
        let* () = check_dcon ctx es
        and* () = dim_iter es in
        ok ()
      | Var _ 
      | Literal  _ 
      | Unit  
      | OpFunc _ -> ok ()
    in 
    let ctx = 
      Core.Option.value_map mdecl
      ~f:(
        fun (MDecl (mod_path, _)) ->
          let (_, pos) = both mod_path in
          BindingContext.add BindingContext.empty (BindingContext.Mod mod_path, pos) 
      )
      ~default:BindingContext.empty
    in
    R.Par.iter decls ~f:(decl_dim ctx)
end

type spec = 
  OverDim.spec = { 
    fapp   : int;
    tuple  : int;
    list   : int;
    record : int;
    dcon   : int;
  }
  
let run = OverDim.run

let dump_result src r = 
  let open ElAst.ToString in
  let dump_entry ((ctx, site, dim) : violation) =
    match site with
    | App expr -> 
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Function application (at %s) below hits an arity upper bound with\n    - arguments by %d\n\n%s\n"
                    (pos_to_string pos)
                    dim
                    (Src.Source.lines src pos)
    | Tuple expr ->
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Tuple expression (at %s) below hits a dimension upper bound with\n    - tuple length of %d\n\n%s\n"
                    (pos_to_string pos)
                    dim
                    (Src.Source.lines src pos)
    | List expr ->
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "List expression (at %s) below hits a dimension upper bound with\n    - list length of %d\n\n%s\n"
                    (pos_to_string pos)
                    dim
                    (Src.Source.lines src pos)
    | Record expr ->
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Record expression (at %s) below hits a dimension upper bound with\n    - record with fields by %d\n\n%s\n"
                    (pos_to_string pos)
                    dim
                    (Src.Source.lines src pos)
    | DCon expr ->
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Data constructor (at %s) below hits an arity upper bound with\n    - arguments by %d\n\n%s\n"
                    (pos_to_string pos)
                    dim
                    (Src.Source.lines src pos)
  in
  let ((), violations) = R.run r in
  Core.List.iter violations ~f:dump_entry
