(* WarnCodeLen.ml
 *
 * This files contains the analysis-report pass WarnCodeLen. This path
 * traverses the module AST and reports the subexpressions reaching a given 
 * code length criterion. The criterion requires that a specific module, 
 * function or a lambda should contain information less than a given amount. 
 * 
 * The code length is measured by two dimensions: plain length and weighed length.
 * 
 * For each expression that violates the rule, provide the name of its toplevel binding 
 * and its own position.
 *
 * This pass runs on the El AST.
 *
 * This pass is altered based on the WarnParentheseDepth template.
 *
 *)

open Oaklib
open ElAst.Syntax
open ElAst.Node

module BindingContext : sig
  type binder = 
    | Val of pat'
    | Fun of var' 
    | Lam of expr' (* // Todo: treat \\ as lambda starter in ElAst to properly record position  *)

  type t
  val empty : t
  val add : t -> binder * pos -> t
  val dump : t -> (binder * pos) list
end = struct
  type binder = 
    | Val of pat'
    | Fun of var' 
    | Lam of expr'

  (* Context is a stack of a binder and the position of the enclosing construct *)
  type t = (binder * pos) list

  (* Empty context *)
  let empty : t = []

  let add ctx bp : t = bp :: ctx

  let dump ctx = List.rev ctx
end

type binder = BindingContext.binder
type ctx    = BindingContext.t

(* The size are expressed in:
 * - plain length
 * - weighed length
 * a violation of either will produce a warning
 *)
type size = Size of int * float

let excceed (Size (p, w)) (Size (pl, wl)) =
  p > pl || w > wl

(* The site deals with all "meaningful" data structure during this pass. *)
type site = 
  | Module   of mdecl option
  | Function of decl'
  | Lambda   of expr'

type violation = Violation of (binder * pos) list * site * size

type result = size * violation list

let merge_result (Size (px, wx), vx) (Size (py, wy), vy) =
  (Size(px + py, wx +. wy), vx @ vy)

let bare s = (s, [])

(* The "nullary" size, A.K.A. the "bottom" size *)
let bot = bare @@ Size (0, 0.)
(* The "trival" size. *)
let trival = bare @@ Size (1, 1.)
let readable = bare @@ Size (1, 0.5)

let merge_results = Core.List.fold ~init:bot ~f:merge_result


module CodeLen =
struct

  let name_size node base rate  to_string =
    let name = elem node in
    let len = name |> to_string |> String.length in
    let len_extra = len - base in 
    let penalty = if len_extra > 0 then Int.to_float len_extra else 0. in
    bare @@ Size (1, 1. +. penalty *. rate)

  (* var_size_weighed =
  *   len <= 24 -> 1
  *   len > 24  -> 1 + 0.1 * (len - 24)
  *)
  let var_size var' = 
    name_size var' 24 0.1 Oaklib.ElAst.VarId.to_string 
  and tvar_size tvar' = 
    name_size tvar' 24 0.1 Oaklib.ElAst.TVarId.to_string
  
  (* con_size_weighed =
  *   len <= 16 -> 1
  *   len > 16  -> 1 + 0.15 * (len - 16)
  *)
  let mcon_size mcon' = 
    name_size mcon' 16 0.15 Oaklib.ElAst.MConId.to_string
  and tycon_size tycon' = 
    name_size tycon'  16 0.15 Oaklib.ElAst.TyConId.to_string
  and dcon_size dcon' = 
    name_size dcon' 16 0.15 Oaklib.ElAst.DConId.to_string

  (* field_size_weighed =
  *   len <= 10 -> 1
  *   len > 10  -> 1 + 0.1 * (len - 10)
  *)
  let field_size field' = 
    name_size field' 10 0.1 Oaklib.ElAst.FieldId.to_string

  let lit_size lit' =
    match elem lit' with
    | Int _      -> trival
    | Float _    -> trival
    | String str -> let len = (str |> String.length |> Int.to_float) *. 0.1 in
    bare @@ Size (Float.to_int len, len)

  let qvar_size qvar' = 
    let QVar(_, var') = elem qvar' in
    var_size var'

  let qtycon_size qtycon' = 
    let QTyCon(_,tycon') = elem qtycon' in
    tycon_size tycon'
  and qdcon_size qdcon' = 
    let QDCon(_, dcon') = elem qdcon' in
    dcon_size dcon'

  let rec pat_size pat' =
    let pat: pat = elem pat' in
    match pat with
    | Var       (var')           -> var_size var'
    | Any
    | Unit                       -> trival
    | EmptyList                  -> readable
    | Literal   (lit')           -> lit_size lit'
    | List      (pat's)          -> merge_results @@ readable :: (List.map pat_size pat's)
    | Tuple     (pat's)          -> merge_results @@ readable :: (List.map pat_size pat's)
    | Con       (qdcon', pat's)  -> merge_results @@ qdcon_size qdcon' :: (List.map pat_size pat's)

  let rec typ_size typ' =
    let typ: typ = elem typ' in
    match typ with
    | TVar   (tvar')        -> tvar_size tvar'
    | Unit                  -> readable
    | TyCon  (qtcon')       -> qtycon_size qtcon'
    | Arrow  (l, r)         -> merge_results @@ readable :: [typ_size l; typ_size r]
    | TApp   (typ', typ'el) -> merge_results @@ bot      :: [typ_size typ'; typ_size typ'el]
    | Record (row)          -> merge_results @@ readable :: [(row_size row)]
    | Tuple  (typ's)        -> merge_results @@ readable :: List.map typ_size typ's
  
  and row_size row =
    let pair_size (field', typ') = merge_result (field_size field') (typ_size typ') in
    match row with 
    | RVar tvar' -> tvar_size tvar'
    | Extension (row, pairs) -> merge_results @@ row_size row :: List.map pair_size pairs 
    | Fields    pairs        -> merge_results @@ List.map pair_size pairs

  let op_size op = 
    match op with
    _ -> trival

  (* { x : limit * soft } 
   * limit is size's the lowest upper bound;
   * soft is the size that higher level sees in case of violation
   *)
  type code_len_args = { m : size * size; func : size * size; lambda : size * size }
  let code_len_args_default = { 
    m = (Size (1000, 1000.0), Size (800, 800.0)); 
    func = (Size (100, 100.0), Size (70, 70.0)); 
    lambda = (Size (20, 20.0), Size (20, 20.0) )
  }

  let run ?(args = code_len_args_default)
    (Mod (mdecl, _imports, decls)) : result =

    let check ctx site (limit, soft) res : result =
      let (s, vios) = res in
      let vio = Violation (BindingContext.dump ctx, site, s) in
      (* if s is larger than limit, then count as soft for higher layer *)
      if excceed s limit then (soft, vios @ [vio]) else res
    in

    let check_m ctx size = 
      check ctx (Module mdecl) args.m size
    in
    let check_func ctx decl' size = 
      check ctx (Function decl') args.func size
    in
    let check_lambda ctx expr' size = 
      check ctx (Lambda expr') args.lambda size
    in

    let rec decl_size ctx decl' : result =
      let (decl, pos) = both decl' in
      match decl with 
      (* Annotations wouldn't be counted *)
      | Annot _ -> bot
      | TyCon ((tycon', tvar's), branches) -> 
        let brance_size (dcon', typ') = merge_results @@ dcon_size dcon' :: (List.map typ_size typ') in
        let var_ = merge_results @@ List.map tvar_size tvar's
        and branch_ = merge_results @@ List.map brance_size branches
        in merge_results [(tycon_size tycon'); var_; branch_]
      | Alias ((con', var's), typ') -> 
        let var_ = merge_results @@ List.map tvar_size var's
        in merge_results [(tycon_size con'); var_; typ_size typ']
      | Pat (pat', expr') -> 
        let ctx' = BindingContext.add ctx (Val pat', pos) in
        merge_results [pat_size pat'; expr_size ctx' expr']
      | Fun ((var', pat's), expr') -> 
        let ctx' = BindingContext.add ctx (Fun var', pos) in
        let res = merge_results @@ var_size var' :: expr_size ctx' expr' :: List.map pat_size pat's in
        check_func ctx decl' res
  
    and expr_size ctx expr': result =
      let siz = expr_size ctx in
      let (expr, (pos, _)) = both expr' in
      match expr with 
      (* Context altering cases *)
      | Case (e, branches) -> 
        let branch_depth (pat, expr') = 
          let ctx' = BindingContext.add ctx (Val pat, pos) in
          expr_size ctx' expr'
        in merge_results @@ (bare @@ Size(2, 0.)) :: siz e :: List.map branch_depth branches 
      | Lambda (pat's, expr')      ->
        let ctx' = BindingContext.add ctx (Lam(expr'), pos) in
        check_lambda ctx' expr' (merge_results @@ List.map pat_size pat's @ [expr_size ctx' expr'])
      | Let (decls, e)     -> 
        (* Context updates were folded inside decl_size *)
        merge_results @@ siz e :: List.map (decl_size ctx) decls
      (* Context preserving cases *)
      | If (e0, (e1, e2))  -> merge_results [bare @@ Size(3, 4.); siz e0; siz e1; siz e2]
      | App (e, es)        -> merge_results @@ siz e :: List.map siz es
      | Infix (op, e0, e1) -> merge_results [op_size op; siz e0; siz e1]
      | Tuple es           -> merge_results @@ readable :: (List.map siz es)
      | List es            -> merge_results @@ readable :: (List.map siz es)
      | Record field_exprs -> merge_results @@ List.map (fun fe -> siz @@ snd fe) field_exprs
      | Con (qdcon', es)   -> merge_results @@ qdcon_size qdcon' :: List.map siz es
      | Var qvar'          -> qvar_size qvar'
      | Literal  lit'      -> lit_size lit'
      | Unit               -> readable
      | OpFunc op          -> op_size op
  in
  let ctx = BindingContext.empty in
  check_m ctx (merge_results @@ List.map (decl_size ctx) decls)
end

let run = CodeLen.run

let dump_result src (Size (plain, weighed), violations) = 
  let dump_entry (Violation (ctx, site, Size (plain, weighed)) : violation) =
    let dump_binding (binder, pos) =
      let binder_str = 
        match binder with
        | BindingContext.Val pat   -> ElAst.ToString.pat_to_string pat
        | BindingContext.Lam expr  -> ElAst.ToString.expr_to_string expr
        | BindingContext.Fun var   -> ElAst.ToString.var_to_string var
      in
      Printf.printf "In the definition of \"%s\" at %s:\n" 
                    binder_str (ElAst.ToString.pos_to_string pos)
    in 
    match site with
    | Module Some mdecl ->
      let MDecl (mcon, _) = mdecl in
      let (_, pos) = both mcon in
      List.iter dump_binding ctx;
      Printf.printf "Module <%s> (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s"
                    (ElAst.ToString.mcon_to_string mcon)
                    (ElAst.ToString.pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
    | Module None ->
      List.iter dump_binding ctx;
      Printf.printf "Module excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n"
                    plain 
                    weighed
    | Function decl' -> 
      let pos = attr decl' in
      List.iter dump_binding ctx;
      Printf.printf "Function definition (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s"
                    (ElAst.ToString.pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
    | Lambda expr ->
      let (pos, _) = attr expr in
      List.iter dump_binding ctx;
      Printf.printf "Expression (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s"
                    (ElAst.ToString.pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
  in
  if List.length violations == 0 then
    Printf.printf "The module has\n    - plain length of %d;\n    - weighed length of %.2f.\n" 
      plain weighed
  else
    Core.List.iteri violations ~f:(
      fun idx entry ->
        if idx != 0 then print_endline "" else ();
        dump_entry entry
    )