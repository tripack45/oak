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

open Core
open Oaklib

let (>.) = Core.Float.(>.)

module R = Util.PassResult.WarnList
open R.Pervasive

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
type size = int * float

let excceed ((p, w)) ((pl, wl)) =
  p > pl || w >. wl

(* let r_excceed r1 r2 = r1 ** r2 >>| Tuple2.uncurry excceed *)


(* The site deals with all "meaningful" data structure during this pass. *)
type site = 
  | Module   of mdecl option
  | Function of decl'
  | Lambda   of expr'

type violation = Violation of (binder * pos) list * site * size

type result = (size, violation) R.t

(* The "nullary" size, A.K.A. the "bottom" size *)
let bot = (0, 0.)
(* The "trival" size. *)
let trival = (1, 1.)
let readable = (1, 0.5)

let merge_size ((px, wx)) ((py, wy)) =
  (px + py, wx +. wy)

let merge_sizes =
  Core.List.fold ~init:bot ~f:merge_size

let merge x y =
  ok @@ merge_size x y

let merge2 r1 r2 = r1 ** r2 >>= Tuple2.uncurry merge

let (<*>) = merge2

let merge_results r: result = R.Seq.fold ~init:(ok bot) ~f:merge r


module CodeLen =
struct

  let name_size node base rate  to_string =
    let name = elem node in
    let len = name |> to_string |> String.length in
    let len_extra = len - base in 
    let penalty = if len_extra > 0 then Int.to_float len_extra else 0. in
    ok (1, 1. +. penalty *. rate)

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
    | Int _      -> ok trival
    | Float _    -> ok trival
    | Char _     -> ok trival
    | String str -> let len = (str |> String.length |> Int.to_float) *. 0.1 in
    ok (Float.to_int len, len)

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
    | Var var'     -> var_size var'
    | Any
    | Unit         -> ok trival
    | EmptyList    -> ok readable
    | Literal lit' -> lit_size lit'
    | Alias (pat', var') -> 
      let* pat = pat_size pat'
      and* var = var_size var' in
      merge_results [ readable; pat; var; ]
    | Cons (pat', pat'') -> 
      let* pat' = pat_size pat'
      and* pat'' = pat_size pat'' in
      merge_results [readable; pat'; pat'']
    | List pat's
    | Tuple pat's  -> 
      let* pat's = R.Par.map ~f:pat_size pat's in
      merge_results @@ readable :: pat's
    | Record fs    -> 
      let* fs = R.Par.map ~f:(fun (f, _) -> field_size (node f ())) fs in
      merge_results @@ readable :: fs
    | Con (qdcon', pat's) -> 
      let* qdcon' = qdcon_size qdcon'
      and* pat's = R.Par.map ~f:pat_size pat's in
      merge_results @@ qdcon' :: pat's

  let rec typ_size typ' =
    let typ: typ = elem typ' in
    match typ with
    | TVar   (tvar')        -> tvar_size tvar'
    | Unit                  -> ok readable
    | TyCon  (qtcon')       -> qtycon_size qtcon'
    | Arrow  (l, r)         -> 
      let* l = typ_size l
      and* r = typ_size r in
      merge_results @@ readable :: [l; r]
    | TApp   (typ', typ'el) -> 
      let* typ' = typ_size typ'
      and* typ'el = typ_size typ'el in
      merge_results @@ bot      :: [typ'; typ'el]
    | Record (row)          -> 
      let* row = row_size row in
      merge_results @@ readable :: [row]
    | Tuple  (typ's)        -> 
      let* typ's = R.Par.map ~f:typ_size typ's in
      merge_results @@ readable :: typ's
  
  and row_size row: result =
    let pair_size (field', typ') = merge2 (field_size field') (typ_size typ') in
    match row with 
    | RVar tvar' -> 
      tvar_size tvar'
    | Extension (row, pairs) ->
      let* row = row_size row
      and* pairs = R.Par.map ~f:pair_size pairs in
      merge_results @@ readable :: row :: pairs 
    | Fields pairs -> 
      let* pairs = R.Par.map ~f:pair_size pairs in
      merge_results @@ pairs

  let op_size op = 
    match op with
    _ -> ok trival

  (* { x : limit * soft } 
   * limit is size's the lowest upper bound;
   * soft is the size that higher level sees in case of violation
   *)
  type code_len_args = { m : size * size; func : size * size; lambda : size * size }
  let code_len_args_default = { 
    m = ((1000, 1000.0), (800, 800.0)); 
    func = ((100, 100.0), (70, 70.0)); 
    lambda = ((20, 20.0), (20, 20.0) )
  }

  let run ?(args = code_len_args_default)
    (Mod (mdecl, _imports, decls)) : result =

    let check ctx site (limit, soft) size : result =
      (* if s is larger than limit, then count as soft for higher layer *)
      if excceed size limit then
        warn soft @@ Violation (BindingContext.dump ctx, site, size)
      else
        ok size
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

    let rec decl_size ctx (decl': decl') : result =
      let (decl, pos) = both decl' in
      match decl with 
      (* Annotations wouldn't be counted *)
      | Annot _ -> ok bot
      | Port _ ->  ok bot
      | TyCon ((tycon', tvar's), branches) -> 
        let brance_size (dcon', typ') : result = 
          let* dcon' = dcon_size dcon'
          and* typ' = R.Par.map typ' ~f:typ_size
          in
          merge_results @@ dcon' :: typ'
        in
        let* tycon' = tycon_size tycon'
        and* var_ = R.Par.map tvar's ~f:tvar_size
        and* branch_ = R.Par.map branches ~f:brance_size
        in merge_results @@ tycon' :: branch_ @ var_
      | Alias ((con', var's), typ') -> 
        let* con' = tycon_size con'
        and* typ' = typ_size typ'
        and* var_ = R.Par.map var's ~f:tvar_size in
        merge_results @@ con' :: typ' :: var_
      | Pat (pat', expr') -> 
        let ctx' = BindingContext.add ctx (Val pat', pos) in
        let* pat' = pat_size pat'
        and* expr' = expr_size ctx' expr' in
        merge_results [pat'; expr']
      | Fun ((var', pat's), expr') -> 
        let ctx' = BindingContext.add ctx (Fun var', pos) in
        let* var' = var_size var'
        and* expr' = expr_size ctx' expr'
        and* pat's = R.Par.map pat's ~f: pat_size in
        let* res = merge_results @@ var' :: expr' :: pat's in
        check_func ctx decl' res
  
    and expr_size ctx (expr': expr'): result =
      let siz e: result = expr_size ctx e in
      let (expr, (pos, _)) = both expr' in
      match expr with 
      (* Context altering cases *)
      | Case (e, branches) -> 
        let branch_size (pat, expr') = 
          let ctx' = BindingContext.add ctx (Val pat, pos) in
          let* pat = pat_size pat
          and* expr' = expr_size ctx' expr' in
          merge_results [pat; expr']
        in
        let* e = siz e
        and* branches = R.Par.map branches ~f:branch_size in
        merge_results @@ (2, 0.) :: e :: branches 
      | Lambda (pat's, expr')      ->
        let ctx' = BindingContext.add ctx (Lam(expr'), pos) in
        let siz = check_lambda ctx' expr' in
        let* pat's = R.Par.map pat's ~f:pat_size
        and* expr' = expr_size ctx' expr' in
        let* res = merge_results @@ expr' :: pat's in
        siz res
      | Let (decls, e)     -> 
        (* Context updates were folded inside decl_size *)
        let* e = siz e
        and* decls = R.Par.map decls ~f:(decl_size ctx) in
        merge_results @@ e :: decls
      (* Context preserving cases *)
      | If (e0, (e1, e2))  -> ok (3, 4.) <*> siz e0 <*> siz e1 <*> siz e2
      | App (e, es)        -> siz e <*> R.Par.map_then es ~fmap:siz ~fthen:merge_sizes
      | Infix (op, e0, e1) -> op_size op <*> siz e0 <*> siz e1
      | Unary (op, e)      -> op_size op <*> siz e
      | Tuple es           -> ok readable <*> R.Par.map_then es ~fmap:siz ~fthen:merge_sizes
      | List es            -> ok readable <*> R.Par.map_then es ~fmap:siz ~fthen:merge_sizes
      | Record field_exprs -> R.Par.map_then field_exprs ~fmap:(fun fe -> siz @@ snd fe) ~fthen:merge_sizes
      | ProjFunc   f       -> field_size f
      | Project    (e, f)  -> siz e <*> field_size f
      | Extension  (e, fs) -> ok readable <*> siz e <*> R.Par.map_then fs ~fmap:(fun fe -> siz @@ snd fe) ~fthen:merge_sizes
      | Con (qdcon', es)   -> qdcon_size qdcon' <*> R.Par.map_then es ~fmap:siz ~fthen:merge_sizes
      | Var qvar'          -> qvar_size qvar'
      | Literal  lit'      -> lit_size lit'
      | Unit               -> ok readable
      | OpFunc op          -> op_size op
  in
  let ctx = BindingContext.empty in
  (* check_m ctx (merge_results @@ List.map (decl_size ctx) decls) *)
  let* m_size = R.Par.map ~f:(decl_size ctx) decls in
  let* m_size = merge_results m_size in
  check_m ctx m_size
end

let run = CodeLen.run

let dump_result src r = 
  let dump_entry (Violation (ctx, site, (plain, weighed)) : violation) =
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
      let MDecl (path, _) = mdecl in
      let (_, pos) = both path in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Module <%s> (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s"
                    (ElAst.ToString.path_to_string path)
                    (ElAst.ToString.pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
    | Module None ->
      List.iter ctx ~f:dump_binding;
      Printf.printf "Module excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n"
                    plain 
                    weighed
    | Function decl' -> 
      let pos = attr decl' in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Function definition (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s"
                    (ElAst.ToString.pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
    | Lambda expr ->
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Expression (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s"
                    (ElAst.ToString.pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
  in
  let ((plain, weighed), violations) = R.run r in
  if List.length violations = 0 then
    Printf.printf "The module has\n    - plain length of %d;\n    - weighed length of %.2f.\n" 
      plain weighed
  else
    Core.List.iteri violations ~f:(
      fun idx entry ->
        if not (idx = 0) then print_endline "" else ();
        dump_entry entry
    )