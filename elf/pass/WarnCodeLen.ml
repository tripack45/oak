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
open PassUtil

let (>.) = Core.Float.(>.)

module R = Util.PassResult.WarnList
open R.Pervasive

open ElAst.Syntax
open ElAst.Node

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

let merge ((px, wx)) ((py, wy)) =
  (px + py, wx +. wy)

let merge_all =
  Core.List.fold ~init:bot ~f:merge

let merge_map r ~fmap = R.Par.map r ~f:fmap >>| merge_all

let merge_t l = ok @@ merge_all l

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
    name_size var' 24 0.1 ElAst.VarId.to_string 
  and tvar_size tvar' = 
    name_size tvar' 24 0.1 ElAst.TVarId.to_string
  
  (* con_size_weighed =
  *   len <= 16 -> 1
  *   len > 16  -> 1 + 0.15 * (len - 16)
  *)
  let mcon_size mcon' = 
    name_size mcon' 16 0.15 ElAst.MConId.to_string
  and tycon_size tycon' = 
    name_size tycon'  16 0.15 ElAst.TyConId.to_string
  and dcon_size dcon' = 
    name_size dcon' 16 0.15 ElAst.DConId.to_string

  (* field_size_weighed =
  *   len <= 10 -> 1
  *   len > 10  -> 1 + 0.1 * (len - 10)
  *)
  let field_size field' = 
    name_size field' 10 0.1 ElAst.FieldId.to_string

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
      let* p = pat_size pat'
      and* v = var_size var' in
      merge_t [ readable; p; v; ]
    | Cons (pat', pat'') -> 
      let* pat = pat_size pat'
      and* pat' = pat_size pat'' in
      merge_t [readable; pat; pat']
    | List pat's
    | Tuple pat's  -> 
      let* ps = merge_map pat's ~fmap:pat_size in
      merge_t [readable; ps]
    | Record fs    -> 
      let* fs = merge_map fs ~fmap:(fun (f, _) -> field_size (node f ())) in
      merge_t [readable; fs]
    | Con (qdcon', pat's) -> 
      let* qd = qdcon_size qdcon'
      and* ps = merge_map pat's ~fmap:pat_size in
      merge_t [qd; ps]

  let rec typ_size typ' =
    let typ: typ = elem typ' in
    match typ with
    | TVar   (tvar')        -> tvar_size tvar'
    | Unit                  -> ok readable
    | TyCon  (qtcon')       -> qtycon_size qtcon'
    | Arrow  (l, r)         -> 
      let* l = typ_size l
      and* r = typ_size r in
      merge_t [readable; l; r]
    | TApp   (typ', typ'el) -> 
      let* t = typ_size typ'
      and* te = typ_size typ'el in
      merge_t [bot; t; te]
    | Record (row)          -> 
      let* r = row_size row in
      merge_t [readable; r]
    | Tuple  (typ's)        -> 
      let* ts = merge_map typ's ~fmap:typ_size in
      merge_t [readable; ts]
  
  and row_size row: result =
    let pair_size (field', typ') = 
      let* f = field_size field'
      and* t = typ_size typ' in
      merge_t [f; t]
    in
    match row with 
    | RVar tvar' -> 
      tvar_size tvar'
    | Extension (row, pairs) ->
      let* r = row_size row
      and* ps = merge_map pairs ~fmap:pair_size in
      merge_t [readable; r; ps]
    | Fields pairs -> 
      merge_map pairs ~fmap:pair_size

  let op_size op = 
    match op with
    _ -> ok trival

  (* { x : limit * soft } 
   * limit is size's the lowest upper bound;
   * soft is the size that higher level sees in case of violation
   *)
  type len_spec = { m : size * size; func : size * size; lambda : size * size }
  let len_spec = { 
    m = ((1000, 1000.0), (800, 800.0)); 
    func = ((100, 100.0), (70, 70.0)); 
    lambda = ((20, 20.0), (20, 20.0) )
  }

  let run ?(spec = len_spec)
    (Mod (mdecl, _imports, decls)) : result =

    let check ctx site (limit, soft) size : result =
      (* if s is larger than limit, then count as soft for higher layer *)
      if excceed size limit then
        warn soft @@ Violation (BindingContext.dump ctx, site, size)
      else
        ok size
    in

    let check_m ctx size = 
      check ctx (Module mdecl) spec.m size
    in
    let check_func ctx decl' size = 
      check ctx (Function decl') spec.func size
    in
    let check_lambda ctx expr' size = 
      check ctx (Lambda expr') spec.lambda size
    in

    let rec decl_size ctx (decl': decl') : result =
      let (decl, pos) = both decl' in
      match decl with 
      (* Annotations wouldn't be counted *)
      | Annot _ -> ok bot
      | Port _ ->  ok bot
      | TyCon ((tycon', tvar's), branches) -> 
        let branch_size (dcon', typ') : result = 
          let* d = dcon_size dcon'
          and* ts = merge_map typ' ~fmap:typ_size in
          merge_t [d; ts]
        in
        let* t = tycon_size tycon'
        and* bs = merge_map branches ~fmap:branch_size
        and* ts = merge_map tvar's ~fmap:tvar_size in
        merge_t [t; bs; ts]
      | Alias ((con', var's), typ') -> 
        let* tc = tycon_size con'
        and* t = typ_size typ'
        and* vs = merge_map var's ~fmap:tvar_size in
        merge_t [tc; t; vs]
      | Pat (pat', expr') -> 
        let ctx' = BindingContext.add ctx (Val pat', pos) in
        let* p = pat_size pat'
        and* e = expr_size ctx' expr' in
        merge_t [p; e]
      | Fun ((var', pat's), expr') -> 
        let ctx' = BindingContext.add ctx (Fun var', pos) in
        let* res =
          let* v = var_size var'
          and* e = expr_size ctx' expr'
          and* ps = merge_map pat's ~fmap: pat_size in
          merge_t [v; e; ps]
        in
        check_func ctx decl' res
  
    and expr_size ctx (expr': expr'): result =
      let siz e: result = expr_size ctx e in
      let (expr, (pos, _)) = both expr' in
      match expr with 
      (* Context altering cases *)
      | Case (e, branches) -> 
        let branch_size (pat, expr') = 
          let ctx' = BindingContext.add ctx (Val pat, pos) in
          let* p = pat_size pat
          and* e = expr_size ctx' expr' in
          merge_t [p; e]
        in
        let* e = siz e
        and* bs = merge_map branches ~fmap:branch_size in
        merge_t [(2, 0.); e; bs]
      | Lambda (pat's, expr')      ->
        let ctx' = BindingContext.add ctx (Lam, pos) in
        let siz = check_lambda ctx' expr' in
        let* res = 
          let* ps = merge_map pat's ~fmap:pat_size
          and* e = expr_size ctx' expr' in
          merge_t [ps; e]
        in
        siz res
      | Let (decls, e)     -> 
        (* Context updates were folded inside decl_size *)
        let* e = siz e
        and* ds = merge_map decls ~fmap:(decl_size ctx) in
        merge_t [e; ds]
      (* Context preserving cases *)
      | If (e0, (e1, e2))  -> 
        let* i = ok (3, 4.)
        and* e0 = siz e0
        and* e1 = siz e1
        and* e2 = siz e2 in
        merge_t [i; e0; e1; e2]
      | App (e, es)        -> 
        let* e = siz e
        and* es = merge_map es ~fmap:siz in
        merge_t [e; es]
      | Infix (op, e0, e1) -> 
        let* op = op_size op
        and* e0 = siz e0
        and* e1 = siz e1 in
        merge_t [op; e0; e1]
      | Unary (op, e)      -> 
        let* op = op_size op
        and* e = siz e in
        merge_t [op; e]
      | Tuple es           -> 
        let* es = merge_map es ~fmap:siz in
        merge_t [readable; es]
      | List es            -> 
        let* es = merge_map es ~fmap:siz in
        merge_t [readable; es]
      | Record field_exprs -> merge_map field_exprs ~fmap:(fun fe -> siz @@ snd fe)
      | ProjFunc   f       -> field_size f
      | Project    (e, f)  -> 
        let* e = siz e
        and* f = field_size f in
        merge_t [e; f]
      | Extension  (e, fs) -> 
        let* e = siz e
        and* fs = merge_map fs ~fmap:(fun fe -> siz @@ snd fe) in
        merge_t [readable; e; fs]
      | Con (qdcon', es)   -> 
        let* qd = qdcon_size qdcon'
        and* es = merge_map es ~fmap:siz in
        merge_t [qd; es]
      | Var qvar'          -> qvar_size qvar'
      | Literal  lit'      -> lit_size lit'
      | Unit               -> ok readable
      | OpFunc op          -> op_size op
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
  let* m_size = merge_map decls ~fmap:(decl_size ctx) in
  check_m ctx m_size
end

type spec = CodeLen.len_spec = { m : size * size; func : size * size; lambda : size * size }
let run = CodeLen.run

let dump_result src r = 
  let open ElAst.ToString in
  let dump_entry (Violation (ctx, site, (plain, weighed)) : violation) =
    match site with
    | Module Some mdecl ->
      let MDecl (path, _) = mdecl in
      let (_, pos) = both path in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Module <%s> (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s\n"
                    (path_to_string path)
                    (pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
    | Module None ->
      List.iter ctx ~f:dump_binding;
      Printf.printf "Module below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n"
                    plain 
                    weighed
    | Function decl' -> 
      let pos = attr decl' in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Function definition (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s\n"
                    (pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
    | Lambda expr ->
      let (pos, _) = attr expr in
      List.iter ctx ~f:dump_binding;
      Printf.printf "Expression (at %s) below excceeds a code length upper bound with\n    - plain length of %d;\n    - weighed length of %.2f.\n\n%s\n"
                    (pos_to_string pos)
                    plain 
                    weighed
                    (Src.Source.lines src pos)
  in
  let ((plain, weighed), violations) = R.run r in
  if List.length violations = 0 then
    Printf.printf "The module has\n    - plain length of %d;\n    - weighed length of %.2f.\n" 
      plain weighed
  else
    Core.List.iter violations ~f:dump_entry
