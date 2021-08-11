(* WarnParentheseDepth.ml
 *
 * This files contains the analysis-report pass WarnParentheseDepth. This path
 * traverses the module AST and reports the subexpressions reaching a given 
 * parenthese nesting depth. It reports the smallest sub-expression reaching given
 * depths, and for each such expression, provide the name of its toplevel binding 
 * and its own position.
 *
 * This pass runs on the El AST.
 *
 * This pass should serve as a general example on how analysis/transformations 
 * work for a functional language. The transformation goes into two directions:
 *
 * - When one walks down in the syntax tree, collect information and store them 
 *   in a "context" argument.
 * - We we reach the leaf of the syntax tree, compute analysis result and (or) 
 *   preform neccssary transformations.
 *)

open Core
open Oaklib
open PassUtil

module R = Util.PassResult.WarnList

open R.Pervasive

open ElAst.Syntax
open ElAst.Node

type violation = (binder * pos) list * expr' * int

(* The result of the analysis can be either 
 * - Violations are found, in which case we stop tracking parentheses depths as 
 *   we are looking for the smallest expression violating depths constraints, or
 * - No violations has been found, in which case we note the current parentheses
 *   depths of the expression.
 *)
type result = (int, violation) R.t

(* An expression can have multiple sub-expressions, for example a case-expression 
 * can have any number of branches each with a different parentheses depth. 
 * - If any of the sub-expr constitutes a violation, then we no longer need to check 
 *   for enclosing expressions. We do need to collect the results as there might 
 *   be more than one violation.
 * - None of the sub-expressions constitutes a violation, then we use the maximum
 *   parentheses depths of all sub-expressions as its depths.
 *)
let maxints ints = 
  List.max_elt ints ~compare:Int.compare |> Option.value ~default: 0

let maxd ints = ok @@ maxints ints

let run ?(max_depth=5) ?soft (Mod (mdecl, _imports, decls)) : result = 
  let soft = max 0 (Option.value soft ~default:(max_depth - 2)) in
  let rec decl_par_depths ctx decl : result =
    let (decl, pos) = both decl in
    match decl with 
    | Annot _  
    | Port _  
    | TyCon _ 
    | Alias _ -> ok 0
    | Pat (pat, e) -> 
      let ctx' = BindingContext.add ctx (Val pat, pos) in
      expr_par_depths ctx' e
    | Fun ((var, _), e) -> 
      let ctx' = BindingContext.add ctx (Fun var, pos) in
      expr_par_depths ctx' e

  and expr_par_depths ctx expr : result = 
    let dep = expr_par_depths ctx in
    let dep_map_max ~fmap rs = R.Par.map rs ~f:fmap >>| maxints in
    let dep_max = dep_map_max ~fmap:dep in
    let (expr', (pos, par)) = both expr in
    (* Stacks the parenthese depths of current expr node *)
    let stack_par result : result = 
      let* x = result in
      match par with
      | Naked -> ok x
      | Wrapped y -> 
        if x + y < max_depth then 
          ok (x + y)
        else
          warn soft (BindingContext.dump ctx, expr, x + y)
    in
    let sub_result = 
      match expr' with 
      (* Context altering cases *)
      | Case (e, branches) -> 
        let branch_depth (pat, expr') : result = 
          let ctx' = BindingContext.add ctx (Val pat, pos) in
          expr_par_depths ctx' expr'
        in 
        let* e = dep e
        and* bs = dep_map_max branches ~fmap:branch_depth in
        maxd [e; bs]
      | Lambda (_, e)      ->
        let ctx' = BindingContext.add ctx (Lam, pos) in
        expr_par_depths ctx' e
      | Let (decls, e)     -> 
        (* Context updates were folded inside decl_par_depths *)
        let* e = dep e
        and* fs = dep_map_max decls ~fmap:(decl_par_depths ctx) in
        maxd [e; fs]
      (* Context preserving cases *)
      | If (e0, (e1, e2))  -> 
        let* e0 = dep e0
        and* e1 = dep e1
        and* e2 = dep e2 in
        maxd [e0; e1; e2]
      | App (e, es)        -> 
        let* e0 = dep e
        and* e1 = dep_max es in
        maxd [e0; e1]
      | Infix (_, e0, e1)  -> 
        let* e0 = dep e0
        and* e1 = dep e1 in
        maxd [e0; e1]
      | Unary (_, e)       -> dep e
      | Tuple es           -> dep_max es
      | List es            -> dep_max es
      | Record field_exprs -> dep_map_max field_exprs ~fmap:(fun (_, fe) -> dep fe)
      | ProjFunc _         -> ok 0
      | Project (e, _)     -> dep e
      | Extension  (e, fs) -> 
        let* e = dep e
        and* fs = dep_map_max fs ~fmap:(fun (_, fe) -> dep fe) in
        maxd [e; fs]
      | Con (_, es)        -> dep_max es
      | Var _ 
      | Literal  _ 
      | Unit  
      | OpFunc _ -> ok 0
    in stack_par sub_result
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
  R.Par.map decls ~f:(decl_par_depths ctx) >>| maxints


let dump_result src r = 
  let open ElAst.ToString in
  let dump_entry ((ctx, expr, depths) : violation) =
    let (pos, _) = attr expr in
    List.iter ~f:dump_binding ctx;
    Printf.printf "Expression (at %s) below reaches a parentheses depths of %d:\n%s\n\n"
                  (pos_to_string pos)
                  depths 
                  (Src.Source.lines src pos)
  in
  let (_, entries) = R.run r in
  Core.List.iter entries ~f:dump_entry
