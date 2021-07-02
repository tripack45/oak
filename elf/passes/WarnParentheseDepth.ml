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

module R = Util.PassResult.WarnList

open R.Pervasive

open ElAst.Syntax
open ElAst.Node

module BindingContext : 
sig 
  type binder =
    | Val of pat'
    | Fun of var'
    | Lam
  type t 
  val empty : t
  val add  : t -> binder * pos -> t 
  val dump : t -> (binder * pos) list
end =
struct
  type binder =
    | Val of pat'
    | Fun of var'
    | Lam

  (* Context is a stack of a binder and the position of the enclosing construct *)
  type t = (binder * pos) list

  (* Empty context *)
  let empty : t = []

  let add ctx bp : t = (bp::ctx)
  let dump ctx = List.rev ctx
end

type binder    = BindingContext.binder
type ctx       = BindingContext.t
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
let maxd rx ry = (rx ** ry) >>| Tuple2.uncurry max

let maxints ints = 
  List.max_elt ints ~compare:Int.compare |> Option.value ~default: 0

let ( <*> ) = maxd

let run ?(max_depth=5) ?soft (Mod (_mdecl, _imports, decls)) : result = 
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
        in dep e <*> R.Par.map_then branches ~fmap:branch_depth ~fthen:maxints
      | Lambda (_, e)      ->
        let ctx' = BindingContext.add ctx (Lam, pos) in
        expr_par_depths ctx' e
      | Let (decls, e)     -> 
        (* Context updates were folded inside decl_par_depths *)
        dep e <*> R.Par.map_then decls ~fmap:(decl_par_depths ctx) ~fthen:maxints
      (* Context preserving cases *)
      | If (e0, (e1, e2))  -> dep e0 <*> dep e1 <*> dep e2
      | App (e, es)        -> dep e  <*> R.Par.map_then es ~fmap:dep ~fthen:maxints
      | Infix (_, e0, e1)  -> (dep e0) <*> (dep e1)
      | Tuple es           -> R.Par.map_then es ~fmap:dep ~fthen:maxints
      | List es            -> R.Par.map_then es ~fmap:dep ~fthen:maxints
      | Record field_exprs -> R.Par.map_then field_exprs ~fmap:(fun fe -> dep @@ snd fe) ~fthen:maxints
      | Con (_, es)        -> R.Par.map_then es ~fmap:dep ~fthen:maxints
      | Var _ 
      | Literal  _ 
      | Unit  
      | OpFunc _ -> ok 0
    in stack_par sub_result
  in 
  R.Par.map_then decls ~fmap:(decl_par_depths BindingContext.empty) ~fthen:maxints


let dump_result src r = 
  let dump_entry ((ctx, expr, depths) : violation) =
    let dump_binding (binder, pos) =
      let binder_str = 
        match binder with
        | BindingContext.Lam -> "lambda function"
        | BindingContext.Val pat -> ElAst.ToString.pat_to_string pat
        | Fun var -> ElAst.ToString.var_to_string var
      in
      Printf.printf "In the definition of \"%s\" at %s:\n" 
                    binder_str (ElAst.ToString.pos_to_string pos)
    in 
    let (pos, _) = attr expr in
    List.iter ~f:dump_binding ctx;
    Printf.printf "Expression (at %s) below reaches a parentheses depths of %d:\n%s\n\n"
                  (ElAst.ToString.pos_to_string pos)
                  depths 
                  (Src.Source.lines src pos)
  in
  let (_, entries) = R.run r in
    Core.List.iteri entries ~f:(
      fun _idx entry -> dump_entry entry
    )