(* AstCommon.ml
 *
 * This file defines an attribute-carrying AST. The file defines the shape of the AST
 * while in the same time leave open an arbitrary type argument for possible AST annotations
 *
 * The annotation could contain
 * 
 * - Source positional info
 * - Type info if type checking were to be performed on the program
 * - Analysis information of any kind
 *
 * This definition can be extended so that different syntax constructs carries different info
 * but for the purpose of parsing this does not seem neccessary
 *
 * This module also offers the convenience of defining maps and folds on AST nodes
 * E.g. 
 *    Map  : 'a Ast.t -> ('a -> 'b) -> 'b Ast.t 
 *    Fold : 'a Ast.t -> ('a -> 'b) -> (('a * 'b) list -> 'b) -> 'b Ast.t
 * 
 * Can both be trivially coded once for all 'a and 'b
 *)
module Ast =
struct
  open SyntaxModular

  type pos = Lexing.position * Lexing.position

  type paren =
    | Wrapped of int   (* counts number of parens wrapped *)
    | Naked

  
  module Node = 
  struct 
    type 'x t = 'x * pos 
  end

  module ParNode =
  struct 
    type 't par_node = 't * pos * paren
  end 

  module Var = Node

  let naked x pos : 't ParNode.t = (x, pos, Naked)

  let wrapped (x, _, par) pos : 't par_node = 
    match par with 
    | Naked     -> (x, pos, Wrapped 1)
    | Wrapped i -> (x, pos, Wrapped (i + 1))
end

(* This module will be opened and exposed to the parser *)
module Util =
struct

  open AstCommon

  include Ast

  (* Identifiers *)
  let breakQualifiedId (tok, (start, _)) = 
    match tok with 
    | Tokens.QVARID s -> 
      let toks = Lex.LayoutSensitiveLexer.explode_qualified_name s start in
      toks
    | Tokens.QCONID s ->
      let toks = Lex.LayoutSensitiveLexer.explode_qualified_name s start in
      toks
    | _ -> assert false

  let expr n pos : expr = naked n pos
  let pat  n pos : pat  = naked n pos

  let var name pos  : var  = node (Var.of_string name) pos
  let con name pos  : con  = node (Con.of_string name) pos

  (* Operator intermediate value *)
  type op =
  | MathOp of Expr.op_math
  | CmpOp  of Expr.op_cmp

  let op2expr op e1 e2 =
    match op with 
    | MathOp op -> Expr.InfixMath (op, e1, e2) 
    | CmpOp  op -> Expr.InfixCmp  (op, e1, e2)

  let op2opfunc op =
    match op with
    | MathOp op -> Expr.OpFuncMath op
    | CmpOp  op -> Expr.OpFuncCmp  op

  (* Intermediate values for binders *)
  type binders =
  | FuncBinder of var * pat list
  | PatBinder  of pat

  (* Intermediate values for atomic constructors 
  * This is needed because those constructors, though syntatically sharing the same node
  * can either serve as a value in an expresion, or an atomic pattern *)
  type gcon = | Unit | EmptyList | ConRef of qcon

  let gcon2pat g =
    match g with 
    | Unit      -> Pattern.Unit
    | EmptyList -> Pattern.EmptyList
    | ConRef c  -> Pattern.Ctor (c, [])

  let gcon2expr g =
    match g with 
    | Unit       -> Expr.Unit
    | EmptyList  -> Expr.List []
    | ConRef c   -> Expr.Con (c, [])
end