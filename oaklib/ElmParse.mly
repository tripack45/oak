%{
  
module Util =
struct
  open ElAst
  open Syntax
  open Node

  let naked n pos = node n (pos, Naked)
  let wrapped par_node pos' : 't par_node = 
    let f = function
            | (pos, Naked)     -> (pos', Wrapped 1)
            | (pos, Wrapped i) -> (pos', Wrapped (i + 1))
    in map_attr f par_node

  let expr n pos : expr = naked n pos
  let pat  n pos : pat  = node n pos

  (* Identifiers *)
  let var name pos  : var  = node (VarId.of_string name) pos
  let con name pos  : con  = node (ConId.of_string name) pos
  
  let id_var var      : exposing_ident = Var var
  let id_tycon con    : exposing_ident = TyCon con
  let id_abstycon con : exposing_ident = AbsTyCon con

  let field name pos : field = node (FieldId.of_string name) pos

  let qvar (path, var) pos : qvar  = node (QVar (path, var)) pos
  let qcon (path, con) pos : qcon  = node (QCon (path, con)) pos

   (* This funcstion does a few things 
    * - Break the string into tokens using lexer assuming some token pos
    * - Extracts the last token and wraps it into an AST node
    * - Flatten and collects all positions into a list 
    * - Collects Path tokens into a list
    *     [ (tok, (start, end)), ... (tok', (start', end')), tp] 
    * =>  ([tok, ... tok'], [start, end, ..., start', end'], tp)
    *)
  let parse_qualified s (start, _) =
    let rec flatten toks = 
      match toks with 
      | [base] -> ([], [], base)
      | (con, (s, e)) :: toks -> 
        let (cons, poss, base) = flatten toks in
        (con::cons, s::e::poss, base)
      | [] -> assert false
    in
    flatten (Lex.LayoutSensitiveLexer.explode_qualified_name s start)

  let rec module_path cons = 
      match cons with 
      | [Tokens.CONID con]       -> Just (ConId.of_string con)
      | (Tokens.CONID con)::cons -> More (ConId.of_string con, module_path cons)
      | _ -> assert false

  let parse_modid (s : string) (pos : pos) : path =
    let path =
      (Lex.LayoutSensitiveLexer.explode_qualified_name s (fst pos))
      |> Core.List.map ~f:fst
      |> module_path
    in node path pos
      
  let parse_qvar (s : string) (pos : pos) : qvar = 
    let (cons, poss, (id_tok, id_pos)) = parse_qualified s pos in
    let path_node = node (module_path cons) (List.hd poss, List.hd (List.rev poss)) in
    match id_tok with 
    | Tokens.VARID varid -> 
      node (QVar (Some path_node, var varid id_pos)) pos
    | _ -> assert false

  let parse_qcon (s : string) (pos : pos) : qcon = 
    let (cons, poss, (id_tok, id_pos)) = parse_qualified s pos in
    let path_node = node (module_path cons) (List.hd poss, List.hd (List.rev poss)) in
    match id_tok with 
    | Tokens.CONID varid -> 
      node (QCon (Some path_node, con varid id_pos)) pos
    | _ -> assert false

  (* Operator intermediate value *)
  let op2expr op e1 e2 = Infix (op, e1, e2) 

  let op2opfunc op = OpFunc op

  (* Intermediate values for atomic constructors 
  * This is needed because those constructors, though syntatically sharing the same node
  * can either serve as a value in an expresion, or an atomic pattern *)
  type gcon = | Unit | EmptyList | QCon of qcon

  let gcon2pat g : _pat =
    match g with 
    | QCon c    -> Ctor (c, [])
    | Unit      -> Unit
    | EmptyList -> EmptyList

  let gcon2expr g : _expr =
    match g with 
    | Unit       -> Unit
    | EmptyList  -> List []
    | QCon c     -> Con (c, [])

  let mod_decl   (con, exposings) _ = (MDecl (con, exposings))
  let mod_import (qcon, as_con, exposings) _ = (Import (qcon, as_con, exposings))

  let mod_decls decls = ([], decls)
  let add_import (imps, decls) imp = (imp::imps, decls)

end

open ElAst.Alias
open ElAst.Node
open Util

%}

(* keywords *)
%token IMPORT MODULE EXPOSING AS
%token CASE OF 
%token LET IN 
%token IF THEN ELSE
%token TYPE ALIAS
(* Lexical Delimiters *)
%token EQ 
%token DOT
%token DOTDOT
%token LAMBDA
%token ARROW 
%token COMMA 
%token UNDERSCORE
%token SEMI 
%token BAR 
%token OF_TYPE 
(* Semantic Delimiters *)
%token LPAREN  RPAREN 
%token LBRACE  RBRACE  
(* Operators *)
%token LKET    RKET  
%token CONCAT 
     
%token <string>INTCONST 
%token <string>FLOATCONST
%token <string>STRCONST

%token <string>VARID
%token <string>CONID
%token <string>QVARID
%token <string>QCONID
(* Layout Sensitive Transformation *)
%token LDELIM             (* {{  *)
%token RDELIM             (* }}  *)

%token PREPEND
%token GT LT GEQ LEQ
%token PLUS MINUS
%token TIMES DIV

(* Arithematics *)
%nonassoc GT LT GEQ LEQ EQ
%right PREPEND
%left PLUS MINUS
%left TIMES DIV

(* Unused/intermedia tokens to supress compiler warnings *)
%token EOF 
%token <int>REQ_INDENT 
%token <int>WTH_INDENT

%start <ElAst.Alias.Module.m> main

%%

main:
| LDELIM module_decl SEMI body RDELIM                                               { Module.Mod (Some $2, fst $4, snd $4) }
| LDELIM body RDELIM                                                                { Module.Mod (None, fst $2, snd $2) }

module_decl :
| MODULE c=CONID                                                                    { mod_decl (con c $loc(c), None)    $loc }
| MODULE c=CONID exposing                                                           { mod_decl (con c $loc(c), Some $3) $loc }

body:
| impdecl SEMI body                                                                 { add_import $3 $1 }
| separated_list(SEMI, topdecl)                                                     { mod_decls $1     }

impdecl:
| IMPORT qc=modid as_con=option(as_con) ex=option(exposing)                          { mod_import (qc, as_con, ex) $loc} 

as_con:
| AS CONID                                                                          { con $2 $loc             }

exposing:
| EXPOSING LPAREN DOTDOT RPAREN                                                     { Module.Any              }
| EXPOSING LPAREN ids=separated_nonempty_list(COMMA, id) RPAREN                     { Module.Idents ids       }

topdecl:
| TYPE                                                                              { assert false       }
| decl                                                                              { $1                 }

decls:
| LDELIM separated_nonempty_list(SEMI, decl) RDELIM                                 { $2 }

// - Elm: Type annotations must directly precede expression 
// - Elm: Top-level binding does not support bind to pattern but we are going to allow it
// - Elm: Type annotations are not allowed in let bindings but we allow them here
decl :
| gendecl                                                                           { $1 }
| udecl                                                                             { $1 }

udecl:
| funlhs rhs                                                                        { node (Decl.Fun ($1, $2)) $loc   }
| pat    rhs                                                                        { node (Decl.Pat ($1, $2)) $loc   }

gendecl :
| var OF_TYPE type_                                                                 { node (Decl.Annot ($1, $3)) $loc }

// type is an OCaml keyword
type_:
| LPAREN RPAREN                                                                     { node Typ.Unit $loc }

// Function declaration
// - Elm does not support custom operators
// - Haskell allows for function declarations of form 
//     let (f x) y = .. in .. 
//   which is functionally equivalent to 
//     let f x y = .. in ..
//   but provided for a degree of syntatic clarity. Elm forbids that.
//   We will be allowing this form while *not* tracking parenthese
funlhs :
| var nonempty_list(apat)                                                           { ($1, $2)     }
| LPAREN funlhs RPAREN nonempty_list (apat)                                         { assert false }
// | pat varop pat                                                                     { 1 }

rhs:
| EQ e=exp                                                                          { e            }
// | gdrhs 

// Expression sub language

exp:
| LAMBDA nonempty_list(apat) ARROW exp                                              { expr (Expr.Lambda ($2, $4))   $loc }
| LET decls=decls IN e=exp                                                          { expr (Expr.Let (decls, e))    $loc }
| IF e1=exp THEN e2=exp ELSE e3=exp                                                 { expr (Expr.If (e1, (e2, e3))) $loc }
| CASE e=exp OF LDELIM alts=separated_list(SEMI, alt) RDELIM                        { expr (Expr.Case (e, alts))    $loc }
| infixexp                                                                          { $1 }

fexp:
| aexp nonempty_list(aexp)                                                          { expr (Expr.App ($1, $2)) $loc  }

infixexp:
| infixexp qop infixexp                                                             { expr (op2expr $2 $1 $3) $loc   }
| MINUS aexp                                                                        { assert false }
| fexp                                                                              { $1 }
| aexp                                                                              { $1 }

aexp :
| qvar                                                                              { expr (Expr.Var $1)        $loc }
| gcon                                                                              { expr (gcon2expr $1)       $loc }
| literal                                                                           { expr (Expr.Literal $1)    $loc }
| LPAREN exp RPAREN                                                                 { wrapped $2                $loc }
| LPAREN e = exp COMMA es =separated_nonempty_list(COMMA, exp) RPAREN               { expr (Expr.Tuple (e::es)) $loc }
| LKET   es = separated_nonempty_list(COMMA, exp) RKET                              { expr (Expr.List es)       $loc }
| LBRACE separated_list(COMMA, fbind) RBRACE                                        { expr (Expr.Record $2)     $loc }

/* Elm forbids uses of operator such a (+ 1) and (+ 2) but allows (+) */ 
| LPAREN qop RPAREN                                                                 { expr (op2opfunc $2)       $loc }
// | LPAREN infixexp qop RPAREN                                                        { 1 }
// | LPAREN qop_no_minus infixexp RPAREN                                               { 1 }

%inline qop:
| PLUS                                                                              { Expr.PLUS    }
| MINUS                                                                             { Expr.MINUS   }
| TIMES                                                                             { Expr.TIMES   }
| DIV                                                                               { Expr.DIV     }
| EQ                                                                                { Expr.EQ      }
| GT                                                                                { Expr.GT      }
| GEQ                                                                               { Expr.GEQ     }
| LT                                                                                { Expr.LT      }
| LEQ                                                                               { Expr.LEQ     }
//| PREPEND                                                                           { assert false }

// Global constructors
gcon:
| LPAREN RPAREN                                                                     { Unit               }
| LKET   RKET                                                                       { EmptyList          }
| qcon                                                                              { QCon $1            }

// Casing branches
alt:
| pat ARROW exp                                                                     { ($1, $3)           }

// This is syntax for record expressions
//     { field = e1, field = e2 }
// Haskell seems to allow qualified field to disambiguous, but Elm forbids such syntax
// hence the it is attempting to write the this rule: 
//   fbind: var EQ exp
// However it must be understood that here the identifier does not reference a binded place,
// nor does it reference a qualified variable from another module. It's just a label of variable-like syntax.
// Hence VARID should be used
fbind :
| v=VARID EQ e=exp                                                                  { (field v $loc(v), e)              }

// Pattern language 
pat:
| lpat                                                                              { $1 }

// Haskell syntax allows for gcon instead of qcon in non atomic rule possibly 
// for support in function arguments. Let's restrict the patter to qcon
// for now and see how it goes
lpat: 
// | apat                                                                              { pat (Pattern.Any) $loc }                                
| apat                                                                              { $1 }                                
| qcon nonempty_list(apat)                                                          { pat (Pattern.Ctor ($1, $2))  $loc }

apat:
| var                                                                               { pat (Pattern.Var $1)         $loc }
| gcon                                                                              { pat (gcon2pat $1)            $loc }
| LBRACE separated_list(COMMA, var) RBRACE                                          { assert false                      }
| literal                                                                           { pat (Pattern.Literal $1)     $loc }
| UNDERSCORE                                                                        { pat (Pattern.Any)            $loc }
| LPAREN p=pat COMMA ps=separated_nonempty_list(COMMA, pat) RPAREN                  { pat (Pattern.Tuple (p::ps))  $loc }
| LKET   ps=separated_nonempty_list(COMMA, pat) RKET                                { pat (Pattern.List ps)        $loc }
// There is no realistic way to factor patterns (or types) so we will not 
// track its parenthese depths
| LPAREN pat RPAREN                                                                 { $2                                }

// Identifiers 
id:
| VARID                                                                             { id_var (var $1 $loc)         }
| CONID                                                                             { id_abstycon (con $1 $loc)              }
| CONID LPAREN DOTDOT RPAREN                                                        { id_tycon (con $1 $loc)            }

var:
| VARID                                                                             { var $1 $loc }

qvar:
| qvarid                                                                            { $1 }

qcon:
| qconid                                                                            { $1 }

// literals
literal:
| INTCONST                                                                          { node (Literal.Int $1)    $loc }
| FLOATCONST                                                                        { node (Literal.Float $1)  $loc }
| STRCONST                                                                          { node (Literal.String $1) $loc }

// ========= "Inlined" lexical syntax ========== 
modid:
| QCONID                                                                            { parse_modid $1 $loc }
| CONID                                                                             { parse_modid $1 $loc }

qconid:
| QCONID                                                                            { parse_qcon $1 $loc            }
| CONID                                                                             { qcon (None, con $1 $loc) $loc }

qvarid:
| QVARID                                                                            { parse_qvar $1 $loc            }
| VARID                                                                             { qvar (None, var $1 $loc) $loc }