%{
  
module Util =
struct
  open ElAst
  open Syntax
  open Node

  let naked n pos = node n (pos, Naked)
  let wrapped par_node pos' : 't par_node = 
    let f = function
            | (_pos, Naked)     -> (pos', Wrapped 1)
            | (_pos, Wrapped i) -> (pos', Wrapped (i + 1))
    in map_attr f par_node

  let expr n pos : expr' = naked n pos
  let pat  n pos : pat'  = node n pos

  (* Identifiers *)
  let var name pos   : var'   = node (VarId.of_string name) pos
  let tvar name pos  : tvar'  = node (TVarId.of_string name) pos
  let mcon name pos  : mcon'  = node (MConId.of_string name) pos
  let tycon name pos : tycon' = node (TyConId.of_string name) pos
  let dcon name pos  : dcon'  = node (DConId.of_string name) pos
  
  let id_var var      : exposing_ident = Var var
  let id_tycon con    : exposing_ident = TyCon con
  let id_abstycon con : exposing_ident = AbsTyCon con

  let field name pos : field' = node (FieldId.of_string name) pos

  let qvar   (path, var) pos : qvar'   = node (QVar (path, var)) pos
  let qtycon (path, con) pos : qtycon' = node (QTyCon (path, con)) pos
  let qdcon  (path, con) pos : qdcon'  = node (QDCon (path, con)) pos

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
      | [Tokens.CONID con]       -> Just (MConId.of_string con)
      | (Tokens.CONID con)::cons -> More (MConId.of_string con, module_path cons)
      | _ -> assert false

  let parse_modid (s : string) (pos : pos) : path' =
    let path =
      (Lex.LayoutSensitiveLexer.explode_qualified_name s (fst pos))
      |> Core.List.map ~f:fst
      |> module_path
    in node path pos

  let parse_qid fid fqid (s : string) (pos : pos) =
    let (cons, poss, (id_tok, id_pos)) = parse_qualified s pos in
    let path_node = node (module_path cons) (List.hd poss, List.hd (List.rev poss)) in
    match id_tok with 
    | Tokens.VARID id
    | Tokens.CONID id -> 
      fqid (Some path_node, fid id id_pos) pos
    | _ -> assert false
      
  let parse_qvar (s : string) (pos : pos) : qvar' = 
    parse_qid var qvar s pos

  let parse_qdcon (s : string) (pos : pos) : qdcon' = 
    parse_qid dcon qdcon s pos

  let parse_qtycon (s : string) (pos : pos) : qtycon' = 
    parse_qid tycon qtycon s pos

  (* Operator intermediate value *)
  let op2expr op e1 e2 = Infix (op, e1, e2) 

  let op2opfunc op = OpFunc op

  (* Intermediate values for atomic constructors 
  * This is needed because those constructors, though syntatically sharing the same node
  * can either serve as a value in an expresion, or an atomic pattern *)
  type gcon = | Unit | EmptyList | QDCon of qdcon'

  let gcon2pat g : pat =
    match g with 
    | QDCon c   -> Con (c, [])
    | Unit      -> Unit
    | EmptyList -> EmptyList

  let gcon2expr g : expr =
    match g with 
    | Unit       -> Unit
    | EmptyList  -> List []
    | QDCon c    -> Con (c, [])

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
%token IMPORT MODULE EXPOSING AS PORT
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

(* Operators *)
%token APL APR
%token OR AND
%token EQU NE GT LT GEQ LEQ
%token CONS APPEND 
%token PLUS MINUS TIMES FDIV IDIV POW
%token COMPOSEL COMPOSER

%left APL
%right APR
%left OR AND
%nonassoc EQU NE GT LT GEQ LEQ
%right CONS APPEND
%left PLUS MINUS TIMES FDIV IDIV POW
%left COMPOSEL
%right COMPOSER

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
| option(PORT) MODULE c=CONID ex=option(exposing)                                   { mod_decl (mcon c $loc(c), ex) $loc }

body:
| impdecl SEMI body                                                                 { add_import $3 $1 }
| separated_list(SEMI, topdecl)                                                     { mod_decls $1     }
| impdecl                                                                           { add_import ([], []) $1 }

impdecl:
| IMPORT qc=modid as_con=option(as_con) ex=option(exposing)                         { mod_import (qc, as_con, ex) $loc} 

as_con:
| AS CONID                                                                          { mcon $2 $loc             }

exposing:
| EXPOSING LPAREN DOTDOT RPAREN                                                     { Module.Any              }
| EXPOSING LPAREN ids=separated_nonempty_list(COMMA, id) RPAREN                     { Module.Idents ids       }

topdecl:
| typdecl                                                                           { $1                      }
| decl                                                                              { $1                      }

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
| PORT var OF_TYPE type_                                                            { node (Decl.Port  ($2, $4)) $loc }

typdecl:
| TYPE ALIAS simpletype EQ type_                                                    { node (Decl.Alias ($3, $5)) $loc }
| TYPE simpletype EQ separated_nonempty_list(BAR, constr)                           { node (Decl.TyCon ($2, $4)) $loc }

simpletype:
| tycon list(tvar)                                                                  { ($1, $2)           }

constr:
| dcon list(atype)                                                                  { ($1, $2)           }

// type is an OCaml keyword
// Function type
type_ :
| btype ARROW type_                                                                 { node (Typ.Arrow ($1, $3)) $loc }
| btype                                                                             { $1                             }

// Type application
btype:
| btype atype                                                                       { node (Typ.TApp ($1, $2)) $loc  }
| atype                                                                             { $1                             }

// Elm does not have [TyCon] syntax for Lists, but it could be nice to have
atype:
| tvar                                                                              { node (Typ.TVar $1)       $loc  }
| gtycon                                                                            { $1                             }
| LPAREN t=type_ COMMA ts=separated_nonempty_list(COMMA, type_) RPAREN              { node (Typ.Tuple (t::ts)) $loc  }
| LKET type_ RKET                                                                   { assert false                   }
| LBRACE row RBRACE                                                                 { node (Typ.Record $2)     $loc  }
| LPAREN type_ RPAREN                                                               { $2                             }

gtycon:
| qtycon                                                                            { node (Typ.TyCon $1) $loc       }
| LPAREN RPAREN                                                                     { node (Typ.Unit) $loc           }
/*
| LKET RKET                                                                         { node (Typ.EmptyList)     $loc  }
*/

row : 
| tvar                                                                              { Typ.RVar $1                    } 
| row BAR fields                                                                    { Typ.Extension ($1, $3)         }
| fields                                                                            { Typ.Fields $1                  }

fields:
| separated_list(COMMA, field)                                                      { $1                             } 

field :
| fieldid OF_TYPE type_                                                             { ($1, $3)                       }

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
// 
// The Haskell expression language (by extenstin Elm) allows control flow expressions to be under infix operators,
// this naturally causes tons of room for ambiguities, to which the Haskell syntax guide says (quote):
//
//    In both the lexical and the context-free syntax, there are some ambiguities that are to be resolved by making 
//    grammatical phrases as long as possible, proceeding from left to right (in shift-reduce parsing, resolving 
//    shift/reduce conflicts by shifting). In the lexical syntax, this is the “maximal munch” rule. In the context-free 
//    syntax, this means that conditionals, let-expressions, and lambda abstractions extend to the right as far as possible.
//
// Examples of expressions allowed include:
//   1 + if True then 1 else 2 + 3     parses as       1 + (if True then 1 else (2 + 3))
//   2 + 3 * let x = 1 in x * 3 + 2    parses as       2 + 3 * (let x = 1 in ((x * 3) + 2))
//
// "Extend to the right as far as possible" is not very precise in terms of CFG rules. 
//
// This parser interpretes the rule as following: Expressions must either not contain control flow operators entirely, or
// the control flow expression must appear at the right most leaf of the expression parse tree. 
//
exp:
| infixexp_exp                                                                      { $1 }
| infixexp                                                                          { $1 } 

cexp:
| LAMBDA nonempty_list(apat) ARROW exp                                              { expr (Expr.Lambda ($2, $4))   $loc }
| LET decls=decls IN e=exp                                                          { expr (Expr.Let (decls, e))    $loc }
| IF e1=exp THEN e2=exp ELSE e3=exp                                                 { expr (Expr.If (e1, (e2, e3))) $loc }
| CASE e=exp OF LDELIM alts=separated_list(SEMI, alt) RDELIM                        { expr (Expr.Case (e, alts))    $loc }

infixexp_exp:
| infixexp qop infixexp_exp                                                         { expr (op2expr $2 $1 $3) $loc }
| cexp                                                                              { $1 }

infixexp:
| infixexp qop infixexp                                                             { expr (op2expr $2 $1 $3) $loc   }
| MINUS aexp                                                                        { assert false }
| fexp                                                                              { $1 }
| aexp                                                                              { $1 }

fexp:
| aexp nonempty_list(aexp)                                                          { expr (Expr.App ($1, $2)) $loc  }

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
| APL                                                                               { Expr.APL      }
| APR                                                                               { Expr.APR      }
| OR                                                                                { Expr.OR      }
| AND                                                                               { Expr.AND     }
| EQU                                                                               { Expr.EQU     }
| NE                                                                                { Expr.NE      }
| GT                                                                                { Expr.GT      }
| LT                                                                                { Expr.LT      }
| GEQ                                                                               { Expr.GEQ     }
| LEQ                                                                               { Expr.LEQ     }
| CONS                                                                              { Expr.CONS     }
| APPEND                                                                            { Expr.APPEND   }
| PLUS                                                                              { Expr.PLUS    }
| MINUS                                                                             { Expr.MINUS   }
| TIMES                                                                             { Expr.TIMES   }
| FDIV                                                                              { Expr.FDIV    }
| IDIV                                                                              { Expr.IDIV    }
| POW                                                                               { Expr.POW     }
| COMPOSEL                                                                          { Expr.COMPOSEL }
| COMPOSER                                                                          { Expr.COMPOSER }

// Global constructors
gcon:
| LPAREN RPAREN                                                                     { Unit               }
| LKET   RKET                                                                       { EmptyList          }
| qdcon                                                                             { QDCon $1           }

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
| fieldid EQ e=exp                                                                  { ($1, e)              }

// Pattern language 
pat:
| lpat                                                                              { $1 }

// Haskell syntax allows for gcon instead of qcon in non atomic rule possibly 
// for support in function arguments. Let's restrict the patter to qcon
// for now and see how it goes
lpat: 
// | apat                                                                              { pat (Pattern.Any) $loc }                                
| apat                                                                              { $1 }                                
| qdcon nonempty_list(apat)                                                         { pat (Pattern.Con ($1, $2))   $loc }

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
| var                                                                               { id_var $1                         }
| tycon                                                                             { id_abstycon $1                    }
| tycon LPAREN DOTDOT RPAREN                                                        { id_tycon $1                       }

tvar:
| VARID                                                                             { tvar $1 $loc }

fieldid:
| VARID                                                                             { field $1 $loc }

var:
| VARID                                                                             { var $1 $loc }

tycon:
| CONID                                                                             { tycon $1 $loc }

dcon:
| CONID                                                                             { dcon $1 $loc }

qvar:
| qvarid                                                                            { $1 }

qtycon:
| qtyconid                                                                          { $1 }

qdcon:
| qdconid                                                                           { $1 }

// literals
literal:
| INTCONST                                                                          { node (Literal.Int $1)    $loc }
| FLOATCONST                                                                        { node (Literal.Float $1)  $loc }
| STRCONST                                                                          { node (Literal.String $1) $loc }

modid:
| QCONID                                                                            { parse_modid $1 $loc }
| CONID                                                                             { parse_modid $1 $loc }

qdconid:
| QCONID                                                                            { parse_qdcon $1 $loc            }
| dcon                                                                              { qdcon (None, $1) $loc          }

qtyconid:
| QCONID                                                                            { parse_qtycon $1 $loc           }
| tycon                                                                             { qtycon (None, $1) $loc         }

qvarid:
| QVARID                                                                            { parse_qvar $1 $loc             }
| var                                                                               { qvar (None, $1) $loc           }