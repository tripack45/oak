
type t = 
    (* Keywords *)
    | MODULE | EXPOSING | IMPORT | AS
    | CASE   | OF
    | LET    | IN
    | IF     | THEN | ELSE 
    | TYPE
    | ALIAS
    (* Lexical Delimiters *)
    | EQ
    | ARROW
    | COMMA
    | LAMBDA           (* \  *)
    | SEMI             (* ;  *)
    | BAR              (* |  *)
    | OF_TYPE          (* :  *)
    (* Semantic Operators *)
    | DOT              (* .  *)
    | DOTDOT           (* .. *)
    | LPAREN | RPAREN  (* () *)
    | LKET   | RKET    (* [] *)
    | LBRACE | RBRACE  (* {} *)
    | UNDERSCORE       (* _  *)
    | PREPEND          (* :: *)
    | CONCAT           (* ++ *)
    | LPIPE            (* <| *)
    | RPIPE            (* |> *)
    | LCOMBINATOR      (* << *)
    | RCOMBINATOR      (* >> *)
    (* Arithematics *)
    | PLUS  | MINUS
    | TIMES | DIV
    | GT    | LT
    | GEQ   | LEQ
    (* Literals *)
    | INTCONST   of string
    | FLOATCONST of string
    | STRCONST   of string
    (* Identifiers *)
    | VARID     of string
    | CONID     of string
    | QVARID    of string
    | QCONID    of string
    (* Layout Sensitive Transformation *)
    | EOF                (* eof *)
    | LDELIM             (* {{  *)
    | RDELIM             (* }}  *)
    | REQ_INDENT of int  (* {n} *)
    | WTH_INDENT of int  (* <n> *)

(* Defined for menhir*)
type token = t

let to_string tok=  
  match tok with
| IMPORT  -> "import"
| MODULE  -> "module"
| EXPOSING -> "exposing"
| AS      -> "as"
| CASE    -> "case"
| OF      -> "of"
| LET     -> "let"
| IN      -> "in"
| IF      -> "if"
| THEN    -> "then"
| ELSE    -> "else"
| TYPE    -> "type"
| ALIAS   -> "alias"
| EQ      -> "="
| ARROW   -> "->"
| COMMA   -> ","
| BAR     -> "|"
| OF_TYPE -> ":"
| DOT     -> "."        
| DOTDOT  -> ".."
| LPAREN  -> "("
| RPAREN  -> ")"
| LKET    -> "["
| RKET    -> "]"
| LBRACE  -> "{"
| RBRACE  -> "}"
| LAMBDA  -> "\\"
| UNDERSCORE  -> "_"
| PREPEND     -> "::"
| CONCAT      -> "++"
| LPIPE       -> "<|"
| RPIPE       -> "|>"
| LCOMBINATOR -> "<<"
| RCOMBINATOR -> ">>"
| PLUS    -> "+"
| MINUS   -> "-"
| TIMES   -> "*"
| DIV     -> "/"
| GT      -> ">"
| LT      -> "<"
| GEQ     -> ">="
| LEQ     -> "<="
| INTCONST   s -> s ^ "_d"
| FLOATCONST s -> s ^ "_f"
| STRCONST   s -> "\"" ^ s  ^ "\""
| VARID      s -> "Var:" ^ s 
| CONID      s -> "Con:" ^ s
| QVARID     s -> "QVar:" ^ s 
| QCONID     s -> "QCon:" ^ s
| EOF          -> "EOF"
| SEMI         -> ";"
| LDELIM       -> "{%"
| RDELIM       -> "%}"
| REQ_INDENT i -> Printf.sprintf "{%d}" i
| WTH_INDENT i -> Printf.sprintf "<%d>" i

let to_parse_string tok = 
  match tok with
| IMPORT   -> "IMPORT"
| MODULE   -> "MODULE"
| EXPOSING -> "EXPOSING"
| AS       -> "AS"
| CASE     -> "CASE"
| OF       -> "OF"
| LET      -> "LET"
| IN       -> "IN"
| IF       -> "IF"
| THEN     -> "THEN"
| ELSE     -> "ELSE"
| TYPE     -> "TYPE"
| ALIAS    -> "ALIAS"
| EQ       -> "EQ"
| ARROW    -> "ARROW"
| COMMA    -> "COMMA"
| BAR      -> "BAR"
| OF_TYPE  -> "OF_TYPE"
| DOT      -> "DOT"        
| DOTDOT   -> "DOTDOT"
| LPAREN   -> "LPAREN"
| RPAREN   -> "RPAREN"
| LKET     -> "LKET"
| RKET     -> "RKET"
| LBRACE   -> "LBRACE"
| RBRACE   -> "RBRACE"
| LAMBDA   -> "LAMBDA"
| UNDERSCORE  -> "UNDERSCORE"
| PREPEND     -> "PREPEND"
| CONCAT      -> "CONCAT"
| LPIPE       -> "LPIPE"
| RPIPE       -> "RPIPE"
| LCOMBINATOR -> "LCOMBINATOR"
| RCOMBINATOR -> "RCOMBINATOR"
| PLUS        -> "PLUS"
| MINUS       -> "MINUS"
| TIMES       -> "TIMES"
| DIV         -> "DIV"
| GT          -> "GT"
| LT          -> "LT"
| GEQ         -> "GEQ"
| LEQ         -> "LEQ"
| INTCONST   _ -> "INTCONST"
| FLOATCONST _ -> "FLOATCONST"
| STRCONST   _ -> "STRCONST"
| VARID      _ -> "VARID"
| CONID      _ -> "CONID"
| QVARID     _ -> "QVARID"
| QCONID     _ -> "QCONID"
| EOF          -> "EOF"
| SEMI         -> "SEMI"
| LDELIM       -> "LDELIM"
| RDELIM       -> "RDELIM"
| REQ_INDENT _ -> "REQ_INDENT"
| WTH_INDENT _ -> "WTH_INDENT"

let is_lexeme tok = 
  match tok with 
  | EOF
  | LDELIM | RDELIM
  | REQ_INDENT _ | WTH_INDENT _ -> false
  | _ -> true

exception Eof

exception UnmatchedToken of char


