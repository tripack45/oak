type t = 
  (* Keywords *)
  | MODULE | EXPOSING | IMPORT | AS | PORT
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

 (* infix right 0 (<|)   = apL
  * infix left  0 (|>)   = apR
  * infix right 2 (||)   = or
  * infix right 3 (&&)   = and
  * infix non   4 (==)   = eq
  * infix non   4 (/=)   = neq
  * infix non   4 (<)    = lt
  * infix non   4 (>)    = gt
  * infix non   4 (<=)   = le
  * infix non   4 (>=)   = ge
  * infix right 5 (::)   = cons
  * infix right 5 (++)   = append
  * infix left  6 (+)    = add
  * infix left  6 (-)    = sub
  * infix left  7 (\*\)  = mul
  * infix left  7 (/)    = fdiv
  * infix left  7 (//)   = idiv
  * infix right 8 (^)    = pow
  * infix left  9 (<<)   = composeL
  * infix right 9 (>>)   = composeR
  *)

  | APL                 (* <| *)
  | APR                 (* |> *)
  | OR                  (* || *)
  | AND                 (* && *)
  | EQU                 (* == *)
  | NE                  (* /= *)
  | GT    | LT          (* >  *)
  | GEQ   | LEQ         (* >= *)
  | CONS                (* :: *)
  | APPEND              (* ++ *)
  (* Arithematics *)
  | PLUS  | MINUS
  | TIMES 
  | FDIV                (* /  *)
  | IDIV                (* // *)
  | POW                 (* ^  *)
  | COMPOSEL            (* << *)
  | COMPOSER            (* >> *)
  (* Literals *)
  | INTCONST   of string
  | FLOATCONST of string
  | CHARCONST  of string
  | STRCONST   of string
  (* Comments *)
  | LCOMMENT   of (Lexing.position * Lexing.position, string) Core.Tuple2.t
  | BCOMMENT   of (Lexing.position * Lexing.position, string) Core.Tuple2.t
  (* ProjFunc; Elm *)
  | PROJ_FUNC  of string
  | PROJECT    of string
  (* Identifiers *)
  | VARID      of string
  | CONID      of string
  | QVARID     of string
  | QCONID     of string
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
| PORT    -> "port"
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
| UNDERSCORE -> "_"
| APL     -> "<|"
| APR     -> "|>"
| OR      -> "||"
| AND     -> "&&"
| EQU     -> "=="
| NE      -> "/="
| GT      -> ">"
| LT      -> "<"
| GEQ     -> ">="
| LEQ     -> "<="
| CONS    -> "::"
| APPEND  -> "++"
(* Arithematics *)
| PLUS     -> "+"
| MINUS    -> "-"
| TIMES    -> "*"
| FDIV     -> "/"
| IDIV     -> "//"
| POW      -> "^"
| COMPOSEL -> "<<"
| COMPOSER -> ">>"
| INTCONST   s -> s ^ "_d"
| FLOATCONST s -> s ^ "_f"
| CHARCONST  s -> "\'" ^ s ^ "\'"
| STRCONST   s -> "\"" ^ s ^ "\""
| LCOMMENT (_, s) -> "--" ^ s ^ "\n"
| BCOMMENT (_, s) -> "{-" ^ s ^ "-}"
| PROJ_FUNC  s -> "PROJ_FUNC:" ^ s 
| PROJECT    s -> "PROJECT:" ^ s 
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
| PORT     -> "PORT"
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
| UNDERSCORE -> "UNDERSCORE"
| APL      -> "APL"
| APR      -> "APR"
| OR       -> "OR"
| AND      -> "AND"
| EQU      -> "EQU"
| NE       -> "NE"
| GT       -> "GT"
| LT       -> "LT"
| GEQ      -> "GEQ"
| LEQ      -> "LEQ"
| CONS     -> "CONS"
| APPEND   -> "APPEND"
(* Arithematics *)
| PLUS     -> "PLUS"
| MINUS    -> "MINUS"
| TIMES    -> "TIMES"
| FDIV     -> "FDIV"
| IDIV     -> "IDIV"
| POW      -> "POW"
| COMPOSEL -> "COMPOSEL"
| COMPOSER -> "COMPOSER"
| INTCONST   _ -> "INTCONST"
| FLOATCONST _ -> "FLOATCONST"
| CHARCONST  _ -> "CHARCONST"
| STRCONST   _ -> "STRCONST"
| LCOMMENT   _ -> "LCOMMENT"
| BCOMMENT   _ -> "BCOMMENT"
| PROJ_FUNC  _ -> "PROJ_FUNC"
| PROJECT    _ -> "PROJECT"
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


