{
module Make (T : module type of Tokens)
= struct

module L = Lexing
}

let varid = ['a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let conid = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let modid = (conid ".")* conid
let decnum = ("0" | ['1'-'9'](['0'-'9']*))

let ws = [' ' '\t' '\r' '\011' '\012'] 

rule initial =
  parse
    ws+           { initial lexbuf }
  | '\n'          { (L.new_line lexbuf; initial lexbuf) }

  | "import"      { T.IMPORT }
  | "module"      { T.MODULE }
  | "exposing"    { T.EXPOSING }
  | "as"          { T.AS }
  | "case"        { T.CASE }
  | "of"          { T.OF }
  | "let"         { T.LET }
  | "in"          { T.IN }
  | "if"          { T.IF }
  | "then"        { T.THEN }
  | "else"        { T.ELSE }
  | "type"        { T.TYPE }
  | "alias"       { T.ALIAS }

  | '='         { T.EQ    }
  | "->"        { T.ARROW }
  | ','         { T.COMMA }
  | '\\'        { T.LAMBDA }
  | '.'         { T.DOT }
  | '|'         { T.BAR   }
  | ":"         { T.OF_TYPE }
  | ".."        { T.DOTDOT }

  | '('         { T.LPAREN }
  | ')'         { T.RPAREN }
  | '['         { T.LKET }
  | ']'         { T.RKET }
  | '{'         { T.LBRACE }
  | '}'         { T.RBRACE }

  | '_'         { T.UNDERSCORE }

  | "<|"        { T.APL }
  | "|>"        { T.APR }

  | "||"        { T.OR  }
  | "&&"        { T.AND }
  | "=="        { T.EQU }
  | "/="        { T.NE  }
  | '>'         { T.GT  }
  | '<'         { T.LT  }
  | ">="        { T.GEQ }
  | "<="        { T.LEQ }

  | "::"        { T.CONS }
  | "++"        { T.APPEND }

  | '+'         { T.PLUS  }
  | '-'         { T.MINUS }
  | '*'         { T.TIMES }
  | '/'         { T.FDIV  }
  | "//"        { T.IDIV  }
  | '^'         { T.POW   }

  | "<<"        { T.COMPOSEL }
  | ">>"        { T.COMPOSER }

  | ';'         { T.SEMI      } 
  | "{%"        { T.LDELIM    } 
  | "%}"        { T.RDELIM    } 

  | decnum as n   { T.INTCONST n }

  | "\"" ([^'\"']* as s) "\"" { T.STRCONST s}

  (* Spaces are not allowed in qualified names such as M.N.Cons or M.N.P *)
  | (modid '.' conid) as name { T.QCONID name }
  | (modid '.' varid) as name { T.QVARID name }
  | conid as name             { T.CONID name  }
  | varid as name             { T.VARID name  }

  | eof         { T.EOF }

  | _ as s      { raise (T.UnmatchedToken s) }

(* Used by parser to further breakup qualified ids *)
and qcon_var = 
  parse  
      (conid as name) '.'       { T.CONID name }
    | conid as name             { T.CONID name }
    | varid as name             { T.VARID name }
    | eof                       { T.EOF        }
    | _ as s                    { raise (T.UnmatchedToken s) }

(* TRAILER *)
{
end
}