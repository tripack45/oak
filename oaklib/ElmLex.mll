{
module Make (T : module type of Tokens)
= struct

module L = Lexing

(* let line_table = ref []
let block_table = ref [] *)

}

let varid = ['a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let conid = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let modid = (conid ".")* conid
let decnum = ("0" | ['1'-'9'](['0'-'9']*))
let hexnum = "0x" ("0" | ['1'-'9' 'A'-'F' 'a'-'f'](['0'-'9' 'A'-'F' 'a'-'f']*))
let intnum = decnum | hexnum
let floatnum = (['0'-'9']* '.'? ['0'-'9']+ (['e' 'E'] ['-' '+']? ['0'-'9']+)?)

let char = (('\\' _) | [^'\''])
let str = (('\\' _) | [^'\"'])*
let strTri = (('\\' _) | [^'\"'] | '\"' [^'\"'] | '\"' '\"' [^'\"'])*

let ws = [' ' '\t' '\r' '\011' '\012'] 

rule initial =
  parse
    ws+           { initial lexbuf }
    
  | '-' '-'+      { line_tag (L.lexeme_start_p lexbuf) [] lexbuf }
  | "{-"          { nested_block 0 (L.lexeme_start_p lexbuf) [] lexbuf }
  | '\n'          { (L.new_line lexbuf; initial lexbuf) }

  | "import"      { T.IMPORT }
  | "module"      { T.MODULE }
  | "exposing"    { T.EXPOSING }
  | "as"          { T.AS }
  | "port"        { T.PORT }
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
  (* | '.'         { T.DOT } *)
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

  | intnum as n                   { T.INTCONST   n }
  | floatnum as n                 { T.FLOATCONST n }
  | "\'" char as c "\'"           { T.CHARCONST  c }
  | "\"\"\"" strTri as s "\"\"\"" { T.STRCONST   s }
  | "\"" str as s "\""            { T.STRCONST   s }

  (* PROJ_FUNC; PROJECT is only yielded from lexing optional path *)
  | '.' (varid as f)          { T.PROJ_FUNC f }

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

and line_tag start phrasebuf =
    parse eof { T.EOF }
  | '\n'      { (
                (* line_table := [((start, L.lexeme_end_p lexbuf), Core.String.of_char_list @@ List.rev phrasebuf)] :: !line_table;  *)
                L.new_line lexbuf; initial lexbuf
              ) }
  | _ as c    { line_tag start (c::phrasebuf) lexbuf }

and nested_block depth start phrasebuf =
    parse eof { assert false }
  | "{-"      { nested_block (depth+1) start ('-'::'{'::phrasebuf) lexbuf }
  | "-}"      { if depth > 0
                then nested_block (depth-1) start ('}'::'-'::phrasebuf) lexbuf
                else (
                  (* block_table := [((start, L.lexeme_end_p lexbuf), Core.String.of_char_list @@ List.rev phrasebuf)] :: !block_table;  *)
                  initial lexbuf
                ) }
  | '\n'      { L.new_line lexbuf; nested_block depth start ('\n'::phrasebuf) lexbuf }
  | _ as c    { nested_block depth start (c::phrasebuf) lexbuf }


(* TRAILER *)
{
end
}