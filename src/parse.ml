open Lexing

(* Parser Driver *)
module I = ElmParse.MenhirInterpreter

exception ParseFailure

let parse_chn chn =
  let lexbuf = Lexing.from_channel ~with_positions:true chn in
  let t = Lex.LayoutSensitiveLexer.from_lexbuf lexbuf in
  let init = ElmParse.Incremental.main lexbuf.lex_curr_p in
  (* Loop functions *)
  let supplier () = 
    let (tok, (pos_s, pos_e)) = Lex.LayoutSensitiveLexer.take_expanded t in
    (tok, pos_s, pos_e)
  in
  let succeed v = v in
  let fail _ = raise ParseFailure in
  I.loop_handle succeed fail supplier init

let parse_src src =
  let lexbuf = Lexing.from_string ~with_positions:true (Src.Source.raw src) in
  let _ = Lexing.set_filename lexbuf (Src.Source.name src) in
  let t = Lex.LayoutSensitiveLexer.from_lexbuf lexbuf in
  let init = ElmParse.Incremental.main lexbuf.lex_curr_p in
  (* Loop functions *)
  let supplier () = 
    let (tok, (pos_s, pos_e)) = Lex.LayoutSensitiveLexer.take_expanded t in
    (tok, pos_s, pos_e)
  in
  let succeed v = v in
  let fail _ = raise ParseFailure in
  I.loop_handle succeed fail supplier init

(*
let parse_chn chn =
  let lexbuf = Lexing.from_channel ~with_positions:true chn in
  let t = Lex.LayoutSensitiveLexer.from_lexbuf lexbuf in
  let init = ElmParse.Incremental.main lexbuf.lex_curr_p in
  (* A hack. Ideally we would query the lexer for this information 
   * but we are being lazy here by just saving the results with refernces.*)
  let (last_s, last_e) = (ref Lexing.dummy_pos, ref Lexing.dummy_pos) in
  (* Loop functions *)
  let supplier () = 
    let (tok, (pos_s, pos_e)) = Lex.LayoutSensitiveLexer.take_expanded t in
    (* See fail function for their uses *)
    last_s := pos_s;
    last_e := pos_e;
    (tok, pos_s, pos_e)
  in
  let succeed v = (* Printf.printf "parse completed\n"; *) v
  in
  let fail chkpt =
  in
  let rec fail_retry chkpt chkpt' =
    (* This implements Note 5 of Haskell Syntax report 
     * 
     * L (t:ts) (m:ms)	=	} : (L (t:ts) ms)	if m /= 0 and parse-error(t)
     *
     * The side condition parse-error(t) is to be interpreted as follows: if the tokens generated 
     * so far by L together with the next token t represent an invalid prefix of the Haskell grammar, 
     * and the tokens generated so far by L followed by the token "}" represent a valid prefix of the 
     * Haskell grammar, then parse-error(t) is true.
     *
     * In Elm all braces are implicitly added, therefore we may always attempt to insert braces. As of
     * now the only valid place for implicit braces seems to be before IN token.
     *)
    match chkpt with 
    | I.Rejected _ -> raise ParseFailure
    | I.HandlingError _ -> 
      begin 
        let tok_rdelim = (Tokens.RDELIM, !last_e, !last_e) in
        match I.shifts (I.offer chkpt tok_rdelim) with
        | Some _ -> 
          (* The parser accepts the token and therefore the rbrace allows for 
           * forms a valid Elm syntax prefix. If the parser accepts the token.
           * we restarts the parser from that location
           *)
          print_endline "Token } injected.\n";
          I.loop_handle_undo succeed fail supplier (I.offer chkpt tok_rdelim)
        | None -> 
          I.loop_handle_undo succeed fail supplier chkpt)
      end
    | _ -> I.loop_handle_undo succeed fail supplier chkpt
  in
  I.loop_handle_undo succeed fail supplier init
*)


(* This function formats Elm program string and formats it so that it introduces 
 * some layout and identations that reflects program structure. *)
let dump_with_layout ?(inc=4) elm_str = 
  let t = Stream.of_string elm_str in
  let outs  = Core.Out_channel.output_string stdout in
  let outch = Core.Out_channel.output_char stdout in
  let out_indent indent = 
    if max indent 0 != 0 then
      Printf.sprintf "%*s" (max indent 0) " " |> outs
    else 
      ()
  in
  let rec next indent =
    try 
      match Stream.next t with
      | '{' -> outs "{\n"; out_indent (indent + inc); next (indent + inc)
      | ';' -> outs ";\n"; out_indent indent; next indent
      | '|' -> outch '\n'; out_indent (indent - 2); outs "| "; next indent 
      | '}' -> outch '\n'; out_indent (indent - inc); outs "}";  next (indent - inc)
      | ch  -> outch ch; next indent
    with Stream.Failure -> ()
  in next 0; outch '\n'
  