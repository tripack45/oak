(* This module wraps the layout insensitive lexer generated into 
 * a layout sensitive tokenizer *)
module LayoutSensitiveLexer =
struct

  (* Base layout*)
  module T = Tokens
  module L = ElmLex.Make(T)

  exception IndentationError of string

  type pos = Lexing.position * Lexing.position

  type token_pos = T.t * pos 

  let explode_qualified_name s start = 
    let lexbuf = Lexing.from_string s in
    let () = Lexing.set_position lexbuf start in
    let rec looper s = 
      let tok     = L.qcon_var lexbuf in
      if tok = T.EOF then 
        s 
      else 
        let p_start = Lexing.lexeme_start_p lexbuf in
        let p_end   = Lexing.lexeme_end_p   lexbuf in
        let tp = (tok, (p_start, p_end)) in
        tp :: looper s
    in looper []

  (* Haskell 98 Sec 9.3: The first column is designated column 1, not 0 *)
  let token_lc (tp : token_pos) =
    let (_, (p_start, _)) = tp in
    let linum = p_start.pos_lnum in
    let col   = p_start.pos_cnum - p_start.pos_bol + 1
    in (linum, col)

  let pos_to_string (l, c) = 
    Printf.sprintf "(%d:%d)" l c

  (* Lexing state of the layout sensitive lexer *)
  type t = {
    lexbuf : Lexing.lexbuf;
    (* Whether we have seen a lexeme in a module *)
    mutable last_lexeme : token_pos option;
    (* A buffer that allows us to push/pop/peek into the non-annotated stream *)
    mutable peeked : token_pos option;
    (* A buffer that allows us to construct a layout annotated stream of token on the fly *)
    mutable layout_buf  : token_pos list;
    (* m context used algorithm L, rule 6 *)
    mutable mctx : int list;
    mutable l_saved : token_pos option;
  }

  let from_lexbuf lexbuf : t = 
    {
      lexbuf       = lexbuf;
      last_lexeme  = None;
      peeked       = None;
      layout_buf   = [];
      mctx         = [];
      l_saved      = None;
    }
 
  (* Take a token and annotate its positon *)
  let take_lexbuf t = 
    let tok = L.initial t.lexbuf in
    let p_start = Lexing.lexeme_start_p t.lexbuf in
    let p_end   = Lexing.lexeme_end_p t.lexbuf in
    (tok, (p_start, p_end))

  (* layout_buf and lex_buf together forms a stream like object that affords 
   * getting tokens, putting back tokens, and peeking from the stream *)
  let take_unannotated t =
    match t.peeked with 
    | Some tp -> t.peeked <- None; tp
    | None -> take_lexbuf t

  (* let peek_unannotated t =
    match t.peeked with 
    | Some tp -> tp
    | None -> 
      let tp = take_lexbuf t in
      t.peeked <- (Some tp); tp *)

  (* These function forms of stream of layout annotated tokens *)
  let push_annotated t tp =
    t.layout_buf <- tp::t.layout_buf; ()

  let take_annotated t = 
    match t.layout_buf with
    | tp::tps -> 
      t.layout_buf <- tps; tp
    | [] -> 
      let (tok, pos) as tp = take_unannotated t in 
      let (l, c) = token_lc tp in
      if tok = T.EOF then
        tp
      else
        (* We first resolve post fixed layout elements *)
        let _ = 
          match tok with 
          | T.LET | T.OF -> (
              let (tok', _) as tp' = take_unannotated t in
              let (_, c') = token_lc tp' in
              (* FIXME: 
                 This implementation extracts tp' from the stream so that if tp is on a 
                 second line, a <n> token does not gets inserted in front. This behavior 
                 actually causes trouble in expressions such as "let let" and "case of let"
                 However it's fine right now as those programs are illformed anyway.
               *)
              push_annotated t tp';
              match tok' with 
              | T.LBRACE -> ()
              | T.EOF    -> push_annotated t (T.REQ_INDENT 0,  pos)
              | _        -> push_annotated t (T.REQ_INDENT c', pos)
          )
          | _ -> () in
        (* We then resolve possibly pre-fixed layout lex elements *)
        let ret : token_pos = 
          match t.last_lexeme with 
          | None ->
            (* This must be the first lexeme *)
            push_annotated t tp;
            (T.REQ_INDENT c, pos)
          | Some ((_, pos') as tp') ->
            let (l', _) = token_lc tp' in
            if l' < l then
              (* Encountered a newline *)
              (push_annotated t tp;
              (T.WTH_INDENT c, pos'))
            else
              tp
        in 
        (* Need to update last_lexeme to the just extracted lexeme before returning*)
        t.last_lexeme <- Some tp;
        ret

  exception Unimplemented

  (* Algorithm L *)
  let rec take_expanded t : token_pos =
    match t.l_saved with 
    | Some tp -> t.l_saved <- None; tp
    | None -> 
      let (tok, pos) as tp = take_annotated t in
      
        (* print_string ("\n?[" ^ String.concat ", " (List.map Int.to_string t.mctx) ^ "] ");
        print_string ("  ! " ^ T.to_string tok ^ "\n" );  *)
     
      match (tok, t.mctx) with 
      | (T.WTH_INDENT n, m::_) when m = n -> (T.SEMI, pos)
      | (T.WTH_INDENT n, m::ms) when n < m -> (push_annotated t tp; t.mctx <- ms; (T.RDELIM, pos))
      | (T.WTH_INDENT _, _)                -> take_expanded t 
      | (T.REQ_INDENT n, m::ms) ->
        if n > m then 
          (t.mctx <- n::m::ms; (T.LDELIM, pos))
        else
          raise (IndentationError "Sub context must be further indented.")
      | (T.REQ_INDENT n, []) ->
        if n > 0 then
          (t.mctx <- [n]; (T.LDELIM, pos))
        else
          raise (IndentationError "Sub context must be further indented.")
      (* | (T.REQ_INDENT _, _) ->
          push_annotated t tp;
          t.l_saved <- Some T.RDELIM;
          T.LDELIM *)
      | (T.LDELIM, _) | (T.RDELIM, _) ->
          (* Elm forbids explicit layout. We should never encounter them *)
          assert false
      | (T.EOF, []) -> raise (T.Eof)
      | (T.EOF, m::ms) -> 
        if m > 0 then
          (push_annotated t tp; t.mctx <- ms; (T.RDELIM, pos))
        else
          raise (IndentationError "EOF should not be in explicit context")
      | (t, _) -> (t, pos)
      | _ -> raise (IndentationError "Syntax Error")

end

(* layout_insensitive *)

module L = LayoutSensitiveLexer
module T = L.T
let layout_insensitive_src src=
  let lexbuf = Lexing.from_string ~with_positions:true (Src.Source.raw src) in
  let t = L.from_lexbuf lexbuf in
    let rec looper () = 
      let (tok, _pos) = L.take_expanded t in (
        match tok with 
        | T.EOF -> raise T.Eof
        | _ ->
          print_string @@ T.to_string tok ^ " ";
          looper()
      )
    in 
    try
         looper ()
    with 
    T.Eof -> ();
    print_endline ""
