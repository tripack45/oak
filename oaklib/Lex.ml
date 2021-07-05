(* This would make a very fun VE280 exercise *)
module MakePersistent :
sig
  val seq_from : (unit -> 'a option) -> 'a Seq.t
end =
struct

  type 'a lnode = 
  | Unexplored        (* The end of the explored values of f *)
  | Nil               (* f have reached its end *)
  | Node of           (* some previous extracted values of f *)
    {
      next  : 'a lnode ref;
      value : 'a ;
    }

  let seq_from (f : unit -> 'a option) : 'a Seq.t =
    let chain = ref Unexplored in
    let rec take (head : 'a lnode ref) () : 'a Seq.node =
      match !head with 
      | Unexplored -> 
        begin
          match f () with
          | None   -> head := Nil; Seq.Nil
          | Some v -> 
            let next = ref Unexplored in
            head := Node { next = next; value = v };
            Seq.Cons (v, take next)
        end
      | Nil -> Seq.Nil
      | Node { next; value} ->
        Seq.Cons (value, take next)
    in (take chain)
end

(* This module wraps the layout insensitive lexer generated into 
 * a layout sensitive tokenizer *)
module LayoutSensitiveLexer :
sig
  exception IndentationError of string

  module T = Tokens

  type tok = T.t
  type pos = Lexing.position * Lexing.position
  type tp  = tok * pos
  type cmt = 
    | LineCmt  of string
    | BlockCmt of string
  type cmtp = cmt * pos

  val explode_qualified_name : string -> Lexing.position -> tp list

  (* Deprecated imperative API *)
  module Legacy : 
  sig
    module T = Tokens

    type t 
    val from_lexbuf   : Lexing.lexbuf -> t
    val take_expanded : t -> tp
  end

  (* A functional API *)
  module Raw :
  sig
    type t 
    val t_of_lexbuf  : (cmtp -> unit) -> Lexing.lexbuf -> t
    (* This should only be used to dump stuff. Ideally we would replace them with 
    * corresponding dump functions. Same goes for Annotated.tp_seq_of_t. *)
    val tp_seq_of_t  : t -> tp Seq.t
  end

  module Annotated:
  sig
    type t
    val annotate     : Raw.t -> t
    val t_of_lex_buf : (cmtp -> unit) -> Lexing.lexbuf -> t
    val tp_seq_of_t  : t -> tp Seq.t
  end

  module AlgL :
  sig
    type mctx = int list
    type chkpt = (Annotated.t * mctx)

    val initial : Annotated.t -> chkpt

    val algl : chkpt -> [`Just of tp * chkpt | `Choice of (tp *chkpt) * (tp * chkpt)]
  end

end =
struct

  (* Base layout*)
  module T = Tokens
  module L = ElmLex.Make(T)

  open Seq

  exception IndentationError of string

  type tok = T.t
  type pos = Lexing.position * Lexing.position

  type token_pos = T.t * pos 

  type tp = token_pos

  type cmt = 
    | LineCmt  of string
    | BlockCmt of string

  type cmtp = cmt * pos

  let (@::) = Seq.cons

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

  module Legacy =
  struct

  module T = Tokens

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

  module Raw =
  struct
    type t = tp Seq.t

    (* EOF needs to be prodeced as an actual token because we need its 
     * position for the subsequent code to inser the closing scopes. *)
    let t_of_lexbuf on_comment lexbuf : t = 
      MakePersistent.seq_from (
        let eof_produced = ref false in
        let rec loop () = 
          if not !eof_produced then
            let tok = L.initial lexbuf in
            let p_start = Lexing.lexeme_start_p lexbuf in
            let p_end   = Lexing.lexeme_end_p lexbuf in
            match tok with
            | T.LCOMMENT (pos, s) -> on_comment (LineCmt  s, pos); loop ()
            | T.BCOMMENT (pos, s) -> on_comment (BlockCmt s, pos); loop ()
            | _ -> 
              eof_produced := (tok = T.EOF);
              Some (tok, (p_start, p_end))
          else 
            None
        in loop
      )

    let tp_seq_of_t = Fun.id
  end

  module Annotated =
  struct
    type t = tp Seq.t

    (* Haskell report 2010, section 10.3 Layout:
    *
    * A stream of lexemes as specified by the lexical syntax in the Haskell report, with the following additional tokens:
    * 
    * - If a let, where, do, or of keyword is not followed by the lexeme {, the token {n} is inserted after the keyword, 
    *   where n is the indentation of the next lexeme if there is one, or 0 if the end of file has been reached.
    *
    * - If the first lexeme of a module is not { or module, then it is preceded by {n} where n is the indentation of the lexeme.
    * 
    * - Where the start of a lexeme is preceded only by white space on the same line, this lexeme is preceded by < n > where n is the 
    *   indentation of the lexeme, provided that it is not, as a consequence of the first two rules, preceded by {n}. 
    *)
    let annotate raw : t =
      let at_start_of (_, (s, _)) t' = (t', (s, s)) in
      let at_end_of   (_, (_, e)) t' = (t', (e, e)) in
      let rec kick (ts : Raw.t) () =
        match ts () with 
        | Nil -> failwith "Must end with EOF."
        | Cons (tp, ts') ->
          match (fst tp) with 
          (* The file is empty *)
          | T.EOF -> 
            Cons (at_start_of tp T.EOF, Seq.empty) 
          (* We insert {n} regardless of whether the next token is on a second line *)
          | T.LET | T.OF -> (
            (* peak next token *)
            match ts' () with 
            | Nil -> failwith "Must end with EOF."
            | Cons (tp', _) when fst tp' = T.LDELIM -> Cons (tp, kick ts')
            | Cons (tp', _) when fst tp' = T.EOF    -> Cons (tp, at_start_of tp' (T.REQ_INDENT 0) @:: kick ts')
            | Cons (tp', _) ->
              let (_, c') = token_lc tp' in
              Cons (tp, at_start_of tp' (T.REQ_INDENT c') @:: kick ts')
          )
          | _ -> (
            (* peak next token *)
            match ts' () with 
            | Nil -> failwith "Must end with EOF."
            | Cons (tp', _) when fst tp' = T.EOF ->
              (* EOF is not considered a lexeme by Haskell syntax *)
              Seq.Cons (tp, kick ts')
            | Cons (tp', _) ->
              let (l, _)  = token_lc tp 
              and (l', c') = token_lc tp' in
              if l < l' then
                Seq.Cons (tp, at_end_of tp (T.WTH_INDENT c') @:: kick ts')
              else 
                Seq.Cons (tp, kick ts')
          )
      in 
      (* Insert the initial indentation context *)
      match raw () with 
      | Nil -> failwith "Must end with EOF."
      | Cons (tp, _) -> 
        match fst tp with 
        | T.LDELIM -> kick raw
        | _ -> 
          let col = snd (token_lc tp) in
          at_start_of tp (T.REQ_INDENT col) @:: kick raw

      let t_of_lex_buf on_comment lexbuf =
        annotate (Raw.t_of_lexbuf on_comment lexbuf)
      let tp_seq_of_t = Fun.id
  end

  module AlgL =
  struct

    type mctx = int list

    type chkpt = (tp Seq.t * mctx)

    let initial seqt = (seqt, [])

    let rec algl ((seqt, ms) : chkpt) =
      match seqt () with 
      | Nil -> failwith "Parser requested non-existent token."
      | Cons ((tok, pos) as t , ts) ->
        let semi   = (T.SEMI, pos)
        and ldelim = (T.LDELIM, pos)
        and rdelim = (T.RDELIM, pos) in
        match (tok, ms) with 
        | (T.WTH_INDENT n, m::ms) when n = m -> `Just (semi, (ts, m::ms))
        | (T.WTH_INDENT n, m::ms) when n < m -> `Just (rdelim, (seqt, ms))
        | (T.WTH_INDENT _, ms)               -> algl (ts, ms)
        | (T.REQ_INDENT n, m::ms) when n > m -> `Just (ldelim, (ts, n::m::ms))
        | (T.REQ_INDENT n, [])    when n > 0 -> `Just (ldelim, (ts, [n]))
        | (T.REQ_INDENT n, m::ms) when n > m -> algl (ldelim @:: rdelim @:: (T.WTH_INDENT n, pos) @:: ts, ms)
        (* Elm does not allow explicit context therefore we should not see those tokens *)
        | (T.RDELIM, 0::ms)                  -> `Just (t, (ts, ms))
        | (T.RDELIM, _)                      -> raise (IndentationError "Explicit context must be closed explicitly")
        | (T.LDELIM, ms)                     -> `Just (t, (ts, 0::ms))
        (* EOFs must be matched first so that they don't consumed by the wildcard match *)
        | (T.EOF, m::ms)    when not (m = 0) -> `Just (rdelim, (seqt, ms))
        | (T.EOF, [])                        -> `Just (t, (Seq.empty, []))
        (* Algorithm L note #5. The condition should always satisfy since Elm does not support explicit layout *)
        | (_, m::ms)        when not (m = 0) -> `Choice ( (t, (ts, m::ms)), (rdelim, (seqt, ms)) )
        (* | (Nil, m::ms)                 when not (m = 0) -> `Just (rdelim, (seqt, ms)) *)
        | _                                  -> raise (IndentationError "Indentation Error")
  end

end

(* layout_insensitive *)

(* module L = LayoutSensitiveLexer
module T = L.T
let layout_insensitive_src src=
  let lexbuf = Lexing.from_string ~with_positions:true (Src.Source.raw src) in
  let t = L.Legacy.from_lexbuf lexbuf in
    let rec looper () = 
      let (tok, _pos) = L.Legacy.take_expanded t in (
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
    print_endline "" *)
