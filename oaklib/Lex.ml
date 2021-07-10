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

(* Base layout*)
module T = Tokens
module L = ElmLex.Make(T)

type tok = T.t
type pos = Lexing.position * Lexing.position
type tp  = tok * pos
type cmt = 
  | LineCmt  of string
  | BlockCmt of string
type cmtp = cmt * pos

exception IndentationError of string

type token_pos = T.t * pos 

open Seq
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


module Raw =
struct
  type t = tp Seq.t

  (* EOF needs to be prodeced as an actual token because we need its 
    * position for the subsequent code to inser the closing scopes. *)
  let elmlex_of_lexbuf on_comment lexbuf : t = 
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
  
  let rec separate_project seq : t =
    let sp = separate_project in
    fun () ->
      match seq () with 
      | Nil             -> Seq.Nil
      | Cons (tp, seq') -> 
        match seq' () with
        | Nil               -> Seq.Cons (tp, Seq.empty)
        | Cons (tp', seq'') -> 
          let ((_, e), (s', _)) = (snd tp, snd tp') in
          let blank = s'.pos_cnum - e.pos_cnum = 1 in
          match (fst tp, fst tp') with
          | (T.RPAREN      , T.PROJ_FUNC field)
          | (T.VARID _     , T.PROJ_FUNC field)
          | (T.QVARID _    , T.PROJ_FUNC field)
          | (T.RBRACE      , T.PROJ_FUNC field)
          | (T.PROJECT _   , T.PROJ_FUNC field)
          (* Make the behavior compliant with Elm compiler *)
          | (T.PROJ_FUNC _ , T.PROJ_FUNC field) when not blank ->
            Seq.Cons (tp, sp ((T.PROJECT field, snd tp') @:: seq''))
          | _ ->
            Seq.Cons (tp, sp seq')

  let t_of_lexbuf on_comment lexbuf : t =
    separate_project (elmlex_of_lexbuf on_comment lexbuf)
    

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

    let t_of_lexbuf on_comment lexbuf =
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
      | _                                  -> raise (IndentationError "Indentation Error")
end


