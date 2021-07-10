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


(* A functional API *)
module Raw :
sig
  type t 
  val elmlex_of_lexbuf : (cmtp -> unit) -> Lexing.lexbuf -> t
  val t_of_lexbuf      : (cmtp -> unit) -> Lexing.lexbuf -> t
  (* This should only be used to dump stuff. Ideally we would replace them with 
  * corresponding dump functions. Same goes for Annotated.tp_seq_of_t. *)
  val tp_seq_of_t      : t -> tp Seq.t
end

module Annotated:
sig
  type t
  val annotate     : Raw.t -> t
  val t_of_lexbuf : (cmtp -> unit) -> Lexing.lexbuf -> t
  val tp_seq_of_t  : t -> tp Seq.t
end

module AlgL :
sig
  type mctx = int list
  type chkpt = (Annotated.t * mctx)

  val initial : Annotated.t -> chkpt

  val algl : chkpt -> [`Just of tp * chkpt | `Choice of (tp *chkpt) * (tp * chkpt)]
end
