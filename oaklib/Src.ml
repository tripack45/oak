(* src.ml
 * 
 * This file implements a source file manager. The source file managers is in charge of
 * keep track of source info and produce source strings based on line info.
 *)

module L = Lex

module Source :
sig 
  type t

  val of_stdin      : unit -> t
  val of_in_channel : string -> in_channel -> t

  val to_stream  : t -> char Stream.t
  val raw        : t -> string
  val name       : t -> string 
  val lines      : t -> L.pos -> string
  val add_comment: t -> L.cmtp -> unit
end =
struct
  
  type src_name = 
     | Stdin 
     | File of string

  (* Current representation redudants the source info by twice 
   * A better representation would save the content of the source file to 
   * a character array then extract the index of each newline character. *)
  type t = 
  {
    name    : src_name;
    lines   : string Array.t;
    raw     : string;
    mutable comment : (L.cmt * L.pos) list;
  }

  let _of_chn src_name chn =
    let strings = Core.In_channel.input_lines chn in
    let raw = Core.String.concat ~sep:"\n" strings in
    let lines = Core.Array.of_list strings in
    (* Break the raw string by resending them into a channel *)
    { name = src_name ; raw; lines; comment = [] }

  let of_stdin () = 
    _of_chn Stdin stdin

  let of_in_channel name chn = 
    _of_chn (File name) chn

  let to_stream { raw; _ } = 
    Stream.of_string raw

  let raw { raw; _ } = raw

  let to_in_channel { raw; _ } = 
    Core.In_channel.create raw

  let name {name; _} = 
    match name with 
    | Stdin -> "stdin"
    | File name -> name

  let lines {lines; _} ((s, e) : Lexing.position * Lexing.position) = 
    (* Line number counts from 1*)
    let (l, l') = (s.pos_lnum - 1, e.pos_lnum) in
    Core.Array.slice lines l l'
    |> Core.Array.mapi ~f:(fun i str -> Printf.sprintf "%3d| %s" (i + l + 1) str)
    |> Core.String.concat_array ~sep:"\n"

  let add_comment src (cmt, pos) = 
    src.comment <- (cmt, pos) :: src.comment
end