open Core

(* This type of output handler has no "state" and directly writes calls to their
 * output functions to out_channel.
 *)
module type DIRECT_WRITE = 
sig
  type t

  include Passman.OUTPUT_TARGET with type t := t

  val printf  : t -> ('a, Out_channel.t, unit) format -> 'a
  val eprintf : t -> ('a, Out_channel.t, unit) format -> 'a
  val wprintf : t -> ('a, Out_channel.t, unit) format -> 'a
end

module Direct : DIRECT_WRITE with type t = unit = 
struct
  type t = unit

  type Passman.cot += O of t

  let eprintf () format = Printf.eprintf format
  let wprintf () format = Printf.eprintf format
  let printf  () format = Printf.eprintf format

  let dump  () str = Parse.dump_with_layout ~chn:stderr str
  let debug () str = eprintf () "%s" str
  let print () str = printf () "%s" str 
end

module DirectSourced : 
sig
  include DIRECT_WRITE with type t = Src.Source.t 

  val wprint_src : t -> Lexing.position * Lexing.position -> unit
  val eprint_src : t -> Lexing.position * Lexing.position -> unit
end = 
struct
  type t = Src.Source.t

  type Passman.cot += O of t

  let eprintf _t format = Printf.eprintf ("Error: " ^^ format)
  let wprintf _t format = Printf.eprintf ("Warning: " ^^ format)
  let printf  _t format = Printf.printf format

  let wprint_src t range = Printf.eprintf "%s\n" (Src.Source.lines t range)
  let eprint_src t range = Printf.eprintf "%s\n" (Src.Source.lines t range)

  let dump  _t str = Parse.dump_with_layout ~chn:stderr str
  let debug _t str = Printf.eprintf "%s" str
  let print t str = printf t "%s" str 
end