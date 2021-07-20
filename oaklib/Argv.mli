open Core.Arg

module Verbosity :
sig
  type t = 
    | Debug
    | Info
    | Warning
    | Error
    | Critical

  val default : t 

  val compare : t -> t -> int
end

type spec = key * Arg.spec * doc

type opt' 

type opt =
{
  trace_parsing : bool;
  warn_as_err   : bool;
  verbosity     : Verbosity.t;
}

val default : opt

val specs : unit -> spec list * opt' 

val finalize : opt' -> opt