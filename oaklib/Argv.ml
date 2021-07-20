(* Argv: This module implements an extensible argv handling *)

open Core
open Arg

module Verbosity =
struct
  type t = 
    | Debug
    | Info
    | Warning
    | Error
    | Critical

  let to_int = function
    | Debug    -> 0
    | Info     -> 1
    | Warning  -> 2
    | Error    -> 3
    | Critical -> 4

  let symbols = ["debug"; "info"; "warning"; "error"; "critical"]

  let of_symbols = function
    | "debug"    -> Debug
    | "info"     -> Info
    | "warning"  -> Warning
    | "error"    -> Error
    | "critical" -> Critical
    | _ -> failwith "unkown symbol"

  let default = Warning

  let compare l1 l2 = 
    Int.compare (to_int l1) (to_int l2)
end

type spec = key * Arg.spec * doc

type opt' = 
{
  trace_parsing : bool ref;
  warn_as_err   : bool ref;
  verbosity     : Verbosity.t ref;
}

type opt =
{
  trace_parsing : bool;
  warn_as_err   : bool;
  verbosity     : Verbosity.t;
}

let default : opt =
{
  trace_parsing = false;
  warn_as_err   = false;
  verbosity     = Verbosity.Warning;
}

let defaults' () : opt' = 
{
  trace_parsing = ref default.trace_parsing;
  warn_as_err   = ref default.warn_as_err;
  verbosity     = ref default.verbosity;
}

let finalize (opt' : opt') : opt =
{
  trace_parsing = !(opt'.trace_parsing);
  warn_as_err   = !(opt'.warn_as_err);
  verbosity     = !(opt'.verbosity);
}

let specs () : spec list * opt' =
  let opt' = defaults' () in
  let speclist = 
    [
      ("-trace-parsing", Arg.Set opt'.trace_parsing, "Trace parsing information");
      ("-warn-as-err",   Arg.Set opt'.warn_as_err, "Warnings are treated as erros.");
      ("-verbosity",     Arg.Symbol (Verbosity.symbols, fun s -> opt'.verbosity := Verbosity.of_symbols s), "Verbosity")
    ]
  in
  (speclist, opt')
