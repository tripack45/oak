(* Passman - A pass manager for Oak *)

(* Common output target *)
type cot = ..

module type OUTPUT_TARGET =
sig 
  type t 
  type cot += O of t
  val dump  : t -> string -> unit
  val debug : t -> string -> unit
end

(* A compilation pipeline compiling takes 'a to 'b, much like a function *)
type ('a, 'b) pipeline

(* A compilation phase that takes the common output type o and *)
type ('a, 'b) pass = 
{
  (* The name of the pass *)
  name : string;
  (* The transformation function of the pass *)
  func : cot -> 'a -> 'b option;
  (* The function that allows the pass manager to dump the result *)
  to_string : 'b -> string;
}

type 'a analysis =
{
  (* The name of the pass *)
  name : string;
  (* The analysis function of the pass *)
  func : cot -> 'a -> unit;
}

(* A trivial pipe line that directly produces the result *)
val ok         : 'b -> ('a, 'b) pipeline
(* A pipeline that directly signifies a failure *)
val error      : ('a, 'b) pipeline
(* A pipeline defined through a compilation pass *)
val pass       : ('a, 'b) pass -> ('a, 'b) pipeline 
(* A pipeline defined through a optional compilation pass *)
val opt_pass   : ('a, 'a) pass -> ('a, 'a) pipeline 
(* A pipeline defined through an anlysis pass that does not transform the code *)
val analysis   : 'a analysis -> ('a, 'a) pipeline
(* A pseudo-pipeline that marks a tracepoint in the compilation *)
val tracepoint : string -> ('a, 'a) pipeline
(* Issues a log through Output.debug function *)
val log        : string -> ('a, 'a) pipeline
(* Dumps the current state of program in the pipeline, if dump is available *)
val dump       : ('a, 'a) pipeline
(* A pipeline that runs but ignores the result of the previous pipeline *)
val ignore     : ('a, 'b) pipeline -> ('a, 'a) pipeline
(* A pipeline that threads two pipelines into once *)
val andThen    : ('a, 'c) pipeline * ('c, 'b) pipeline -> ('a, 'b) pipeline

(* Runs the pipeline on a given set of inputs, returns the result and output object *)
val exec       : (module OUTPUT_TARGET with type t = 'out) -> 'out -> 'a -> ('a, 'b) pipeline -> 'b option

module Syntax :
sig
  val (|>>) : ('a, 'b) pipeline -> ('b, 'c) pipeline -> ('a, 'c) pipeline
end