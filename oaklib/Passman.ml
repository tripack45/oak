open Core

(* Common output type *)
type cot = ..

module type OUTPUT_TARGET =
sig 
  type t 
  val as_cot : t -> cot 
  val dump   : t -> string -> unit
  val debug  : t -> string -> unit
end

module type PACKAGED =
sig
  include OUTPUT_TARGET
  val ot : t
end

type state = 
{
  out_m : (module PACKAGED);
  (* the function to dump the current representation of the program *)
  dump  : (unit -> string) option;
}

type ('a, 'b) pipeline = state -> 'a -> 'b option * state

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

let ok x : ('a, 'b) pipeline =
  fun s _a -> 
    (Some x, s)

let error : ('a, 'b) pipeline =
  fun s _a -> 
    (None, s)

let pass (pass : ('a, 'b) pass) : ('a, 'b) pipeline = 
  fun s a -> 
    let module Out = (val s.out_m : PACKAGED) in
    let r_opt = pass.func (Out.as_cot Out.ot) a in
    let s' = 
    {
      s with
      dump = Option.map r_opt ~f:(fun r () -> pass.to_string r);
    }
    in
    (r_opt, s')

let opt_pass = pass

let analysis (pass : 'a analysis) : ('a, 'a) pipeline = 
  fun s a -> 
    let module Out = (val s.out_m : PACKAGED) in
    pass.func (Out.as_cot Out.ot) a;
    (Some a, s)

let log str : ('a, 'a) pipeline = 
  fun s a -> 
    let module Out = (val s.out_m : PACKAGED) in
    Out.debug Out.ot str;
    (Some a, s)

let tracepoint _str : ('a, 'a) pipeline = 
  fun s a -> 
    (Some a, s)

let dump : ('a, 'a) pipeline = 
  fun s a -> 
    let module Out = (val s.out_m : PACKAGED) in
    Option.iter s.dump ~f:(fun fdump -> Out.dump Out.ot (fdump ()));
    (Some a, s)

let ignore (p : ('a, 'b) pipeline) :  ('a, 'a) pipeline = 
  fun s a ->
    let (_, s') = p s a in
    (Some a, s')

let andThen (p1, p2) : ('a, 'b) pipeline =
  fun s a -> 
    let (opt, s') = p1 s a in
    match opt with 
    | None -> (None, s)
    | Some x' -> 
      p2 s' x'

let exec (type out) m (init : out) a   (pipe : ('a, 'b) pipeline) =
  let module Out = (val m : OUTPUT_TARGET with type t = out) in
  let module Packaged = 
    struct
      include Out
      let ot = init
    end
  in
  let s : state =
    {
      out_m = (module Packaged : PACKAGED);
      dump  = None;
    }
  in
  let (r_opt, _) = pipe s a in
  r_opt 

module Syntax =
struct
  let (|>>) p1 p2 = andThen (p1, p2)
end