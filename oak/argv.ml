open Oaklib

type spec = Argv.spec

type opt' = 
{
  oaklib : Argv.opt';
  args   : string list ref;
  stdin  : bool ref;
}

type opt = 
{
  oaklib : Argv.opt;
  args   : string list;
  stdin  : bool;
}

let default : opt =
{
  oaklib = Argv.default;
  args   = [];
  stdin  = false;
}

let finalize (opt' : opt') : opt =
{
  oaklib = Argv.finalize opt'.oaklib;
  args   = !(opt'.args);
  stdin  = !(opt'.stdin);
}

let specs () : spec list * opt' =
  let (oaklib_specs, oaklib_opt') = Argv.specs () in
  let opt' : opt' =
  {
    oaklib = oaklib_opt';
    args   = ref default.args;
    stdin  = ref default.stdin;
  }
  in
  let speclist = 
    [
      ("-stdin", Arg.Set opt'.stdin, "Take module source only stdin.")
    ] @
    oaklib_specs
  in
  (speclist, opt')

let parse () : opt =
  let (speclist, opt') = specs () in
  let usage_msg = 
    "oak -stdin" 
  in
  let args_fun arg = 
    opt'.args := arg::!(opt'.args)
  in
  Arg.parse speclist args_fun usage_msg;
  finalize opt'
