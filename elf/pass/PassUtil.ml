module BindingContext : 
sig 
  open Oaklib.ElAst.Syntax
  type binder =
    | Mod of path'
    | Fun of var'
    | Lam
    | Val of pat'
    type t 
    val empty : t
    val add  : t -> binder * pos -> t 
    val dump : t -> (binder * pos) list
end =
struct
  open Oaklib.ElAst.Syntax
  type binder =
    | Mod of path'
    | Fun of var'
    | Lam
    | Val of pat'

  (* Context is a stack of a binder and the position of the enclosing construct *)
  type t = (binder * pos) list

  (* Empty context *)
  let empty : t = []

  let add ctx bp : t = (bp::ctx)
  let dump ctx = List.rev ctx
end

let dump_binding (binder, pos) =
  let open Oaklib.ElAst.ToString in
  let binder_str = 
    match binder with
    | BindingContext.Mod path  -> Printf.sprintf "<module> \"%s\"" @@ path_to_string path
    | BindingContext.Fun var   -> Printf.sprintf "<function> \"%s\"" @@ var_to_string var
    | BindingContext.Lam       -> Printf.sprintf "<lambda>"
    | BindingContext.Val pat   -> Printf.sprintf "<pattern> \"%s\"" @@ pat_to_string pat
  in
  Printf.printf "In the definition of %s (at %s):\n" 
                binder_str (pos_to_string pos)

type binder    = BindingContext.binder
type ctx       = BindingContext.t

