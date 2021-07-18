let max_paren_depth = 6
let code_len_args : Pass.WarnCodeLen.CodeLen.code_len_args = { 
    m = ((1000, 1000.), (750, 750.)); 
    func = ((150, 150.), (100, 100.)); 
    lambda = ((80, 80.), (60, 60.)) 
  }

module Driver = Oaklib.Driver
module Src = Oaklib.Src
module ElAst = Oaklib.ElAst
module Lex = Oaklib.Lex
module Parse = Oaklib.Parse
module OakPass = Oaklib.Pass
module ElfPass = Pass
module ElAstResolved = Oaklib.ElAstResolved

module ElmModule: sig 
  type meta
  type t
  val create_meta: string -> string -> meta
  val of_chn: string -> in_channel -> t
  val of_meta: meta -> t
  val analysis_m: t -> t
  val resolve_module_dependency: t list -> (ElAst.Path.t * ElAst.Syntax.m) list
  val resolve_ast: (ElAst.Path.t * ElAst.Syntax.m) list -> ElAstResolved.Syntax.m list
end = struct
  type meta = {
    (* identifier *)
    name: ElAst.Path.t;
    (* canonical path *)
    src_path: string option;
  }

  let to_path str = ElAst.Path.Just (ElAst.MConId.of_string str)

  let create_meta name_str path_str = 
    let name = to_path name_str in
    let src_path = Some path_str in 
    { name; src_path }

  type t = {
    meta: meta;
    src: Src.Source.t;
    m: ElAst.Syntax.m;
  }

  let of_chn name chn = 
    let meta = {name = to_path name; src_path = None} in
    let () = Driver.print_title "Info" @@ Printf.sprintf "Parsing: %s" name in
    let src = Src.Source.of_in_channel name chn in
    (* let () = Driver.print_title "Info" "Layout Insensitive" in
    let () = print_endline ":: Warning :: current Layout Insensistive Info is legacy and doesn't ensure correctness." in
    let () = Lex.layout_insensitive_src src in *)
    let () = Driver.print_title "Info" "EL AST" in
    let m = Parse.parse_src src in
    let () = Parse.dump_with_layout @@ ElAst.ToString.m_to_string m in
    { meta; src; m }

  let of_meta { name; src_path } = 
    let src_path = Option.get src_path in
    let chn = open_in src_path in
    let t = of_chn (ElAst.Path.to_string name) chn in
    let () = close_in chn in
    t

  let analysis_m ({ src; m; _ } as modl) =
    let () = Driver.print_title "Analysis" "ParenthesesDepth" in
    let () = ElfPass.WarnParentheseDepth.dump_result src 
             @@ ElfPass.WarnParentheseDepth.run ~max_depth:max_paren_depth m in
    let () = Driver.print_title "Analysis" "CodeLength" in
    let () = ElfPass.WarnCodeLen.dump_result src 
             @@ ElfPass.WarnCodeLen.run ~args:code_len_args m in
    modl

  let resolve_module_dependency (modules: t list) =
    let () = Driver.print_title "Phase" "ResolveModuleDependency" in
    let mods = modules |> Core.List.map ~f:(fun {meta; m; _} -> (meta.name, m)) in
    let open Oaklib.PhaseModuleDependency in
    let mods = match run mods with
    | R.Ok (v, _) -> v
    | R.Error (_errors, _warns) -> 
      (* dump_error design seems to be a problem; no src selected here *)
      (* dump_errors src errors; *) 
      assert false
    in
    let () = Core.List.iter mods 
      ~f:(fun (_, m) -> Parse.dump_with_layout @@ ElAst.ToString.m_to_string m)
    in mods
    
  let resolve_ast mods =
    let () = Driver.print_title "Phase" "ResolveSymbols" in
    let open Oaklib.PhaseResolveSymbols in
    let mods = match run mods with
    | Rst.Ok (v, _w) -> v
    | Rst.Error (_e, _ws) -> assert false
    in mods
end

let () =
  let argc = Array.length Sys.argv in
  if argc = 1 then
    ElmModule.of_chn "Stdin" stdin
    |> ElmModule.analysis_m
    |> (fun x -> [x])
    |> ElmModule.resolve_module_dependency
    |> ElmModule.resolve_ast
    |> ignore
  else
  let open Oaklib.Driver in
  let proj_path = Array.get Sys.argv 1 in
  let () = print_title "Info" @@ Printf.sprintf "Searching: \"%s\"" proj_path in
  let src_dir = Core.Filename.concat proj_path "src" in
  SrcFull src_dir
    |> find_elm
    |> List.map (
        fun (SrcName n, x) ->
          let x = Core.Filename.concat src_dir x in
          ElmModule.create_meta n x
      )
    |> List.map ElmModule.of_meta
    (* |> List.map ElmModule.analysis_m
    |> ElmModule.resolve_module_dependency
    |> ElmModule.resolve_ast *)
    |> ignore
