open Core

open! struct

  let to_string_per_mod ts f = 
    List.map ts ~f:(
      fun (path, t) ->
        let header = "== Source " ^ ElAst.Path.to_string path ^ " ==\n" in
        header ^ f t
    )
    |> String.concat ~sep:"\n"

end

(* A list of available passes *)
module PhaseParse =
struct
  type from_t = (ElAst.Path.t * Src.Source.t) list
  type to_t   = (ElAst.Path.t * ElAst.Syntax.m) list

  let pass : (from_t, to_t) Passman.pass = 
    let parse_sources _o srcs = 
      Option.some @@
        List.map srcs ~f:(
          fun (path, src) -> (path, Parse.parse_src src)
        )
    in
    let to_string parsed = 
      to_string_per_mod parsed ElAst.ToString.m_to_string
    in
    {
      name = "Parsing";
      func = parse_sources;
      to_string = to_string;
    }
end

module PhaseResolveModuleDependency = 
struct
  module P = PhaseModuleDependency 

  type from_t = PhaseParse.to_t
  type to_t   = from_t

  let pass : (from_t, to_t) Passman.pass = 
    let resolve_dep o mods = 
      match P.run mods with
      | P.R.Ok (r, warns)       -> 
        P.dump_warnings o warns;
        Some r
      | P.R.Error (errs, warns) -> 
        P.dump_warnings o warns ;
        P.dump_errors   o errs ;
        None
    in
    let to_string resolved = 
      to_string_per_mod resolved ElAst.ToString.m_to_string
    in
    {
      name = "ResolveModuleDependency";
      func = resolve_dep;
      to_string = to_string;
    }
end

module PhaseLexical = 
struct
  module P = PhaseLexical

  type from_t = PhaseResolveModuleDependency.to_t
  type to_t   = (ElAst.Path.t * ElAstLexical.Syntax.m) list

  let pass : (from_t, to_t) Passman.pass = 
    let lexical o mods = 
      match P.run mods with
      | P.R.Ok r       -> Some r
      | P.R.Error errs -> 
        P.dump_errors o errs;
        None
    in
    let to_string lexical =
      to_string_per_mod lexical ElAstLexical.ToString.m_to_string
    in
    {
      name = "Lexical";
      func = lexical;
      to_string = to_string;
    }
end

module PhaseResolveSymbols = 
struct
  module P = PhaseResolveSymbols

  type from_t = PhaseResolveModuleDependency.to_t
  type to_t   = ElAstResolved.Syntax.m list

  let pass : (from_t, to_t) Passman.pass = 
    let resolve _o mods = 
      match P.run mods with
      | P.Rst.Ok    (r, _warns)     -> Some r
      | P.Rst.Error (_errs, _warns) -> None
    in
    let to_string resolved =
      List.map resolved ~f:ElAstResolved.ToString.m_to_string |> String.concat ~sep:"\n"
    in
    {
      name = "PhaseResolveSymbols";
      func = resolve;
      to_string = to_string;
    }
end

module PhaseRename = 
struct
  module P = PhaseRename

  type from_t = PhaseLexical.to_t
  type to_t   = ElAstRenamed.Syntax.m list

  let pass : (from_t, to_t) Passman.pass = 
    let rename _o mods = 
      match P.run mods with
      | P.Rst.Ok    (r, _warns)     -> Some r
      | P.Rst.Error (_errs, _warns) -> None
    in
    let to_string resolved =
      List.map resolved ~f:ElAstRenamed.ToString.m_to_string |> String.concat ~sep:"\n"
    in
    {
      name = "PhaseRename";
      func = rename;
      to_string = to_string;
    }
end

module PhaseTypechecking = 
struct
  module P = PhaseTypecheck

  type from_t = PhaseResolveSymbols.to_t
  type to_t   = unit

  let pass : (from_t, to_t) Passman.pass = 
    let resolve _o mods = 
      match P.run mods with
      | P.Rst.Ok    (r, _warns)     -> Some ()
      | P.Rst.Error (_errs, _warns) -> None
    in
    let to_string _resolved =
      "Done! \n"
      (* List.map resolved ~f:ElAstResolved.ToString.m_to_string |> String.concat ~sep:"\n" *)
    in
    {
      name = "PhaseTypechecking";
      func = resolve;
      to_string = to_string;
    }
end