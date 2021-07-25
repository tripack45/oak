let opts = Argv.parse ()

open Oaklib

let to_path str = ElAst.Path.Just (ElAst.MConId.of_string str)

let mods = 
  if opts.stdin then 
    [
      (to_path "Stdin", Src.Source.of_stdin ());
    ]
  else
    failwith "Only -stdin supported"

open Passman.Syntax

let pipeline = 
  (
    Passman.log "== PhaseParsing ==\n" |>>
    Passman.pass Pass.PhaseParse.pass |>>
    Passman.dump 
  ) |>>
  (
    Passman.log "== ModDep ==\n" |>>
    Passman.pass Pass.PhaseResolveModuleDependency.pass |>>
    Passman.dump 
  ) |>>
  (
    Passman.ignore (
      (
        Passman.log "== Lexical ==\n" |>>
        Passman.pass Pass.PhaseLexical.pass |>>
        Passman.dump 
      ) |>>
      (
        Passman.log "== Rename ==\n" |>>
        Passman.pass Pass.PhaseRename.pass |>>
        Passman.dump 
      )
    ) 
  ) |>>
  ( 
    Passman.log "== ResolveSym ==\n" |>>
    Passman.pass Pass.PhaseResolveSymbols.pass |>>
    Passman.dump
  )

let _ = 
  Passman.exec (module OTarget.DirectSourced) (List.hd mods |> snd) mods pipeline