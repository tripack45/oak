
open Oaklib
let src = Src.Source.of_stdin ()

module M = Core.Int.Map

let to_path str = ElAst.Path.Just (ElAst.MConId.of_string str)

let mods = 
  [
    (to_path "Stdin", src);
  ]

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
    Passman.log "== Lexical ==\n" |>>
    Passman.ignore (
      Passman.pass Pass.PhaseLexical.pass |>>
      Passman.dump 
    ) 
  ) |>>
  ( 
    Passman.log "== ResolveSym ==\n" |>>
    Passman.pass Pass.PhaseResolveSymbols.pass |>>
    Passman.dump
  )

let _ = 
  Passman.exec (module OTarget.DirectSourced) src mods pipeline