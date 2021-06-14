
open Oaklib
let src = Src.Source.of_stdin ()

(* module L = Lex.LayoutSensitiveLexer
module T = L.T
let _ =
  let lexbuf = Lexing.from_channel ~with_positions:true stdin in
  let t = L.from_lexbuf lexbuf in
    let rec looper () = 
      let (tok, pos) = L.take_expanded t in (
        match tok with 
        | T.EOF -> raise T.Eof
        | _ ->
          print_string (
            (*(* L.pos_to_string pos ^ " " ^ *) T.to_parse_string tok ^ " "*)
            (* L.pos_to_string pos ^ " " ^ *) T.to_string tok ^ " "
          );
          looper()
      )
    in 
    try
         looper ()
    with 
    T.Eof -> ();
    print_endline "" *)

(* module S = Sort *)

module M = Core.Int.Map

let to_path str = ElAst.Path.Just (ElAst.MConId.of_string str)

let mods = 
  [
    (to_path "Stdin", Parse.parse_src' src);
  ]

let () = print_endline "----- [Info] EL AST -----"
let () = 
  Core.List.iter mods 
    ~f:(fun (_, m) -> Parse.dump_with_layout @@ ElAst.ToString.m_to_string m) 

let () = print_endline "----- [Phase] ResolveModuleDependency -----"
let mods = 
  let open Pass.PhaseResolveModuleDependency in
  match run mods with
  | R.Ok v -> v |> Core.List.map ~f:(fun (path, (_, m)) -> (path, m))
  | R.Error errors -> 
    dump_errors src errors; assert false

let () = 
  Core.List.iter mods 
    ~f:(fun (_, m) -> Parse.dump_with_layout @@ ElAst.ToString.m_to_string m)

let () = print_endline "----- [Phase] ResolveSymbols -----" 
let mods = Pass.PhaseResolveSymbols.run mods
let () = 
  Core.List.iter mods 
    ~f:(fun m -> Parse.dump_with_layout @@ ElAstResolved.ToString.m_to_string m)


(* let () = 
    print_endline (String.make 20 '=');
    print_endline "Dumping AST:";
    print_endline (Ast.mod_to_string m) *)