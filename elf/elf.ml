

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
let src = Oaklib.Src.Source.of_stdin ()
let m = Oaklib.Parse.parse_src src
let () = print_endline "----- [Info] EL AST ----"
let () = Oaklib.Parse.dump_with_layout @@ Oaklib.ElAst.ToString.m_to_string m
let () = print_endline "----- [Warning] ParenthesesDepth ----"
let () = Pass.WarnParentheseDepth.dump_result src @@ Pass.WarnParentheseDepth.run ~max_depth:1 m 
let () = print_endline "----- [Phase] ResolveSymbols ----"
let m = Oaklib.Pass.PhaseResolveSymbols.resolve ~modpath:None ~export_dict:[] m 

(* let () = 
    print_endline (String.make 20 '=');
    print_endline "Dumping AST:";
    print_endline (Ast.mod_to_string m) *)