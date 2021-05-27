let src = Oaklib.Src.Source.of_stdin ()
let m = Oaklib.Parse.parse_src src
let () = print_endline "----- [Info] EL AST -----"
let () = Oaklib.Parse.dump_with_layout @@ Oaklib.ElAst.ToString.m_to_string m
let () = print_endline "----- [Warning] ParenthesesDepth -----"
let () = Pass.WarnParentheseDepth.dump_result src @@ Pass.WarnParentheseDepth.run ~max_depth:1 m 
let () = print_endline "----- [Phase] ResolveSymbols -----"
let m = Oaklib.Pass.PhaseResolveSymbols.resolve ~modpath:None ~export_dict:[] m
