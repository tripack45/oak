open Oaklib
open Oaklib.Driver

(* main_driver with chn *)

let main_chn name chn = 
  print_endline @@ Printf.sprintf "----- [Info] Parsing: %s -----" name;
  let src = Src.Source.of_in_channel name chn in
  let () = print_endline "----- [Info] Layout Insensitive -----" in
  let () = Lex.layout_insensitive_src src in
  (* let () = print_endline "----- [Info] EL AST -----" in
  let m = Parse.parse_src src in
  let () = Parse.dump_with_layout @@ ElAst.ToString.m_to_string m in *)
  (* let () = print_endline "----- [Warning] ParenthesesDepth -----" in
  let () = Pass.WarnParentheseDepth.dump_result src @@ Pass.WarnParentheseDepth.run ~max_depth:1 m  in
  let () = print_endline "----- [Warning] CodeLength -----" in
  let () = Pass.WarnCodeLen.dump_result src @@ Pass.WarnCodeLen.run m  in
  let () = print_endline "----- [Phase] ResolveSymbols -----" in
  let m = Pass.PhaseResolveSymbols.resolve_mod ~modpath:None ElAst.Path.Map.empty m in
  let () = Parse.dump_with_layout @@ ElAstResolved.ToString.m_to_string m in *)
  ()



let rec print_string_list list =
  match list with
  | x :: xs -> 
    let () = print_endline x in 
    let () = print_string_list xs in 
    ()
  | [] -> ()

(* let read_file file = 
  let chn = open_in file in
  try
    while true do
      let line = input_line chn in
      print_endline line;
    done
  with End_of_file ->
    close_in chn *)


let () =
  (* path info *)
  let argc = Array.length Sys.argv in
  if argc == 1 then 
    main_chn "stdin" stdin
  else
  let path = Array.get Sys.argv 1 in
  print_endline @@ "searching: " ^ path;
  let path_map = SrcFull path
    |> Driver.traverse_elm_proj_root 
  in
  let _ = path_map
    |> List.map (fun (SrcName n, SrcFull x) -> n ^ " --> " ^ x) 
    |> print_string_list 
  in
  path_map
    |> List.map (fun (SrcName n, SrcFull x) -> (n, x))
    (* |> List.map (fun (n, x) -> read_file x;  (n, x)) *)
    |> List.map (fun (n, x) -> let chn = open_in x in main_chn n chn; close_in chn )
    |> ignore
