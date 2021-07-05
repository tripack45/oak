open Lexing

(* Parser Driver *)
module I = ElmParse.MenhirInterpreter

exception ParseFailure
let rec dump_tpseq tpseq = 
  match tpseq () with 
  | Seq.Nil -> ()
  | Seq.Cons ((tok, _pos), tp') ->
    print_string @@ Tokens.to_string tok ^ " ";
    dump_tpseq tp'

let mctx_to_string mctx = 
  "[" ^ String.concat ", " (List.map Int.to_string mctx) ^ "] "

let parse_src src =
  let open Lex in
  let lexbuf = Lexing.from_string ~with_positions:true (Src.Source.raw src) in
  let () = Lexing.set_filename lexbuf (Src.Source.name src) in
  let init_raw    = Raw.t_of_lexbuf (Src.Source.add_comment src) lexbuf in
  let init_menhir = ElmParse.Incremental.main lexbuf.lex_curr_p in
  let init_algl   = AlgL.initial (Annotated.annotate init_raw) in
  let as_offer (tp, (s, e)) = (tp, s, e) in

  (* 
  let () =
    print_endline "-- Raw input token stream --\n";
    dump_tpseq (Raw.tp_seq_of_t init_raw);
    print_endline "\n\n-- With explicit layout tokens --\n";
    dump_tpseq (Annotated.tp_seq_of_t (Annotated.annotate init_raw));
    print_endline "\n"
  in  
  *)

  let rec reduce_all checkpoint = 
    match checkpoint with 
    | I.AboutToReduce _ ->
      reduce_all (I.resume checkpoint)
    | _ -> checkpoint
  in

  let rec loop algl_chkpt checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
      begin
        match AlgL.algl algl_chkpt with
        | `Just (tp, c') -> 
          (* Printf.printf "Produced `Just   %-20s %s\n" (Tokens.to_string (fst tp)) (mctx_to_string (snd c')); *)
          loop c' (I.offer checkpoint (as_offer tp))
        | `Choice ((tp1, c1), (tp2, c2)) ->
          (* Printf.printf "Produced `Choice %-20s %s\n" (Tokens.to_string (fst tp1)) (mctx_to_string (snd c1)); *)
          let chkpt = reduce_all @@ I.offer checkpoint (as_offer tp1) in
          match chkpt with 
          | I.HandlingError _ 
          | I.Rejected -> 
            (* Printf.printf "Recovered with   %-20s %s\n" (Tokens.to_string (fst tp2)) (mctx_to_string (snd c2)); *)
            loop c2 (I.offer checkpoint (as_offer tp2))
          | _ ->
            loop c1 chkpt
      end
    | I.Shifting _
    | I.AboutToReduce _ ->
        let checkpoint = I.resume checkpoint in
        loop algl_chkpt checkpoint
    | I.HandlingError _env ->
      raise ParseFailure
    | I.Accepted v -> v
    | I.Rejected -> assert false
  in 
  loop init_algl init_menhir

(* This function formats Elm program string and formats it so that it introduces 
 * some layout and identations that reflects program structure. *)
let dump_with_layout ?(inc=4) elm_str = 
  let t = Stream.of_string elm_str in
  let outs  = Core.Out_channel.output_string stdout in
  let outch = Core.Out_channel.output_char stdout in
  let out_indent indent = 
    if max indent 0 != 0 then
      Printf.sprintf "%*s" (max indent 0) " " |> outs
    else 
      ()
  in
  let rec next indent =
    try 
      match Stream.next t with
      | '{' -> outs "{\n"; out_indent (indent + inc); next (indent + inc)
      | ';' -> outs ";\n"; out_indent indent; next indent
      | '|' -> outch '\n'; out_indent (indent - 2); outs "| "; next indent 
      | '}' -> outch '\n'; out_indent (indent - inc); outs "}";  next (indent - inc)
      | ch  -> outch ch; next indent
    with Stream.Failure -> ()
  in next 0; outch '\n'
  