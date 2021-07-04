exception InvalidPath

(* src_path *)
type src_name = SrcName of string
type src_full = SrcFull of string
type src_path = src_name * src_full

(* traverses a path given and return a result of (file) path list or driver_error *)
let rec traverse_path (SrcFull path) =
  if Sys.file_exists path then 
    if Sys.is_directory path then
      List.concat @@ List.map (fun p -> traverse_path @@ SrcFull (Core.Filename.concat path p)) (Array.to_list @@ Sys.readdir path)
    else
      let name = path |> Core.Filename.basename |> Core.Filename.chop_extension in
      [(SrcName name, SrcFull path)]
  else 
    raise InvalidPath

let traverse_elm_proj_root (SrcFull proj): src_path list =
  let proj = Core.Filename.realpath proj in
  let res = traverse_path @@ SrcFull (Core.Filename.concat proj "src")
  in res

let print_title category content =
  let make_up str = if Core.String.length str mod 2 == 0 then str else str ^ " " in
  (* let center fixed txt = 
    let txt_len = Core.String.length txt in
    let len = max fixed txt_len in
    let side = (len - txt_len) / 2 in
    let blank = Core.String.make side ' ' in
    blank ^ txt ^ blank
  in
  let cat_len = 8 in
  let category = make_up category in
  let category = center cat_len category in *)
  let text = Printf.sprintf " [%s] %s " category content in
  let text = make_up text in
  let txt_len = Core.String.length text in
  let (indent_len, fixed_len, incr_len) = (5, 60, 5) in
  let fixed_len = if fixed_len > txt_len then fixed_len else txt_len + incr_len in
  let r_blank_len = fixed_len - txt_len in
  let blank n = Core.String.make n '-' in
  print_endline @@ blank indent_len ^ text ^ blank r_blank_len
