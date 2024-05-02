exception InvalidPath
exception IsDirectory

(* src_path *)
type src_name = SrcName of string
type src_full = SrcFull of string
type src_path = src_name * src_full

(* traverses a path given and return a result of (file) path list or driver_error *)
(* REQUIRES: 
 * base: a valid path to a directory
 * path: a valid path that extends base in the sense of path
 * ENSURES:
 * a list of pairs, where each pair (a, b) satisfies 
 * a is SrcName (human readable)
 * and b is SrcFull
 *)
let rec traverse_path base (SrcFull path) =
  if Sys.file_exists path then 
    if Sys.is_directory path then
      List.concat @@ List.map (fun p -> traverse_path base @@ SrcFull (Core.Filename.concat path p)) (Array.to_list @@ Sys.readdir path)
    else
      let base_n = List.length (Core.Filename.parts base) in
      let (_, file_name) = Core.List.split_n (Core.Filename.parts path) base_n in
      let file_name = Core.String.concat ~sep:"." file_name in
      let (name, ext) = file_name |> Core.Filename.split_extension in
      let name = Core.String.substr_replace_all name ~pattern:"/" ~with_:"." in
      match ext with
      | Some "elm" -> [(SrcName (name ^ ".elm"), SrcFull path)]
      | _ -> []
  else 
    raise InvalidPath


(* REQUIRES: 
 * path: a valid path parts
 * ENSURES:
 * a list of path parts to .elm files
 * raise InvalidPath if path is not a valid dir
 *)
let rec traverse_path' path =
  let path_str = Core.Filename.of_parts path in
  print_endline path_str;
  if not @@ Sys.file_exists path_str then raise InvalidPath else 
  if not @@ Sys.is_directory path_str then raise IsDirectory else 
  let subs = path_str |> Sys.readdir |> Core.Array.to_list in
  let (dirs, files) = Core.List.partition_tf subs ~f:(
      fun x -> 
        Sys.is_directory @@ Core.Filename.concat path_str x
    )
  in
  let dir_ret = 
    Core.List.concat_map dirs ~f:(
      fun dir -> 
        Core.List.map ~f:(Core.List.cons dir) @@ traverse_path' (path @ [dir]) 
    )
  in
  let file_ret = 
    Core.List.filter_map files ~f:(
      fun file -> 
        let (_, ext) = file |> Core.Filename.split_extension in
        match ext with
        | Some ext when Core.String.lowercase ext = "elm" -> 
          Some ([file])
        | _ -> 
          None
    ) 
  in
  file_ret @ dir_ret

let find_elm (SrcFull src) =
  let proj = Filename_unix.realpath src in
  let src_dir = Core.Filename.parts proj in
  let res = traverse_path' src_dir in
  let res = Core.List.map res ~f:(
    fun ps -> 
      (SrcName (Core.String.concat ps ~sep:"."), Core.Filename.of_parts ps)
  ) in
  res

let print_title category content =
  let make_up str = if Core.String.length str mod 2 = 0 then str else str ^ " " in
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
