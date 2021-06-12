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
