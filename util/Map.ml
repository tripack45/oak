let add_or_dup (type k) (type v) (map : (k, v, 'wit) Core.Map.t) ~key ~data = 
  let exception Dup of v in
  let update = function
      | None     -> data
      | Some dup -> raise (Dup dup)
  in
  try 
    `Ok (Core.Map.update map key ~f:update)
  with Dup dup -> `Duplicate dup