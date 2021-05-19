(* Collects a utilities aparatus that are shared among the entrie project *)

module Format =
struct
  (* Maps function f over the list l, then concatenates 
   * the result with separator sep *)
  let concat_map sep f l = 
    String.concat sep (List.map f l)
  
  let braces   = ("{", "}")
  let brackets = ("[", "]")
  let parens   = ("(", ")")

  (* Surrounds a strings with a pair of strings *)
  let surround (open_s, close_s) s =
    Printf.sprintf "%s%s%s" open_s s close_s
end

module List =
struct

end