let xs = [ 81; 88; 83; 94; 84; 21; 72; 27;  4; 55; 42;  2; 13; 97; 24; 22; 
           12; 91; 23; 68; 10; 51; 25;100; 32; 54; 70; 92;  8; 66; 47; 64; 
           40; 85; 46; 33; 35; 69;  5; 48; 62; 79; 65; 98; 71; 49; 17; 14; 
           63; 19; 93; 58; 16; 45; 95; 56; 99; 57; 11; 44;  9; 75; 30; 28; 
           78; 37; 90;  1; 34; 43; 20; 52; 60; 29;  7; 53; 50; 67; 36; 76; 
           73; 82; 41; 38;  3; 59; 26; 39; 96; 80; 87; 77; 15;  6; 61; 86; 
           74; 89; 31; 18]

let dump_ints xs =
  List.map Int.to_string xs |> String.concat " " |> print_endline


let rec qsort xs = 
  match xs with 
  | [] -> []
  | x::xs -> 
    let ge  = List.filter ((<)  x) xs in
    let leq = List.filter ((>=) x) xs in 
    qsort leq @ (x :: qsort ge)

let rec insert_sort xs =
  let rec insert x xs =
    match xs with 
    | [] -> [ x ]
    | x'::xs when x > x' -> x' :: insert x xs
    | _ -> x :: xs
  in match xs with 
    | []    -> []
    | x::xs -> insert x (insert_sort xs)

let rec merge_sort xs = 
  let rec merge xs xs' = 
    match (xs, xs') with
    | ([]   , xs'    ) -> xs'
    | (xs   , []     )  -> xs
    | (x::xs, x'::xs') -> 
      if x < x' then
        x  :: x' :: merge xs xs' 
      else
        x' :: x :: merge xs xs'
  in
  let odd  = List.filteri (fun i _ -> i mod 2 = 1) xs in
  let even = List.filteri (fun i _ -> i mod 2 = 0) xs in
  merge (merge_sort odd) (merge_sort even)

let rec bubble_sort xs = 
  let rec bubble x xs =
    match xs with 
    | []     -> (x, [])
    | x'::xs -> 
      let (min, max) = if x <= x' then (x, x') else (x', x) in
      let (min', l)  = bubble min xs in
      (min', max :: l)
  in 
    match xs with 
    | [] -> []
    | x::xs -> 
      let (min, xs) = bubble x xs in 
      min :: bubble_sort xs
    

(* Dump the original *)
(* let () = dump_ints xs
let () = dump_ints (qsort xs)
let () = dump_ints (insert_sort xs)
let () = dump_ints (bubble_sort xs) *)