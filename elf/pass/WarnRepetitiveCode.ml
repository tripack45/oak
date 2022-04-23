(* WarnRepetitiveCode.ml
 *
 * This files contains the analysis-report pass WarnRepetitveCode. This 
 * analysis first map some ast nodes to string that are considered as high 
 * risk for repetitive code (1. All functions, 2. All branches in Case,
 * 3. All branches in let). Then run meyer's differ algorithm on those strings.
 * If "\\" num divide "-" or "|" num is larger than 0.9, output warnings.
 *
 *)

open Core
open Oaklib
open PassUtil
open ElAst.Syntax
open ElAst.Node
let (>.) = Core.Float.(>.)

module RepeatToString =
struct 
  open Printf
  open ElAst
  open ElAst.Syntax
  open Util.Format

  let pos_to_string (s, e) = 
    let pos2lc (s : Lexing.position) = 
      let line_num = s.pos_lnum in
      let col = s.pos_cnum - s.pos_bol in
      (line_num, col)
    in 
    let (l, c)   = pos2lc s in
    let (l', c') = pos2lc e in
    Printf.sprintf "%d:%d - %d:%d" l c l' c'

  let field_to_string _ = 
    "field"

  let lit_to_string lit = 
    match Node.elem lit with
    | Int    s -> s
    | Float  s -> s
    | Char   s -> surround ("\'", "\'") s
    | String s -> surround ("\"", "\"") s
  
  let var_to_string _  = "var"
  let tvar_to_string _ = "tvar"
  let tycon_to_string _ = "tycon"
  let dcon_to_string _ = "dcon"
  let mcon_to_string _ = "mcon"

  let path_to_string path : string = 
    let to_string p = 
      match p with 
      | Just _ -> "module"
      | More (_, _) -> "module" ^ "." ^ "func"
    in to_string (Node.elem path) 

  let qid_to_string ~extract  qid =
    let (path, _) = extract @@ Node.elem qid in
    match Option.map ~f:path_to_string path with 
    | Some path_str -> path_str ^ "." ^ "id"
    | None -> "id"
  
  let qtycon_to_string_helper _ = "qtycon"
  
  let qtycon_to_string qtycon =
    qid_to_string ~extract:(fun (QTyCon (p_opt, tycon)) -> (p_opt, tycon))
                  qtycon
  
  let qdcon_to_string_helper _ = "qdcon"
  
  let qdcon_to_string qdcon =
    qid_to_string ~extract:(fun (QDCon (p_opt, tycon)) -> (p_opt, tycon))
                  qdcon

  let qvar_to_string qvar =
    qid_to_string ~extract:(fun (QVar (p_opt, tycon)) -> (p_opt, tycon))
                  qvar

  let rec m_to_string (Mod (mdecl, imports, decls)) =
    let mdecl_str   = Option.map ~f:mdecl_to_string  mdecl   in
    let import_strs = List.map  ~f:import_to_string imports in 
    let decl_strs   = List.map   ~f:decl_to_string   decls   in
    sprintf ("%s {%s}") 
            (Option.value mdecl_str ~default:"module")
            (String.concat ~sep:";" (import_strs @ decl_strs))
  and exposing_to_string exposing = 
    let id_to_string (id : exposing_ident) =
      match id with
      | Var var    -> var_to_string var
      | TyCon con  -> tycon_to_string con ^ "(..)"
      | AbsTyCon con -> tycon_to_string con
    in
    match exposing with 
    | Any -> "(..)"
    | Idents ids -> surround ("(", ")") @@ concat_map ", " id_to_string ids 
    
  and mdecl_to_string (MDecl (path, exposing_opt)) = 
    match exposing_opt with 
    | Some exposing -> 
      sprintf "module %s exposing %s" (path_to_string path) (exposing_to_string exposing)
    | None -> 
      sprintf "module %s" (path_to_string path)

  and import_to_string (Import (path, as_opt, exposing_opt)) = 
    let exposing_str = 
      match Option.map ~f:exposing_to_string exposing_opt with 
      | Some s -> " exposing " ^ s 
      | None -> ""
    in
    let as_str = 
      match Option.map ~f:mcon_to_string as_opt with
      | Some s -> " as " ^ s
      | None -> ""
    in 
    (sprintf "import %s%s%s" (path_to_string path) exposing_str as_str)

  and decl_to_string decl = 
    match (Node.elem decl) with
    | TyCon ((con, vars), dcons) ->
      let dcon_to_string (dcon, typs) = 
        sprintf "%s %s" (dcon_to_string dcon)
                        (concat_map " " typ_to_string typs)
      in
      sprintf "datatype %s %s = {%s}" 
              (tycon_to_string con)
              (concat_map " " tvar_to_string vars)
              (concat_map "|" dcon_to_string dcons)
    | Alias ((con, vars), typ) -> 
      sprintf "type %s %s = %s" 
              (tycon_to_string con)
              (concat_map " " tvar_to_string vars)
              (typ_to_string typ)
    | Annot (var, t) -> "annot " ^ var_to_string var ^ " :: " ^ typ_to_string t
    | Port  (var, t) -> "port " ^ var_to_string var ^ " :: " ^ typ_to_string t
    | Pat   (pat, e) -> 
      sprintf "val %s = %s" 
                    (pat_to_string pat) 
                    (expr_to_string e)
    | Fun   ((var, pats), e) ->
      sprintf "fun %s %s = %s" 
                    (var_to_string var) 
                    (concat_map " " pat_to_string pats) 
                    (expr_to_string e)

  and pat_to_string pat = 
    match Node.elem pat with
    | Var var       -> var_to_string var
    | Any           -> "_"
    | Unit          -> "()"
    | EmptyList     -> "[]"
    | Literal lit   -> lit_to_string lit
    | Alias (p, v)  -> pat_to_string p ^ " as " ^ var_to_string v
    | Cons (p1, p2) -> pat_to_string p1 ^ " :: " ^ pat_to_string p2
    | List pats     -> surround ("[", "]") @@ concat_map ", " pat_to_string pats
    | Tuple pats    -> surround ("(", ")") @@ concat_map ", " pat_to_string pats
    | Record (fields) -> surround ("{", "}") @@ concat_map ", " (fun (f, _pat) -> FieldId.to_string f) fields
    | DCon (qdcon, pats) -> 
      match pats with 
      | [] -> qdcon_to_string qdcon
      | _ -> sprintf "%s of %s" (qdcon_to_string qdcon)  (concat_map " " pat_to_string pats)

  and op_to_string = function 
    | APL     -> "<@"
    | APR     -> "@>"
    | OR      -> "or"
    | AND     -> "and"
    | EQU     -> "=="
    | NE      -> "/="
    | GT      -> ">"
    | LT      -> "<"
    | GEQ     -> ">="
    | LEQ     -> "<="
    | CONS    -> "::"
    | APPEND  -> "++"
    (* Arithematics *)
    | PLUS     -> "+"
    | MINUS    -> "-"
    | TIMES    -> "*"
    | FDIV     -> "/"
    | IDIV     -> "//"
    | POW      -> "^"
    | COMPOSEL -> "<<"
    | COMPOSER -> ">>"
  and uop_to_string = function
    | UMINUS   -> "-"

  and expr_to_string e =
    let pp = expr_to_string in
    let expr_str =
      match Node.elem e with
      | Let (decls, e)     -> 
        let decl_strs = "{" ^ concat_map ";" decl_to_string decls ^ "}" in
        sprintf "let %s in %s" decl_strs (pp e) 
      | Case (e, branches) -> 
        let arrow (p, e) = pat_to_string p ^ " -> " ^ pp e in
        sprintf "case %s of {%s}" (pp e) (concat_map "|" arrow branches)
      | If (e1, (e2, e3))  -> sprintf "if %s then {%s} else {%s}" (pp e1) (pp e2) (pp e3)
      | Lambda (pats, e)   -> sprintf "fn %s = {%s}" (concat_map " " pat_to_string pats) (pp e)
      | App (e, es)        -> concat_map " " pp (e::es)
      | Infix (op, e1, e2) -> sprintf "%s %s %s" (pp e1) (op_to_string op) (pp e2)
      | Unary (uop, e)     -> sprintf "%s%s" (uop_to_string uop) (pp e)
      | OpFunc op          -> surround ("(", ")") (op_to_string op)
      | Unit               -> "()"
      | Tuple es           -> surround ("(", ")") @@ concat_map ", " pp es
      | List es            -> surround ("[", "]") @@ concat_map ", " pp es
      | Record field_es    -> 
        let record_field (f, e) = field_to_string f ^ " = " ^ pp e in
        surround ("<{", "}>") @@ (concat_map ";" record_field field_es)
      | ProjFunc   f       -> sprintf "(.%s)" (field_to_string f)
      | Project    (_, f)  -> sprintf "%s.%s" "object" (field_to_string f)
      | Extension  (e, fs) -> 
        let record_field (f, e) = field_to_string f ^ " = " ^ pp e in
        surround ("<{", "}>") @@ sprintf "%s @ %s" (pp e) (concat_map ";" record_field fs)
      | DCon (qdcon, es)   -> qdcon_to_string qdcon ^ concat_map " " pp es
      | Var qvar           -> qvar_to_string qvar
      | Literal lit        -> lit_to_string lit
    in 
    match snd (Node.attr e) with
    | Naked -> expr_str
    | Wrapped _ -> surround ("(", ")") expr_str
    

  and typ_to_string typ = 
    match Node.elem typ with 
    | TVar tvar -> Node.fold_elem TVarId.to_string tvar
    | Unit -> "()"
    | TyCon c -> qtycon_to_string c
    | Arrow (t1, t2) -> typ_to_string t1 ^ " -> " ^ typ_to_string t2
    | TApp (t1, t2) -> typ_to_string t1 ^ " " ^ typ_to_string t2
    | Tuple typs -> surround ("(", ")") @@ concat_map ", " typ_to_string typs
    | Record row -> surround ("<", ">") @@ row_to_string row

  and row_to_string row =
    let field_to_string (field, typ) =
       field_to_string field ^ ": " ^ typ_to_string typ
    in
    match row with 
    | RVar rvar -> Node.fold_elem TVarId.to_string rvar
    | Extension (r, fields) ->
      sprintf "%s & %s" (row_to_string r) (concat_map ", " field_to_string fields)
    | Fields fields -> concat_map ", " field_to_string fields
end

module RepetitiveCode = struct
  let compare_string_dump string1 string2 pos1 pos2 src ctx = 
    let dp = Array.make_matrix ~dimx:(String.length string1 + 1) ~dimy:(String.length string2 + 1) (0, ' ') in
    let min3 ele1 ele2 ele3 =
      let ((step1, _), (step2, _), (step3, _)) = (ele1, ele2, ele3) in
        if (step1 <= step2 && step1 <= step3) then
          ele1
        else
          if (step2 <= step3) then
            ele2
          else
            ele3
    in
    let () = Array.foldi ~init:() ~f:(
      fun i () _ ->
        Array.foldi ~init:() ~f:(
          fun j () _ ->
            if i = 0 then
              if j = 0 then
                ()
              else
                dp.(0).(j) <- (Tuple2.get1 dp.(0).(j - 1) + 1, '-')
            else if j = 0 then
              dp.(i).(0) <- (Tuple2.get1 dp.(i - 1).(0) + 1, '|')
            else
              let dp_tmp3 = 
                if (Core.equal_char (String.get string1 (i - 1)) (String.get string2 (j - 1))) then
                  (Tuple2.get1 dp.(i - 1).(j - 1) + 1, '\\')
                else
                  (99999, ' ')
                in
              dp.(i).(j) <- min3 (Tuple2.get1 dp.(i).(j - 1) + 1, '-') (Tuple2.get1 dp.(i - 1).(j) + 1, '|') dp_tmp3
        ) dp.(i)
    ) dp in
    let slashNum = ((String.length string1 + String.length string2)- ((Tuple2.get1 dp.(String.length string1).(String.length string2)))) in
    let minLength = (min (String.length string1) (String.length string2)) in
    let ratio = (float_of_int slashNum) /. (float_of_int minLength) in
    if ratio >. 0.9 then
      let _ = 
        if ratio >. 0.999 then
          let rec repeat str times out =
            if times = 0 then
              out
            else
              repeat str (times - 1) (out ^ str)
          in
          Printf.printf "%s" ((repeat "#" 40 "") ^ "\n" ^ (repeat "#" 11 "") ^ "Ratio=1.0 detected" ^ (repeat "#" 11 "") ^ "\n" ^ (repeat "#" 40 ""))
      in
      let _ = Printf.printf "Find possibly simular two block of codes.\n" in
      let _ = List.iter ctx ~f:dump_binding in
      Printf.printf "\n%s\n%s\n\n%s\n%s\n\n%s\n\n%s\n\n \"\\\"num: %d, min length: %d, ratio:%f\n\n\n\n"
        (ElAst.ToString.pos_to_string pos1)
        (Src.Source.lines src pos1)
        (ElAst.ToString.pos_to_string pos2)
        (Src.Source.lines src pos2)
        string1
        string2
        slashNum
        minLength
        ((float_of_int slashNum) /. (float_of_int minLength))


  let analysis_repetitive (Mod (mdecl, _imports, decls)) src =
    let ctx = 
      BindingContext.dump(
      Core.Option.value_map mdecl
      ~f:(
        fun (MDecl (mod_path, _)) ->
          let (_, pos) = both mod_path in
          BindingContext.add BindingContext.empty (BindingContext.Mod mod_path, pos) 
      )
      ~default:BindingContext.empty
      )
    in
    let check_string_list string_pos_list =
      List.iteri ~f:(
      fun i (string1, pos1) ->
        List.iteri ~f:(
          fun j (string2, pos2) ->
            let (string1_length, string2_length) = (String.length string1, String.length string2) in
            if i <= j (* make sure every two strings are compared only once *)
              || abs(string1_length - string2_length) > 50 (* skip judgement on two strings if their difference on length > 50*)
              || string1_length < 100 || string2_length < 100 then (* or any of two strings has length < 100 *)
              ()
            else
              compare_string_dump string1 string2 pos1 pos2 src ctx
        ) string_pos_list
    ) string_pos_list
    in
    let rec check_expr' expr' =
      let expr = ElAst.Node.elem expr' in
      let check_expr'_list expr_list  =
        let _ = List.map expr_list ~f:(fun expr -> check_expr' expr) in
        ()
      in
      let check_field_expr'_list field_expr_list = 
        let expr_list = List.map field_expr_list ~f:(fun (_, expr) -> expr) in
        check_expr'_list expr_list
      in
      let check_let_decl'_list decl'_list =
        let decl_string_pos = List.map decl'_list ~f:(fun decl' -> (RepeatToString.decl_to_string decl', attr decl')) in
        check_string_list decl_string_pos
      in
      match expr with
      | Case (_, branches) ->
        let expr'_to_pos expr' = 
          let (pos, _) = (attr expr') in
          pos
        in
        let case_string_pos = List.map branches ~f:(fun (_, expr') -> (RepeatToString.expr_to_string expr', expr'_to_pos expr')) in
        check_string_list case_string_pos
      | Let (decl'_list, expr') ->
        let _ = check_expr' expr' in
        let _ = List.map decl'_list ~f:(fun decl' -> check_decl' decl') in
        check_let_decl'_list decl'_list
      | If (expr1', (expr2', expr3')) ->
        let _ = check_expr' expr1' in
        let _ = check_expr' expr2' in
        check_expr' expr3'
      | App (expr', expr'_list) ->
        let _ = check_expr' expr' in
        let _ = check_expr'_list expr'_list in
        ()
      | Infix (_, expr1', expr2') ->
        let _ = check_expr' expr1' in
        check_expr' expr2'
      | Unary (_, expr') ->
        check_expr' expr'
      | Tuple expr'_list ->
        check_expr'_list expr'_list
      | List expr'_list ->
        check_expr'_list expr'_list
      | Record field_expr'_list ->
        check_field_expr'_list field_expr'_list
      | Project (expr', _) ->
        check_expr' expr'
      | Extension (expr', field_expr'_list) ->
        let _ = check_expr' expr' in
        check_field_expr'_list field_expr'_list
      | DCon (_, expr'_list) ->
        check_expr'_list expr'_list
      | _ -> ()
    and check_decl' decl' =
      let (decl, _) = both decl' in
        match decl with
        | Fun (_, expr') ->
            check_expr' expr'
        | _ ->
          ()
    in
    let _ = List.iter decls ~f:check_decl' in
    let function_string_pos = List.map decls ~f:(fun decl' ->
      let (decl, pos) = both decl' in
        match decl with
        | Fun ((var', pat's), expr') ->
          let pat_string = List.fold pat's ~init: "" ~f:(fun accum pat -> accum ^ RepeatToString.pat_to_string pat ^ " ") in
          let function_string = (RepeatToString.var_to_string var') ^ pat_string ^ (RepeatToString.expr_to_string expr') in
          (function_string, pos)
        | _ ->
          ("", pos)
    )
    in
    check_string_list function_string_pos
end