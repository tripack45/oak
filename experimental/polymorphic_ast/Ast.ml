(* AstCommon.ml
 *
 * This file defines an attribute-carrying AST. The file defines the shape of the AST
 * while in the same time leave open an arbitrary type argument for possible AST annotations
 *
 * The annotation could contain
 * 
 * - Source positional info
 * - Type info if type checking were to be performed on the program
 * - Analysis information of any kind
 *
 * This definition can be extended so that different syntax constructs carries different info
 * but for the purpose of parsing this does not seem neccessary
 *
 * This module also offers the convenience of defining maps and folds on AST nodes
 * E.g. 
 *    Map  : 'a Ast.t -> ('a -> 'b) -> 'b Ast.t 
 *    Fold : 'a Ast.t -> ('a -> 'b) -> (('a * 'b) list -> 'b) -> 'b Ast.t
 * 
 * Can both be trivially coded once for all 'a and 'b
 *)

open Syntax

type pos = Lexing.position * Lexing.position

type paren =
  | Wrapped of int   (* counts number of parens wrapped *)
  | Naked


module type AST_NODES =
sig
  type 't node 
  type 't par_node

  val node : 't -> pos -> 't node

  val naked : 't -> pos -> 't par_node

  val wrapped : 't par_node -> pos -> 't par_node

  val map_node     : ('a -> 'b) -> 'a node -> 'b node
  val map_par_node : ('a -> 'b) -> 'a par_node -> 'b par_node

  val node_elem     : 'a node -> 'a
  val par_node_elem : 'a par_node -> 'a
end

module ParseAstNodes : AST_NODES =
struct 
  (* Ast node *)
  type 't node = 't * pos

  (* Ast node with parentheses annotation *)
  type 't par_node = 't * (pos * paren)

  let node x pos : 't node = (x, pos)

  let naked x pos : 't par_node = (x, (pos, Naked))

  let wrapped (x, (_, par)) pos : 't par_node = 
    match par with 
    | Naked     -> (x, (pos, Wrapped 1))
    | Wrapped i -> (x, (pos, Wrapped (i + 1)))

  let map_node f (elem, attr) = (f elem, attr)
  let map_par_node = map_node
  let node_elem = fst
  let par_node_elem = fst
end

open ParseAstNodes

type path    = Path.t node
type field   = FieldId.t node
type var     = Var.t  node
type qvar    = (path, var) QVar.t node
type con     = Con.t  node
type qcon    = (path, con) QCon.t node
type lit     = Literal.t node
type pat     = _pat par_node and _pat = (var, lit, qcon, pat) Pattern.pattern
type typ     = ('t Typ.typ) node as 't
type expr    = _expr par_node and _expr = (decl, pat, field, qcon, qvar, lit, expr) Expr.expr
and  decl    = (var, pat, expr, typ) Decl.decl node
type import  = (qcon, con, var) Module.Import.import node
type mdecl   = (con, var) Module.Decl.decl node
type m       = (mdecl, import, decl) Module.m

(* This module will be opened and exposed to the parser *)


(* This module provides recursion schemes on the AST *)
module Scheme =
struct
 
  open ParseAstNodes

  type ('var, 'con, 'path, 'lit, 'field, 
        'qv, 'qc, 'typ, 'pat, 'expr, 'decl, 'mdecl, 'imp, 'm) fold' = 
  { fvar    : var -> 'var;
    fcon    : con -> 'con;
    flit    : lit -> 'lit;
    ffield  : field -> 'field;
    fqvar'  : ('path, 'var) QVar.t node -> 'qc;
    fqcon'  : ('path, 'con) QCon.t node -> 'qv;
    fpat'   : ('var, 'lit, 'qc, 'pat) Pattern.pattern node -> 'pat;
    ftyp'   : ('typ) Typ.typ node -> 'typ;
    fdecl'  : ('var, 'pat, 'expr, 'typ) Decl.decl node -> 'decl;
    fexpr'  : ('decl, 'pat, 'field, 'qc, 'qv, 'lit, 'expr) Expr.expr node -> 'expr;
    fmdecl' : ('con, 'var) Module.Decl.decl node -> 'mdecl;
    fimp'   : ('qc, 'con, 'var) Module.Import.import node -> 'imp;
    fmod'   : ('mdecl, 'imp, 'decl) Module.m node -> 'm;
  }

  type ('var, 'con, 'path, 'lit, 'field, 
        'qv, 'qc, 'typ, 'pat, 'expr, 'decl, 'mdecl, 'imp, 'm) folds = 
  { fqvar  : qvar -> 'qv;
    fqcon  : qcon -> 'qc;
    fpat   : pat -> 'pat;
    ftyp   : typ -> 'typ;
    fdecl  : decl -> 'decl;
    fexpr  : expr -> 'expr;
    fmdecl : mdecl -> 'mdecl;
    fimp   : import -> 'imp;
    fmod   : m -> 'm;
  }

  let folds ~(fvar   : var -> 'var)
            ~(fcon   : con -> 'con)
            ~(fpath  : path -> 'path)
            ~(flit   : lit -> 'lit)
            ~(ffield : field -> 'field)
            ~fqvar: (fqvar' :  ('path, 'var) QVar.t node -> 'qv )
            ~fqcon: (fqcon' :  ('path, 'con) QCon.t node -> 'qc )
            ~ftyp:  (ftyp'  :  ('typ) Typ.typ node -> 'typ)
            ~fpat:  (fpat'  :  ('var, 'lit, 'qc, 'pat) Pattern.pattern par_node -> 'pat)
            ~fexpr: (fexpr' :  ('decl, 'pat, 'field, 'qc, 'qv, 'lit, 'expr) Expr.expr par_node -> 'expr)
            ~fdecl: (fdecl' :  ('var, 'pat, 'expr, 'typ) Decl.decl node -> 'decl)
            ~fmdecl:(fmdecl':  ('con, 'var) Module.Decl.decl node -> 'mdecl )
            ~fimp:  (fimp'  :  ('qc, 'con, 'var) Module.Import.import node -> 'imp  )
            ~fmod:  (fmod'  :  ('mdecl, 'imp, 'decl) Module.m -> 'm)
            : ('var, 'con, 'path, 'lit, 'field, 
               'qv, 'qc, 'typ, 'pat, 'expr, 'decl, 'mdecl, 'imp, 'm) folds
            = 
    let node_map = map_node in
    let rec fqvar qv = fqvar'  @@ node_map (QVar.fold ~fvar ~fpath) @@ qv
        and fqcon qc = fqcon'  @@ node_map (QCon.fold ~fcon ~fpath) @@ qc
        and fpat (p: pat)   = fpat'   @@ map_par_node (Pattern.fold ~fvar ~flit ~fqcon ~fpat) @@ p
        and ftyp t   = ftyp'   @@ node_map (Typ.fold ~ftyp:ftyp) @@ t
        and fdecl d  = fdecl'  @@ node_map (Decl.fold ~fvar ~fpat ~ftyp ~fexpr) @@ d
        and fexpr (e : expr)  = fexpr'  @@ map_par_node (Expr.fold ~fdecl ~fpat ~ffield ~fqcon ~fqvar ~flit ~fexpr) @@ e
        and fmdecl d = fmdecl' @@ node_map (Module.Decl.fold ~fcon ~fvar) @@ d 
        and fimp i   = fimp'   @@ node_map (Module.Import.fold ~fqcon ~fcon ~fvar) @@ i
        and fmod m   = fmod'   @@ (Module.fold ~fmdecl ~fimp ~fdecl) @@ m
    in { fqvar; fqcon ; fpat ; ftyp; fdecl; fexpr; fmdecl; fimp; fmod }

  (* We are folding over trees but we are not interested in base elements *)
  let folds_triv_base ~fqvar ~fqcon ~ftyp ~fpat ~fexpr ~fdecl ~fmdecl ~fimp ~fmod = 
    let id x = x in
    folds ~fvar:id ~fcon:id ~fpath:id ~flit:id ~ffield:id
          ~fqvar ~fqcon ~ftyp ~fpat ~fexpr ~fdecl ~fmdecl ~fimp ~fmod

  (* We are folding over trees but we don't care attribute values *)
  let folds_prune_attr ~fvar ~fcon ~fpath ~flit ~ffield
                       ~fqvar ~fqcon ~ftyp ~fpat ~fexpr ~fdecl ~fmdecl ~fimp ~fmod =
    let ($) f g x = f (g x) in
    folds ~fvar:(fvar @@ node_elem)   
          ~fcon:(fcon @@ node_elem)  
          ~fpath:(fpath @@ node_elem) 
          ~flit:(flit @@ node_elem) 
          ~ffield:(ffield @@ node_elem) 
          ~fqvar:(fqvar @@ node_elem) 
          ~fqcon:(fqcon @@ node_elem) 
          ~ftyp:(ftyp @@ node_elem)
          ~fpat:(fpat @@ par_node_elem) 
          ~fexpr:(fexpr @@ par_node_elem)
          ~fdecl:(fdecl @@ node_elem)
          ~fmdecl:(fmdecl @@ node_elem)
          ~fimp:(fimp @@ node_elem)
          ~fmod:(fmod)
end

let {fmod; _} : (string, string, string, string, string,
                 string, string, string, string, string,
                 string, string, string, string) Scheme.folds =
  let fvar var = 
    assert false 

  and fcon con = 
    assert false

  and fpath path =
    assert false

  and flit lit = 
    assert false
    
  and ffield lit = 
    assert false

  and fqvar qvar =
    assert false

  and fqcon qcon = 
    assert false

  and ftyp ftyp =
    assert false

  and fpat fpat =
    assert false

  and fexpr expr =
    assert false

  and fdecl expr = 
    assert false

  and fmdecl mdecl = 
    assert false

  and fimp imp =
    assert false

  and fmod m =
    assert false
  
  in Scheme.folds ~fvar ~fcon ~fpath ~flit ~ffield 
                  ~fqvar ~fqcon ~ftyp ~fpat ~fexpr ~fdecl ~fmdecl ~fimp ~fmod 


(* let rec mod_to_string m = 
  let fimp _ = "import" in
  let fmddecl _ = "module" in
  let (mdecl_str, imp_strs, rdecl_strs) = Module.fold ~fmdecl ~fimp ~fdecl m in
  Printf.sprintf ("%s { \n%s\n}\n") 
                 (Option.value mdecl_str ~default:"module")
                 (String.concat ";\n" (imp_strs @ rdecl_strs))

and fdecl ((decl, _) : decl) : string = 
  match Decl.fold fvar fpat fexpr ftyp decl with
  | Decl.TyCon -> "TyCon"
  | Decl.Annot (vstr, tstr) -> vstr ^ " :: " ^ tstr
  | Decl.Pat   (pstr, estr) -> pstr ^ " = " ^ estr
  | Decl.Fun   ((vstr, pats), estr) ->
    Printf.sprintf "fun %s %s = %s" vstr (String.concat " " pats) estr

and fpat (pat, _) = 
  match Pattern.fold fvar flit fqcon fpat pat with
  | Var vstr  -> vstr
  | Any       -> "_"
  | Unit      -> "( )"
  | EmptyList -> "[ ]"
  | Literal lstr -> lstr
  | List pstrs  -> Printf.sprintf "[%s]" (String.concat ", " pstrs)
  | Tuple pstrs  -> Printf.sprintf "(%s)" (String.concat ", " pstrs)
  | Ctor (qconstr, pstrs) -> 
    match pstrs with 
    | [] -> qconstr
    | _ -> Printf.sprintf "%s of %s" qconstr (String.concat " " pstrs)

and fexpr (expr, _) =
  match Expr.fold fdecl fpat ffield fqcon fqvar flit fexpr expr with
  | Let (decls, e)     -> assert false
  | Case (e, branches) -> 
    Printf.sprintf "case %s of {%s}" 
        e 
        (List.map (fun (p, e) -> p ^ " -> " ^ e) branches |> String.concat " | ")
  | If (e1, (e2, e3))  -> assert false
  | Lambda (pats, e)   -> assert false
  | App (e, es)        -> String.concat " " (e::es)
  | Infix (op, e1, e2) -> e1 ^ " op " ^ e2
  | OpFunc op          -> "(op)"
  | Unit               -> "( )"
  | Tuple es           -> "(" ^ String.concat ", " es ^ ")"
  | List es            -> "[" ^ String.concat ", " es ^ "]"
  | Record field_es    -> 
    Printf.sprintf "<{ %s }>" (List.map (fun (f, e) -> f ^ " = " ^ e) field_es |> String.concat "; ")
  | Con (cr, es)       -> cr ^ String.concat " " es
  | Var vr             -> vr
  | Literal l          -> l

and ffield (field, _) = FieldId.to_string field

and fqcon ((path, con), _) = ConId.to_string (fst con)

and fqvar ((path, var), _) = fvar var

and flit (lit, _) = 
  match lit with
  | Literal.Int s    -> s
  | Literal.Float s  -> s
  | Literal.String s -> Printf.sprintf "\"%s\"" s

and fvar (var, _) = Var.to_string var

and ftyp _ = "TVoid" *)

