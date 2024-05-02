(*
 * This pass performs type checking and type inference
 *)

open ElAstResolved
open Core

module Ast = ElAstResolved.Syntax
module A = ElAstResolved.Alias

module Path = ElAst.Path

module Map = Core.Map
module Set = Core.Set

module TVarId  = ElAstResolved.TVarId
module DConId  = ElAstResolved.DConId
module TyConId = ElAstResolved.TyConId
module MConId  = ElAstResolved.MConId

module Rst = Util.PassResult.WarnListErrorList
open Rst.Pervasive

type warn = 
  | Warn
  (* | MissingModuleSig     of R.path'
  | UnresolvedVarId      of R.varid'
  | UnresolvedDConId     of R.dconid'
  | UnresolvedTyConId    of R.tyconid'
  | VarShadowingInArgs   of R.varid' * R.varid'
  | ImportShadowingVar   of (R.varid'   * R.path) * R.path
  | ImportShadowingDCon  of (R.dconid'  * R.path) * R.path
  | ImportShadowingTyCon of (R.tyconid' * R.path) * R.path *)

type err = 
  | Err
  (* | RepeatedBinder         of R.varid'   * (R.varid' * P.pat')
  | VarIdRedefined         of R.varid'   * R.varid' 
  | TyConIdRedefined       of R.tyconid' * R.tyconid'
  | DConIdRedefined        of R.dconid'  * R.dconid'
  | RepeatedAnnotation     of (P.var' * P.typ') * (P.var' * P.typ')
  | RepeatedPort     of (P.var' * P.typ') * (P.var' * P.typ')
  | DanglingTypeAnnotation of (P.var' * P.typ')
  | RepeatedTypeVar        of P.tvar' * P.tycon'  *)

type 'a rslt = ('a, warn, err) Rst.t

(* TyCons and DCons must use separate map because they live in different 
 * namespaces, hence name collisions between them would be bad. Consider this:
 * 
 * import ModuleX exposing ( Con(..) )
 * import ModuleY exposing ( TyCon(Con) )
 *
 * In this example uses of Con can either be ModuleX.Con or ModuleY.Con, depending
 * whether Con is being used as a TyCon or a DCon
 *)
module CtxOpen =
struct
  type tctx = Ast.path TyConId.Map.t
  type dctx = Ast.path DConId.Map.t
  type vctx = Ast.path VarId.Map.t

  type t = 
  {
    tctx : tctx;
    dctx : dctx;
    vctx : vctx;
  }

  let empty : t = 
  {
    tctx  = TyConId.Map.empty;
    dctx  = DConId.Map.empty;
    vctx  = VarId.Map.empty;
  }
end

(* tctx : Type Constructor Context
 * dctx : Data Constructor Context
 * vctx : Variable Context
 *  
 * Resolution of data constructors must be deferred to until typechecking. Although Elm prevents
 * shadowing of data constructors (at least shadowing between locally defined data constructors), such
 * restrictions does makes it harder to code therefore we might want to relax such restrictions. 
 * Such design would require the compiler to disambiguous data constructors of same name at type-checking
 * based on the type of the constructor inferred (or defaults to a choice). Therefore we must maintain 
 * name even for locally defined data constructors.
 *)

module Ctx =
struct
  type tctx  = TyCon.t TyConId.Map.t
  type dctx  = DCon.t  DConId.Map.t
  type vctx  = Var.t   VarId.Map.t
  type tvctx = TVar.t  TVarId.Map.t

  type t = 
  {
    tctx  : tctx;
    dctx  : dctx;
    vctx  : vctx;
    tvctx : tvctx;
  }

  let empty_tctx = TyConId.Map.empty
  let empty_dctx = DConId.Map.empty
  let empty_vctx = VarId.Map.empty
  let empty_tvctx = TVarId.Map.empty

  let empty : t = 
  {
    tctx  = TyConId.Map.empty;
    dctx  = DConId.Map.empty;
    vctx  = VarId.Map.empty;
    tvctx = TVarId.Map.empty;
  }

end


let tc_mod (m : Ast.m) : unit rslt = 
  ok ()

let run (mods : ElAstResolved.Syntax.m list) = 
  Rst.run 
    begin
      let mctx = Path.Map.of_alist_exn (ElmCore.SigResolved.sigs) in
      Rst.Seq.folding_map mods ~init:(ok mctx) ~f:(
        fun mctx m ->
          let* m' = tc_mod m in
          ok (mctx, m')
      )
    end