(* This file modles the Elm standard library "Core" *)

module R = ElAstResolved

type path = R.Syntax.path

let nullpos = (Lexing.dummy_pos, Lexing.dummy_pos)
let nullnode e = R.Node.node e nullpos

let modcon con : path = R.Syntax.Just (R.MConId.of_string con)
let under_con con path : path = R.Syntax.More (R.MConId.of_string con, path)

module PathAlias : 
sig
  val aliases : (path * path) list 
end =
struct
  let (!) = modcon
  let (@) = under_con

  let aliases : (path * path) list =
    [  
      (!"Array",    "Core" @ !"Array");
      (!"Basics",   "Core" @ !"Basics");
      (!"Bitwise",  "Core" @ !"Bitwise");
      (!"Char",     "Core" @ !"Char");
      (!"Debug",    "Core" @ !"Debug");
      (!"Dict",     "Core" @ !"Dict");
      (!"List",     "Core" @ !"List");
      (!"Maybe",    "Core" @ !"Maybe");

      (!"Platform",            "Core" @ !"Platform");
      ("Platform" @ !"Cmd",    "Core" @ "Platform" @ !"Cmd");
      ("Platform" @ !"Sub",    "Core" @ "Platform" @ !"Sub");

      (!"Process",  "Core" @ !"Process");
      (!"Result",   "Core" @ !"Result");
      (!"Set",      "Core" @ !"Set");
      (!"String",   "Core" @ !"String");
      (!"Task",     "Core" @ !"Task");
      (!"Tuple",    "Core" @ !"Tuple");
    ]
end

(* Every Elm source file implicitly contains the following preemble:
 * https://package.elm-lang.org/packages/elm/core/latest/

import Basics exposing (..)
import List exposing (List, (::))
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String exposing (String)
import Char exposing (Char)
import Tuple

import Debug

import Platform exposing ( Program )
import Platform.Cmd as Cmd exposing ( Cmd )
import Platform.Sub as Sub exposing ( Sub )

*)
module Preamble :
sig
  val preambles : (path * R.Syntax.sigmask) list
end =
struct
  open Core 

  open R.Node
  open R.Syntax

  let (!) = modcon
  let (@) = under_con

  let tycon_any str : Sig.sigmask_tycon = 
    Sig.Any (nullnode (R.TyConId.of_string str))

  let tycon_enumerated str vals : Sig.sigmask_tycon = 
    let vals' = List.map vals ~f:(fun val_str -> nullnode (R.DConId.of_string val_str)) in
    Sig.Enumerated (nullnode (R.TyConId.of_string str), vals')

  let tycon_opaque str : Sig.sigmask_tycon =
    tycon_enumerated str []

  let preambles : (path * sigmask) list =
    [
      ( "Core" @ !"Basics" , Sig.Any);
      ( "Core" @ !"List"   , Sig.Enumerated ([tycon_opaque "List"], []));
      ( "Core" @ !"Maybe"  , Sig.Enumerated ([tycon_any "Maybe"], []));
      ( "Core" @ !"Result" , Sig.Enumerated ([tycon_any "Result"], []));
      ( "Core" @ !"String" , Sig.Enumerated ([tycon_opaque "String"], []));
      ( "Core" @ !"Char"   , Sig.Enumerated ([tycon_opaque "Char"], []));
      ( "Core" @ !"Tuple"  , Sig.Enumerated ([], []));
      ( "Core" @ !"Debug"  , Sig.Enumerated ([], []));

      ( "Core" @ !"Platform", Sig.Enumerated ([tycon_opaque "Program"], []));
      ( "Core" @ "Platform" @ !"Cmd", Sig.Enumerated ([tycon_opaque "Cmd"], []));
      ( "Core" @ "Platform" @ !"Sub", Sig.Enumerated ([tycon_opaque "Sub"], []));
    ]
end

module SigResolved =
struct 
  open Core 

  open R.Node
  open R.Syntax

  let (!) = modcon
  let (@) = under_con

  let sigt tycons vals = R.Syntax.Sig.Sig (tycons, vals) 
  let tc str = nullnode (R.TyConId.of_string str)
  let dc str = nullnode (R.DConId.of_string str)
  let v  str = nullnode (R.VarId.of_string str)
  
  let ($) tc dcs = (tc, dcs)
  
  let sigs : (path * R.Syntax.sigt) list = 
    [
      ("Core" @ !"Basics" , sigt
        [
          tc "Int"   $ [];
          tc "Float" $ [];
          tc "Bool"  $ [dc "True"; dc "False"];
          tc "Never" $ [];
        ]
        [
          (* Todo: fill in common values *)
        ]);

      ("Core" @ !"Maybe", sigt
        [
          tc "Maybe" $ [dc "Just"; dc "Nothing"];
        ]
        [
          v "withDefault";
          v "map";
          v "andThen";
        ]);

      ("Core" @ !"Result", sigt 
        [
          tc "Result" $ [dc "Ok"; dc "Err"];
        ]
        [
          v "withDefault";
          v "map";
          v "andThen";
        ]
      );

      ("Core" @ !"List", sigt 
        [
          tc "List" $ [];
        ]
        [
          v "singleton";
          v "repeat";
          v "map";
          v "indexedMap";
          v "foldl";
          v "foldr";
          v "filter";
          v "filterMap";
          v "length";
          v "reverse";
          v "member";
          v "all";
          v "any";
          v "append";
          v "concat";
          v "concatMap";
          v "sort";
          v "sortBy";
          v "sortWith";
          v "isEmpty";
          v "head";
          v "tail";
          v "take";
          v "drop";
          v "partition";
          v "unzip";
        ]
      );

      ("Core" @ !"String", sigt 
        [
          tc "String" $ [];
        ]
        [
          v "isEmpty";
          v "length";
          v "reverse";
          v "replace";
          v "append";
          v "concat";
          v "split";
          v "join";
          v "contains";
          v "startsWith";
          v "endsWith";
          v "toInt";
          v "fromInt";
          v "toFloat";
          v "fromFloat";
        ]
      );
    ]
end