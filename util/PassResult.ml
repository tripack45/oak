open Core

module Sig =
struct
  module type PAR =
  sig
      type ('r, 'w, 'e) t
      val map         : 'r list -> f:('r -> ('rr, 'w, 'e) t) -> ('rr list, 'w, 'e) t
      val iter        : 'r list -> f:('r -> (unit, 'w, 'e) t) -> (unit, 'w, 'e) t
      val filter      : 'r list -> f:('r -> (bool, 'w, 'e) t) -> ('r list, 'w, 'e) t
      val filter_map  : 'r list -> f:('r -> ('r2 option, 'w, 'e) t) -> ('r2 list, 'w, 'e) t

      (* EXPERIMENTAL: 
        * 
        *   Function "g m1 m2 = (m1 ** m2) >>= f" must be associative 
        * 
        * Normally parallel composition does not offer folds as folds are inherently sequential.
        * A limited version of "fold", namely the "reduce" function may be provided. It is unclear
        * whether the current interface make the most sense. Other candidates:
        * 
        * - First argument takes the type ('r, 'w, 'e) t list 
        * - Third argument may take the  ('r, 'w, 'e) t  -> ('r, 'w, 'e) t -> ('r, 'w, 'e) t
        *)
      val reduce     : 'r list -> init:('r, 'w, 'e) t -> f:('r -> 'r -> ('r, 'w, 'e) t) -> ('r, 'w, 'e) t
      val reduce_exn : 'r list -> f:('r -> 'r -> ('r, 'w, 'e) t) -> ('r, 'w, 'e) t
  end

  module type SEQ =
  sig
    type ('r, 'w, 'e) t
    val map         : 't list -> f:('t -> ('r, 'w, 'e) t) -> ('r list, 'w, 'e) t
    val iter        : 'r list -> f:('r -> (unit, 'w, 'e) t) -> (unit, 'w, 'e) t
    val fold_left   : 't list -> init:('r, 'w, 'e) t -> f:('r -> 't -> ('r, 'w, 'e) t) -> ('r, 'w, 'e) t
    val fold_right  : 't list -> init:('r, 'w, 'e) t -> f:('t -> 'r -> ('r, 'w, 'e) t) -> ('r, 'w, 'e) t
    val fold        : 't list -> init:('r, 'w, 'e) t -> f:('r -> 't -> ('r, 'w, 'e) t) -> ('r, 'w, 'e) t
    val foldi       : 't list -> init:('r, 'w, 'e) t -> f:(int -> 'r -> 't -> ('r, 'w, 'e) t) -> ('r, 'w, 'e) t
    val fold_map    : 't list -> init:('r, 'w, 'e) t -> f:('r -> 't -> ('r * 't2, 'w, 'e) t) -> ('r * 't2 list, 'w, 'e) t
    val folding_map : 't list -> init:('r, 'w, 'e) t -> f:('r -> 't -> ('r * 't2, 'w, 'e) t) -> ('t2 list, 'w, 'e) t
    val filter_map  : 't list -> f:('t -> ('r option, 'w, 'e) t) -> ('r list, 'w, 'e) t
    val filter      : 't list -> f:('t -> (bool, 'w, 'e) t) -> ('t list, 'w, 'e) t
  end

  module type LIST_LIFTED = 
  sig
      type ('r, 'w, 'e) t
      val lift        : ('r list -> 'rr) -> ('r list, 'w, 'e) t -> ('rr, 'w, 'e) t
      val map         : ('r list, 'w, 'e) t -> f:('r  -> 'rr) -> ('rr list, 'w, 'e) t
      val iter        : ('r list, 'w, 'e) t -> f:('r  -> unit) -> (unit, 'w, 'e) t
      val fold        : ('r list, 'w, 'e) t -> init:'rr -> f:('rr -> 'r -> 'rr) -> ('rr, 'w, 'e) t
      val foldi       : ('r list, 'w, 'e) t -> init:'rr -> f:(int -> 'rr -> 'r -> 'rr) -> ('rr, 'w, 'e) t
      val fold_left   : ('r list, 'w, 'e) t -> init:'rr -> f:('rr -> 'r -> 'rr) -> ('rr, 'w, 'e) t
      val fold_right  : ('r list, 'w, 'e) t -> init:'rr -> f:('r  -> 'rr -> 'rr) -> ('rr, 'w, 'e) t
      val fold_map    : ('r list, 'w, 'e) t -> init:'r2 -> f:('r2 -> 'r -> 'r2 * 'r3) -> ('r2 * 'r3 list, 'w, 'e) t
      val folding_map : ('r list, 'w, 'e) t -> init:'r2 -> f:('r2 -> 'r -> 'r2 * 'r3) -> ('r3 list, 'w, 'e) t
      val filter_map  : ('r list, 'w, 'e) t -> f:('r -> 'r2 option) -> ('r2 list, 'w, 'e) t
      val filter      : ('r list, 'w, 'e) t -> f:('r -> bool) -> ('r list, 'w, 'e) t
  end


  module type LET_SYNTAX =
  sig
    type ('r, 'w, 'e) t

    val ( let* ) : ('r, 'w, 'e) t -> ('r -> ('rr, 'w, 'e) t) -> ('rr, 'w, 'e) t
    val ( and* ) : ('r1, 'w, 'e) t -> ('r2, 'w, 'e) t -> ('r1 * 'r2, 'w, 'e) t

    val ( let+ ) : ('r, 'w, 'e) t -> ('r -> 'rr) -> ('rr, 'w, 'e) t
    val ( and+ ) : ('r1, 'w, 'e) t -> ('r2, 'w, 'e) t -> ('r1 * 'r2, 'w, 'e) t
  end

  module type BASIC =
  sig
    type ('r, 'w, 'e) t
    val return : 'r -> ('r, 'w, 'e) t
    val bind   : ('r, 'w, 'e) t -> f:('r -> ('rr, 'w, 'e) t) -> ('rr, 'w, 'e) t 
    val both   : ('r1, 'w, 'e) t -> ('r2, 'w, 'e) t -> ('r1 * 'r2, 'w, 'e) t
  end

  module type PERVASIVE3 =
  sig
    type ('r, 'w, 'e) t
    type ('r, 'w, 'e) view

    val return : 'r -> ('r, 'w, 'e) t
    val bind   : ('r, 'w, 'e) t -> f:('r -> ('rr, 'w, 'e) t) -> ('rr, 'w, 'e) t 
    val run    : ('r, 'w, 'e) t -> ('r, 'w, 'e) view 
    val both    : ('r1, 'w, 'e) t -> ('r2, 'w, 'e) t -> ('r1 * 'r2, 'w, 'e) t
    val ( ** )  : ('r1, 'w, 'e) t -> ('r2, 'w, 'e) t -> ('r1 * 'r2, 'w, 'e) t
    val lift : ('r1 -> 'r2) -> ('r1, 'w, 'e) t -> ('r2, 'w, 'e) t

    include Monad.Infix3 with type ('r, 'w, 'e) t := ('r, 'w, 'e) t
    include LET_SYNTAX   with type ('r, 'w, 'e) t := ('r, 'w, 'e) t
  end

  module type PERVASIVE_POLY_WARN_ERROR =
  sig
    type 'w warn
    type 'e err
    type ('r, 'w, 'e) t
    type ('r, 'w, 'e) view =
      | Ok    of 'r * 'w warn
      | Error of 'e err * 'w warn
    val ok     : 'r -> ('r, 'w, 'e) t
    val warn   : 'r -> 'w -> ('r, 'w, 'e) t
    val warns  : 'r -> 'w warn -> ('r, 'w, 'e) t
    val err    : 'e -> ('r, 'w, 'e) t
    val errs   : 'e err -> ('r, 'w, 'e) t
    include PERVASIVE3 with type ('r, 'w, 'e) t    := ('r, 'w, 'e) t
                        and type ('r, 'w, 'e) view := ('r, 'w, 'e) view
  end

  module type PERVASIVE_POLY_WARN =
  sig
    type 'w warn
    type ('r, 'w) t
    type ('r, 'w) view = 'r * 'w warn
    val ok     : 'r -> ('r, 'w) t
    val warn   : 'r -> 'w -> ('r, 'w) t
    val warns  : 'r -> 'w warn -> ('r, 'w) t
    include PERVASIVE3 with type ('r, 'w, 'e) t    := ('r, 'w) t
                        and type ('r, 'w, 'e) view := ('r, 'w) view
  end
  
  module type PERVASIVE_POLY_ERROR =
  sig
    type 'e err
    type ('r, 'e) t
    type ('r, 'e) view = 
      | Ok    of 'r
      | Error of 'e err
    val ok     : 'r -> ('r, 'e) t
    val err    : 'e -> ('r, 'e) t
    val errs   : 'e err -> ('r, 'e) t
    include PERVASIVE3 with type ('r, 'w, 'e) t    := ('r, 'e) t
                        and type ('r, 'w, 'e) view := ('r, 'e) view
  end

  module type PERVASIVE_WARN_ERROR =
  sig
    type warn
    type err
    type 'r t
    type 'r view =
      | Ok    of 'r * warn
      | Error of err * warn
    val ok     : 'r -> 'r t
    val warn   : 'r -> warn -> 'r t
    val err    : err -> 'r t
    include PERVASIVE3 with type ('r, 'w, 'e) t    := 'r t
                        and type ('r, 'w, 'e) view := 'r view
  end

  module type PERVASIVE_WARN =
  sig
    type warn
    type 'r t
    type 'r view = 'r * warn
    val ok     : 'r -> 'r t
    val warn   : 'r -> warn -> 'r t
    include PERVASIVE3 with type ('r, 'w, 'e) t    := 'r t
                        and type ('r, 'w, 'e) view := 'r view
  end
  
  module type PERVASIVE_ERROR =
  sig
    type err
    type 'r t
    type 'r view = 
      | Ok    of 'r
      | Error of err
    val ok     : 'r -> 'r t
    val err    : err -> 'r t
    include PERVASIVE3 with type ('r, 'w, 'e) t    := 'r t
                        and type ('r, 'w, 'e) view := 'r view
  end
  
end

module Functor =
struct
  module BasicAsBasic3 
    (B : sig 
      type t 
      include Sig.BASIC with type ('r, 'w, 't) t := t 
    end) = 
    struct 
      type ('r, 'w, 't) t = B.t
      include (B : module type of B with type t := B.t)
    end

  module Basic1AsBasic3 
    (B1 : sig 
      type 'a t 
      include Sig.BASIC with type ('r, 'w, 't) t := 'r t 
    end) = 
    struct 
      type ('r, 'w, 't) t = 'r B1.t
      include (B1 : module type of B1 with type 'a t := 'a B1.t)
    end

  module Basic2AsBasic3 
    (B1 : sig 
      type ('r, 'w) t 
      include Sig.BASIC with type ('r, 'w, 't) t := ('r, 'w) t 
    end) = 
    struct 
      type ('r, 'w, 't) t = ('r, 'w) B1.t
      include (B1 : module type of B1 with type ('r, 'w) t := ('r, 'w) B1.t)
    end

  module ExtendBasic (Basic : Sig.BASIC) =
  struct
    open Basic 

    (* Include only methods of Basic but not its types *)
    include Monad.Make3 (
      struct
        include Basic
        let map = `Define_using_bind
      end
    )

    let lift f m = bind m ~f:(fun v -> return @@ f v)

    let ( ** ) = both

    let (let*) m f = bind m ~f
    let (and*) = both
    let (let+) m f = map m ~f
    let (and+) = both
  end

  module ExtendWithList (Basic : Sig.BASIC) =
  struct
    open Basic 

    module Par : Sig.PAR with type ('r, 'w, 't) t := ('r, 'w, 't) t = 
    struct
      let map xs ~f = 
        List.map xs ~f 
          |> List.fold_left ~init:(return []) ~f:( 
              fun lhs m -> 
                bind (both lhs m) ~f:(
                  fun (l, m) -> return @@ m :: l
                )
            )
          |> bind ~f:(fun l -> return @@ List.rev l)

      let iter xs ~f =
        map xs ~f |> bind ~f:(fun _ -> return ())

      let filter_map xs ~f = 
        bind (map xs ~f) ~f:(fun xs -> return @@ List.filter_opt xs)

      let filter xs ~f = 
        filter_map xs ~f:(
          fun x -> bind (f x) ~f:(
            function true  -> return @@ Some x 
                    | false -> return None
          )
        )

      let reduce xs ~init ~f = 
        let xs' = init :: List.map xs ~f:return in
        let g m1 m2 = 
          both m1 m2 |> bind ~f:(fun (x1, x2) -> f x1 x2) 
        in
        List.reduce_balanced_exn xs' ~f:g

      let reduce_exn (xs : 'r list) ~f = 
        let xs' = List.map xs ~f:return in
        let g m1 m2 : ('r, 'w, 'e) t = 
          both m1 m2 |> bind ~f:(fun (x1, x2) -> f x1 x2) 
        in
        List.reduce_balanced_exn xs' ~f:g
    end

    module Seq : Sig.SEQ with type ('r, 'w, 't) t := ('r, 'w, 't) t = 
    struct
      let fold_left xs ~init ~f = 
        List.fold_left xs ~init ~f:(
          fun accum x -> bind accum ~f:(fun accum' -> f accum' x)
        )

      let fold_right xs ~init ~f = 
        List.fold_right xs ~init ~f:(
          fun x accum -> bind accum ~f:(fun accum' -> f x accum')

        )
      let fold = fold_left

      let foldi xs ~init ~f = 
        List.foldi xs ~init ~f:(
          fun i accum x -> bind accum ~f:(fun accum' -> f i accum' x)
        )

      let iter xs ~f = 
        fold ~init:(return ()) ~f:(fun () x -> f x) xs

      let map xs ~f =
        fold_left xs ~init:(return []) ~f:(
          fun xs' x -> bind (f x) ~f:(fun x' -> return (x'::xs'))
        ) 
        |> bind ~f:(fun l -> return @@ List.rev l)

      let fold_map xs ~init ~f =
        let init = bind init ~f:(fun i -> return @@ (i, [])) in
        fold xs ~init:init ~f:(
          fun (accum, xs') x -> bind (f accum x) ~f:(
              fun (accum', x') -> return @@ (accum', x'::xs')
            )
          )
        |> bind ~f:(fun (acc, xs) -> return (acc, List.rev xs))

      let folding_map xs ~init ~f = 
        bind (fold_map xs ~init ~f) ~f:(fun (_, xs') -> return xs')

      let filter_map xs ~f = 
        fold xs ~init:(return []) ~f:(
          fun xs' x -> bind (f x) ~f:(
            function Some x' -> return @@ x'::xs'
                  | None     -> return xs'
          )
        )
        |> bind ~f:(fun l -> return @@ List.rev l)

      let filter xs ~f = 
        filter_map xs ~f:(
          fun x -> bind (f x) ~f:(
            function true  -> return @@ Some x 
                    | false -> return None
          )
        )
        
    end

    module List : Sig.LIST_LIFTED with type ('r, 'w, 't) t := ('r, 'w, 't) t = 
    struct
      let lift f rs = bind rs ~f:(fun rs -> return (f rs))
      let map rs ~f = lift (List.map ~f) rs
      let iter rs ~f = lift (List.iter ~f) rs
      let fold_left rs ~init ~f = lift (List.fold_left ~init ~f) rs
      let fold_right rs ~init ~f = lift (List.fold_right ~init ~f) rs
      let fold rs ~init ~f = lift (List.fold ~init ~f) rs
      let foldi rs ~init ~f = lift (List.foldi ~init ~f) rs
      let fold_map rs ~init ~f = lift (List.fold_map ~init ~f) rs
      let folding_map rs ~init ~f = lift (List.folding_map ~init ~f) rs
      let filter rs ~f = lift (List.filter ~f) rs
      let filter_map rs ~f = lift (List.filter_map ~f) rs
    end
  end

end

module MakePolyWarnError (W : Monoid.Sig.S1_SING) (E : Monoid.Sig.S1_SING) : 
  sig
    include Sig.PERVASIVE_POLY_WARN_ERROR with type 'w warn := 'w W.t
                                            and type 'e err  := 'e E.t
    include Monad.S3 with type ('r, 'w, 'e) t := ('r, 'w, 'e) t
    module Pervasive : Sig.PERVASIVE_POLY_WARN_ERROR with type 'w warn := 'w W.t
                                                      and type 'e err  := 'e E.t
                                                      and type ('r, 'w, 'e) t    = ('r, 'w, 'e) t
                                                      and type ('r, 'w, 'e) view = ('r, 'w, 'e) view
    module Par  : Sig.PAR with type ('r, 'w, 'e) t := ('r, 'w, 'e) t
    module Seq  : Sig.SEQ with type ('r, 'w, 'e) t := ('r, 'w, 'e) t
    module List : Sig.LIST_LIFTED with type ('r, 'w, 'e) t := ('r, 'w, 'e) t
  end =
struct
  module Basic = 
  struct
    type ('r, 'w, 'e) t = ('r, 'e E.t) result * 'w W.t

    type ('r, 'w, 'e) view = 
      | Ok    of 'r * 'w W.t
      | Error of 'e E.t * 'w W.t

    let run (v, w) = 
      match v with 
      | Result.Ok v    -> Ok (v, w)
      | Result.Error e -> Error (e, w)

    let return r        = (Result.Ok r, W.unit)

    let bind (v, w) ~f =
      match v with 
      | Result.Error e -> (Result.Error e, w)
      | Result.Ok r ->
        match f r with 
        | (Result.Ok r', w')    -> (Ok r', W.op w w')
        | (Result.Error e, w')  -> (Error e, W.op w w')

    let both (rx, wx) (ry, wy) = 
      let r' = Result.combine rx ry ~ok:Tuple2.create ~err:E.op in
      (r', W.op wx wy)
  end

  module Pervasive = 
  struct 
    include Basic
    include Functor.ExtendBasic(Basic)
    let ok r        = return r
    let warn r w    = (Result.Ok r, W.singleton w)
    let warns r w   = (Result.Ok r, w)
    let err e       = (Result.Error (E.singleton e), W.unit) 
    let errs e      = (Result.Error e, W.unit)
  end

  include Pervasive
  include Monad.Make3( struct include Basic let map = `Define_using_bind end)
  include Functor.ExtendWithList(Basic)
end

module MakePolyError (E : Monoid.Sig.S1_SING) : 
  sig
    include Sig.PERVASIVE_POLY_ERROR with type 'e err  := 'e E.t
    include Monad.S2 with type ('r, 'e) t := ('r, 'e) t
    module Pervasive : Sig.PERVASIVE_POLY_ERROR with type 'e err  := 'e E.t
                                                  and type ('r, 'e) t    = ('r, 'e) t
                                                  and type ('r, 'e) view = ('r, 'e) view
    module Par  : Sig.PAR with type ('r, 'w, 'e) t := ('r, 'e) t
    module Seq  : Sig.SEQ with type ('r, 'w, 'e) t := ('r, 'e) t
    module List : Sig.LIST_LIFTED with type ('r, 'w, 'e) t := ('r, 'e) t
  end =
struct
  module Basic = 
  struct
    type ('r, 'e) t = ('r, 'e E.t) result

    type ('r, 'e) view = 
      | Ok    of 'r
      | Error of 'e E.t

    let run v = 
      match v with 
      | Result.Ok v    -> Ok v
      | Result.Error e -> Error e

    let return r        = Result.Ok r

    let bind v ~f =
      match v with 
      | Result.Error e -> Result.Error e
      | Result.Ok r -> f r

    let both rx ry = 
      Result.combine rx ry ~ok:Tuple2.create ~err:E.op
  end

  module Arg = Functor.Basic2AsBasic3(Basic)

  module Pervasive = 
  struct 
    include Basic
    include Functor.ExtendBasic(Arg)
    let ok r        = return r
    let err e       = (Result.Error (E.singleton e)) 
    let errs e      = Result.Error e
  end

  include Pervasive
  include Monad.Make2( struct include Basic let map = `Define_using_bind end)
  include Functor.ExtendWithList(Arg)
end

module MakePolyWarn (W : Monoid.Sig.S1_SING) : 
  sig
    include Sig.PERVASIVE_POLY_WARN with type 'w warn := 'w W.t
    include Monad.S2 with type ('r, 'w) t := ('r, 'w) t
    module Pervasive : Sig.PERVASIVE_POLY_WARN with type 'w warn := 'w W.t
                                                and type ('r, 'w) t    = ('r, 'w) t
                                                and type ('r, 'w) view = ('r, 'w) view
    module Par  : Sig.PAR with type ('r, 'w, 'e) t := ('r, 'w) t
    module Seq  : Sig.SEQ with type ('r, 'w, 'e) t := ('r, 'w) t
    module List : Sig.LIST_LIFTED with type ('r, 'w, 'e) t := ('r, 'w) t
  end =
struct
  module Basic = 
  struct
    type ('r, 'w) t = 'r * 'w W.t

    type ('r, 'w) view = 'r * 'w W.t

    let run (v, w) = (v, w)

    let return r   = (r, W.unit)

    let bind (v, w) ~f = 
      let (r', w') = f v in (r', W.op w w')

    let both (rx, wx) (ry, wy) = ((rx, ry), W.op wx wy)
  end

  module Arg = Functor.Basic2AsBasic3(Basic)

  module Pervasive = 
  struct 
    include Basic
    include Functor.ExtendBasic(Arg)
    let ok r        = return r
    let warn r w    = (r, W.singleton w)
    let warns r w   = (r, w)
  end

  include Pervasive
  include Monad.Make2( struct include Basic let map = `Define_using_bind end)
  include Functor.ExtendWithList(Arg)
end

module MakeWarnError (W : Monoid.Sig.S) (E : Monoid.Sig.S) :
  sig
    include Sig.PERVASIVE_WARN_ERROR with type warn := W.t
                                      and type err  := E.t
    include Monad.S with type 'r t := 'r t
    module Pervasive : Sig.PERVASIVE_WARN_ERROR with type warn := W.t
                                                  and type err  := E.t
                                                  and type 'r t = 'r t
                                                  and type 'r view = 'r view
    module Par : Sig.PAR with type ('r, 'w, 'e) t := 'r t
    module Seq : Sig.SEQ with type ('r, 'w, 'e) t := 'r t
    module List : Sig.LIST_LIFTED with type ('r, 'w, 'e) t := 'r t
  end = 
struct

  module Basic = 
  struct
    type 'r t = ('r, E.t) result * W.t

    type 'r view = 
      | Ok    of 'r * W.t
      | Error of E.t * W.t

    let run (v, w) = 
      match v with 
      | Result.Ok v    -> Ok (v, w)
      | Result.Error e -> Error (e, w)

    let return r        = (Result.Ok r, W.unit)

    let bind (v, w) ~f =
      match v with 
      | Result.Error e -> (Result.Error e, w)
      | Result.Ok r ->
        match f r with 
        | (Result.Ok r', w')    -> (Ok r', W.op w w')
        | (Result.Error e, w')  -> (Error e, W.op w w')

    let both (rx, wx) (ry, wy) = 
      let r' = Result.combine rx ry ~ok:Tuple2.create ~err:E.op in
      (r', W.op wx wy)
  end

  module Arg = Functor.Basic1AsBasic3(Basic)

  module Pervasive = 
  struct 
    include Basic 
    include Functor.ExtendBasic(Arg)
    let ok r        = return r
    let warn r w    = (Result.Ok r, w)
    let err e       = (Result.Error e, W.unit) 
  end

  include Pervasive
  include Monad.Make(struct include Basic let map = `Define_using_bind end)
  include Functor.ExtendWithList(Arg)
end

open! struct
  module ListAsMonoid = 
  struct
    type 'a t = 'a list
    let unit = []
    let singleton a = [a]
    let op = List.append
  end
end

module WarnListErrorList = MakePolyWarnError (ListAsMonoid) (ListAsMonoid)
module WarnList          = MakePolyWarn  (ListAsMonoid) 
module ErrorList         = MakePolyError (ListAsMonoid) 