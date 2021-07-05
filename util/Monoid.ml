module Sig = 
struct
  module type S = 
  sig
    type t
    val unit : t
    val op : t -> t -> t
  end

  module type S1 = 
  sig
    type 'a t
    val unit : 'a t
    val op : 'a t -> 'a t -> 'a t
  end

  module type S1_SING = 
  sig
    include S1
    val singleton : 'a -> 'a t
  end 
end