(* This module defines an abstract notions of "vertex". This is useful when the user is looking to 
 * introduce a graph but does not have a particular notion of vertex type in mind. 
 * 
 * Fresh verticies are generated upon calling fresh (). Verticies are ordered according their order
 * of generation (later nodes are conceptually lareger). vmin denotes a vertex that is less than 
 * any Vertex.t other than itself. *)
module Vertex :
sig
  type t
  val vmin : t
  val fresh : unit -> t
  val to_string : t -> string

  include Core.Comparable.S with type t := t
  module Set : Core.Set.S with type Elt.t = t 
                           and type Elt.comparator_witness = comparator_witness
  module Map : Core.Map.S with type Key.t = t 
                           and type Key.comparator_witness = comparator_witness

end

module Directed :
sig
  type ('v, 'a, 'wit) t

  type ('v, 'wit) adj = ('v, 'wit) Core.Set.t

  type 'v edge = 'v * 'v

  exception VertexNotExist

  val of_adj_list    : ('v, 'a * ('v list), 'wit) Core.Map.t -> ('v, 'a, 'wit) t
  val of_adj_set_exn : ('v, 'a * ('v, 'wit) adj, 'wit) Core.Map.t -> ('v, 'a, 'wit) t
  val of_adj_set     : ('v, 'a * ('v, 'wit) adj, 'wit) Core.Map.t -> [`Ok of ('v, 'a, 'wit) t | `EdgeToInvalidVertex of 'v * 'v]
  val of_edge_list   : ('v, 'a, 'wit) Core.Map.t -> ('v edge) list  -> ('v, 'a, 'wit) t
  val to_adj_list    : ('v, 'a, 'wit) t -> ('v * ('v list)) list

  module V :
  sig
    val attr     : ('v, 'a, 'wit) t -> 'v -> 'a option
    val attr_exn : ('v, 'a, 'wit) t -> 'v -> 'a 
    val adj      : ('v, 'a, 'wit) t -> 'v -> ('v, 'wit) adj option
    val adj_exn  : ('v, 'a, 'wit) t -> 'v -> ('v, 'wit) adj
    val sort_by_attr : ('v, 'a, 'wit) t -> compare:('a -> 'a -> int) -> 'v list -> 'v list
  end

  module WithVertex :
  sig
    type ('v, 'a, 'wit) g := ('v, 'a, 'wit) t
    type 'a t = (Vertex.t, 'a, Vertex.comparator_witness) g
  end

  module TopologicalSort :
  sig
    exception GraphIsCyclic
    (* Topological sorting emits nodes according to their toplogical order. In the face of
      * multiple possible choice, it favors the node with "less" attr. *)
    val topsort_attr : ('v, 'a, 'wit) t -> compare:('a -> 'a -> int) -> 'v Core.Sequence.t
    val topsort      : ('v, 'a, 'wit) t -> 'v Core.Sequence.t
  end

  module StronglyConnectedComponents :
  sig
    type ('v, 'wit) component = ('v, 'wit) Core.Set.t 

    val scc              : ('v, 'a, 'wit) t -> ('v, 'wit) component WithVertex.t
    (* This function linearizes ssc resulting according to vertex order *)
    val scc_topsort      : ('v, 'a, 'wit) t -> ('v list) Core.Sequence.t
    (* This function linearizes scc result according to attr *)
    val scc_topsort_attr : ('v, 'a, 'wit) t -> compare:('a -> 'a -> int) -> ('v list) Core.Sequence.t
end
end
