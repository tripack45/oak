module Vertex =
struct
  include Core.Int
  let vmin = 0
  let p = ref 1
  let fresh () : t = let t = !p in p := t + 1; t
end

module Directed =
struct
  open Core

  type ('v, 'wit) adj = ('v, 'wit) Set.t
  type 'v edge = 'v * 'v

  exception VertexNotExist

  type ('v, 'a, 'wit) t = 
  {
    v   : ('v, 'a, 'wit) Map.t;
    adj : ('v, ('v, 'wit) Set.t, 'wit) Map.t;
  }

  module V =
  struct
    let attr {v; _} vert       = Map.find v vert 
    let attr_exn {v; _} vert   = Map.find_exn v vert
    let adj {adj; _} vert      = Map.find adj vert
    let adj_exn {adj; _} vert  = Map.find_exn adj vert

    let sort_by_attr g ~compare verts =
      List.sort verts ~compare:(
        fun vx vy -> compare (attr_exn g vx) (attr_exn g vy)
      )
  end

  let of_adj_list l = failwith "Unimplemented"

  let of_adj_set (type v) (map: (v, 'attr * (v, 'wit) Set.t, 'wit) Map.t) = 
    let exception BadEdge of v * v in
    let v   = Map.map map ~f:fst in
    let adj = Map.map map ~f:snd in
    try 
      let vs = Map.key_set map in
      Map.iteri adj ~f:(
        fun ~key:v ~data:adjs -> 
          let v_frees = Set.diff adjs vs in
          match Set.choose v_frees with 
          | Some v' -> raise (BadEdge (v, v'))
          | None -> ()
      );
      `Ok {v; adj}
    with
    BadEdge (v, v') -> `EdgeToInvalidVertex (v, v')
    

  let of_adj_set_exn map = 
    match of_adj_set map with 
    | `Ok r -> r
    | _ -> raise VertexNotExist

  let of_edge_list l = failwith "Unimplemented"

  let comparator g = (Map.comparator g.adj).compare

  module WithVertex =
  struct
    type ('v, 'attr, 'wit) vt = ('v, 'attr, 'wit) t
    type 'a t = (Vertex.t, 'a, Vertex.comparator_witness) vt

    let comparator   = Vertex.comparator
  end

  module TopologicalSort =
  struct
    exception GraphIsCyclic
    (* Topological sorting emits nodes according to their toplogical order. In the face of
     * multiple possible choice, it favors the node with "less" attr. *)
     
    (* Topologically sort the verticies. Break ties using order of verticies.*)
    let topsort (g : ('v, 'a, 'wit) t) ~compare_opt : 'v Core.Sequence.t = 
      let f adj =
        if Map.is_empty adj then 
          None
        else
          let v_sink_opt = 
            let sink_set = Map.filter adj ~f:(fun adjs -> Set.is_empty adjs) |> Map.key_set in
            match compare_opt with 
            | None         -> Set.min_elt sink_set
            | Some compare -> 
              sink_set |> Set.to_list |> List.min_elt ~compare:(
                fun vx vy -> compare (V.attr_exn g vx) (V.attr_exn g vy)
              )
          in
          match v_sink_opt with 
          | None -> raise GraphIsCyclic
          | Some v_sink ->
          let adj' = 
            Map.filter_mapi adj ~f:(
              fun ~key:v ~data:adjs ->
                if (comparator g) v_sink v = 0 then
                  (* Remove the sink node from graph *)
                  None
                else
                  (* For other verticies remove the sink node if adjacent *)
                  Some (Set.remove adjs v_sink)
            )
          in
          Some (v_sink, adj')
      in 
      Sequence.unfold ~init:g.adj ~f

    (* Topologically sort the verticies, break ties by comparing attributes associated with verts.*)
    let topsort_attr g ~compare = topsort g ~compare_opt:(Some compare)

    let topsort g = topsort g ~compare_opt:None
  end

  module StronglyConnectedComponents =
  struct
    type ('v, 'wit) component = ('v, 'wit) Core.Set.t 

    let scc g : ('v, 'vwit) component WithVertex.t = 
      let singleton v = Set.singleton (Map.comparator_s g.v) v in
      (* This is a stub implementation that essentially says none of vertex 
       * is related to any other vertex. Therefore toplogical sorting would have 
       * placed all vertecies in the original order they came in. *)
      g.v
      |> Map.keys
      |> List.map ~f:(fun v -> (Vertex.fresh (), (singleton v, Vertex.Set.empty)))
      |> Vertex.Map.of_alist_exn
      |> of_adj_set_exn

    let scc_topsort (g : ('v, 'a, 'vwit) t) : 'v list Sequence.t =
      let scc_g : ('v, 'vwit) Set.t WithVertex.t = scc g in
      scc_g
      |> TopologicalSort.topsort_attr ~compare:(
          fun vset_x vset_y -> 
            (comparator g) (Set.min_elt_exn vset_x) (Set.min_elt_exn vset_y)
        )
      |> Sequence.map ~f:(
          fun scc_v -> 
            V.attr_exn scc_g scc_v |> Set.to_list
        )

    let scc_topsort_attr (g : ('v, 'a, 'vwit) t) ~compare : 'v list Sequence.t =
      failwith "Unimplemented"
  end
end
