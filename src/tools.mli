open Graph

val clone_nodes: 'a graph -> 'b graph

val gmap: 'a graph -> ('a -> 'b) -> 'b graph

val add_arc: int graph -> id -> id -> int -> int graph

(* try not to lose genericity *)
val create_flow_graph: 'a graph -> int graph

val update_flow_graph : int graph -> (int arc -> bool) -> int -> int graph

