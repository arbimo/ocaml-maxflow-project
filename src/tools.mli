open Graph

val clone_nodes: 'a graph -> 'b graph

val gmap: 'a graph -> ('a -> 'b) -> 'b graph

val add_arc: int graph -> id -> id -> int -> int graph

(* try not to lose genericity *)
val create_flow_graph: 'a graph -> int graph

val update_flow_graph : int graph -> (int arc -> bool) -> int -> int graph

val check_if_arc_is_in_path : int arc -> int list -> bool

val find_smallest_label_on_path : int graph -> int list -> int option



