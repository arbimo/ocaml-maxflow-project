open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = 
  e_fold gr (fun g arc ->  (new_arc g {src=arc.src; tgt=arc.tgt; lbl=f arc.lbl})) (clone_nodes gr)

let add_arc graph id1 id2 n = 
  let arc = find_arc graph id1 id2 in
  match arc with
  | None -> new_arc graph {src=id1; tgt=id2; lbl=n}
  | Some arc -> new_arc graph {src=arc.src; tgt=arc.tgt; lbl=arc.lbl+n}

let create_flow_graph graph = gmap graph (fun _ -> 0)
  
let update_flow_graph : int graph -> (int arc -> bool) -> int -> int graph =
  fun original_graph condition increment ->
    let new_graph =
      e_fold
        original_graph
        (fun acc_graph arc ->
          let modified_arc =
            if condition arc then
              { arc with lbl = arc.lbl + increment }
            else
              arc
          in
          new_arc acc_graph modified_arc)
        original_graph
    in
    new_graph

let check_if_arc_is_in_path : int arc -> int list -> bool =
  fun arc path ->
    let rec is_arc_in_path path =
      match path with
      | [] | [_] -> false (* A path must have at least two nodes to have an arc *)
      | node1 :: node2 :: rest ->
        if (node1, node2) = (arc.src, arc.tgt) || (node1, node2) = (arc.tgt, arc.src) then
          true
        else
          is_arc_in_path (node2 :: rest)
    in
    is_arc_in_path path

let find_smallest_label_on_path : int graph -> int list -> int option =
  fun graph path ->
    let rec find_smallest_label path =
      match path with
      | [] | [_] -> None (* A path must have at least two nodes to have arcs *)
      | node1 :: node2 :: rest ->
        let arc_opt = find_arc graph node1 node2 in
        match arc_opt with
        | Some arc ->
          let rest_label_opt = find_smallest_label (node2 :: rest) in
          begin
            match rest_label_opt with
            | Some rest_label -> Some (min arc.lbl rest_label)
            | None -> Some arc.lbl
          end
        | None -> None
    in
    find_smallest_label path

  

    