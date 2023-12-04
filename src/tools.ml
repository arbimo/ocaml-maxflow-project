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
  
let update_flow_graph : int graph -> (int arc -> bool) -> (int arc -> bool) -> int -> int graph =
  fun original_graph condition backward_condition increment ->
    let new_graph =
      e_fold
        original_graph
        (fun acc_graph arc ->
          let modified_arc =
            if condition arc then
              { arc with lbl = arc.lbl + increment }
            else if backward_condition arc then
              { arc with lbl = arc.lbl - increment }
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
        if (node1, node2) = (arc.src, arc.tgt) then
          true
        else
          is_arc_in_path (node2 :: rest)
    in
    is_arc_in_path path

let check_if_backward_arc_is_in_path : int arc -> int list -> bool =
  fun arc path ->
    let rec is_arc_in_path path =
      match path with
      | [] | [_] -> false (* A path must have at least two nodes to have an arc *)
      | node1 :: node2 :: rest ->
        if (node2, node1) = (arc.src, arc.tgt) then
          true
        else
          is_arc_in_path (node2 :: rest)
    in
    is_arc_in_path path

let find_max_flow_on_path : int graph -> int graph -> int list -> int option =
  fun graph flow_graph path ->
    let rec find_max_flow_on_path path =
      match path with
      | [] | [_] -> None (* A path must have at least two nodes to have arcs *)
      | node1 :: node2 :: rest ->
        let arc1_opt = find_arc graph node1 node2 in
        let arc2_opt = find_arc flow_graph node1 node2 in
        let arc1_rev_opt = find_arc graph node2 node1 in
        let arc2_rev_opt = find_arc flow_graph node2 node1 in
        match (arc1_opt, arc2_opt, arc1_rev_opt, arc2_rev_opt) with
        | (Some arc1, Some arc2, None, None) ->
          let rest_difference_opt = find_max_flow_on_path (node2 :: rest) in
          begin
            match rest_difference_opt with
            | Some rest_difference -> Some (min (arc1.lbl - arc2.lbl) rest_difference)
            | None -> Some (arc1.lbl - arc2.lbl)
          end
        | (None, None, Some _, Some arc2_rev) ->
          let rest_difference_opt = find_max_flow_on_path (node2 :: rest) in
          begin
            match rest_difference_opt with
            | Some rest_difference -> Some (min (arc2_rev.lbl) rest_difference)
            | None -> Some (arc2_rev.lbl)
          end
        | _ -> None
    in
    find_max_flow_on_path path

let rec while_loop condition action x =
  if condition x then begin
    action x;  (* Execute the action *)
    while_loop condition action x  (* Recursive call for the next iteration *)
  end else
    ()



  

    