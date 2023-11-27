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

(*let update_arc_flow graph src_id dest_id flow = 
  match (find_arc graph src_id dest_id) with
  | Some arc -> arc.lbl <- arc.lbl + flow
  | None -> ()*)

let update_arc_flow graph src_id dest_id flow = gmap graph (fun arc -> if arc.src = src_id && arc.tgt = dest_id then (arc.lbl + flow))

(*let update_flow_graph graph node_ids new_flow = 
  let mylength_acu_better l =
    let rec loop acu l = match l with
      | Empty -> acu
      | Cell(_, tail) -> loop (acu + 1) tail
    in
    loop 0 l ;;*)

(*let find_max_possible_flow graph node_ids = e_fold graph (fun acu arc -> if List.mem arc.src myList) 0*)