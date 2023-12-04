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
          (* Perform some transformation or condition on the arc and accumulate the result in the new graph *)
          let modified_arc =
            if condition arc then
              { arc with lbl = arc.lbl + increment }
            else
              arc
          in
          new_arc acc_graph modified_arc)
        empty_graph
    in
    new_graph
  

    