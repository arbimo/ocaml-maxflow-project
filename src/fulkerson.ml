open Graph
open Tools

let test graph =
  let arcs = out_arcs graph 1 in
  match arcs with
  | [] -> clone_nodes graph
  | x::_rest -> new_arc (clone_nodes graph) x 


let successors n graph = 
  let successors = out_arcs graph n in
  List.map (fun arc -> arc.tgt) successors


let rec find_path_aux graph idList src tgt =
  if src = tgt then
    [src]
  else
    let rec loop neighbors idList =
      match neighbors with
      | [] -> [] (*pas de chemin*)
      | n1::rest ->
        let path = (find_path_aux graph (n1::idList) n1 tgt) in
        match path with
        | [] -> loop rest (n1::idList)
        | _ -> 
          if List.mem n1 path then
            path 
          else n1::path in
       loop (successors src graph) idList 

let find_path graph src tgt =
  src::(find_path_aux graph [] src tgt)

let rec aff nodes =
  match nodes with
  | [] -> ()
  | x::rest -> Printf.printf "%d " x ; aff rest 

let iterate_ford_fulkerson graph flow_graph src tgt =
  let path : int list = find_path graph src tgt in
  Printf.printf "Path: [%s]\n" (String.concat "; " (List.map string_of_int path)) ;

  let new_flow_optional : int option = find_max_flow_on_path graph flow_graph path in
  let new_flow : int = Option.value new_flow_optional ~default:0 in
  Printf.printf "Flow: %d\n" new_flow ;

  update_flow_graph flow_graph (fun arc -> check_if_arc_is_in_path arc path) (fun arc -> check_if_backward_arc_is_in_path arc path) new_flow



