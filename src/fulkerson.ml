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

   