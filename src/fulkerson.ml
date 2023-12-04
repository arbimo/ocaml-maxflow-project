open Graph
open Tools

let test graph =
  let arcs = out_arcs graph 1 in
  match arcs with
  | [] -> clone_nodes graph
  | x::_rest -> new_arc (clone_nodes graph) x 


(* let rec find_path graph s target =
  let arcs = out_arcs graph s in
  match arcs with
  | [] -> [] 
  | arc1::_restArcs -> 
    try
      let dernier_arc=List.find (fun arc -> arc.tgt=target) arcs in
      [dernier_arc]
    with Not_found -> 
      arc1::(find_path graph arc1.tgt target) *)


let successors n graph = 
  let successors = out_arcs graph n in
  List.map (fun arc -> arc.tgt) successors

(* let rec explore graph s =
  let rec loop voisins visited= 
    match voisins with
    | [] -> visited
    | x::rest -> 
      if not (List.mem x visited) then
        explore graph x 
      else visited in
    loop graph [] *)

let memory = Hashtbl.create 20 

let rec dfs graph node visited dest=
  if List.mem node visited || List.mem dest visited then 
    begin
    Hashtbl.add memory node node ; visited
    end 
  else
    let rec explore_neighbors visited_aux neighbors =
      match neighbors with 
      | [] -> visited_aux
      | neighbor::rest -> 
        let a = dfs graph neighbor visited_aux dest in
        explore_neighbors a rest in
    explore_neighbors (node::visited) (successors node graph)  

let find_path graph src dest =
  (Hashtbl.clear memory) ;
  dfs graph src [] dest


let hash = Hashtbl.add memory 2 2; 
    Hashtbl.iter (fun x y -> Printf.printf "%d -> %d\n" x y) memory;;

(* let dfs graph start dest =
  let rec rdfs visited node =
    if not (List.mem node visited) && not(List.mem dest visited) then
      begin
        (* f node; *)
        let s = successors node graph in
        (* if List.mem dest s then
          dest::visited
        else *)
        List.fold_left rdfs (node::visited) s
      end
    else visited
  in rdfs [] start  *)

let rec aff nodes =
  match nodes with
  | [] -> ()
  | x::rest -> Printf.printf "%d " x ; aff rest 

let rec create nodes = 
  match nodes with
  | [] -> empty_graph
  | x::rest -> new_node (create rest) x
   