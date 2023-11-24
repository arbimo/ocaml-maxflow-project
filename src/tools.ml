open Graph

let clone_nodes gr= n_fold gr new_node empty_graph
let gmap gr f = 
  e_fold gr (fun g arc ->  (new_arc g {src=arc.src; tgt=arc.tgt; lbl=f arc.lbl})) empty_graph

let add_arc _a _b _c _d = assert false

