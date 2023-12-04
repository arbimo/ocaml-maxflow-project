open Gfile
open Tools
(*open Fulkerson*)
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let string_graph = from_file infile in
  let graph = gmap string_graph (fun x -> int_of_string x) in

  let flow_graph_0 = create_flow_graph graph in

  (* iteration 1 *)

  let path_1 : int list = [0; 1; 4; 5] in
  Printf.printf "Path 1: [%s]\n" (String.concat "; " (List.map string_of_int path_1)) ;

  let new_flow_optional_1 : int option = find_max_flow_on_path graph flow_graph_0 path_1 in
  let new_flow_1 : int = Option.value new_flow_optional_1 ~default:0 in
  Printf.printf "Flow 1: %d\n" new_flow_1 ;

  let flow_graph_1 = update_flow_graph flow_graph_0 (fun arc -> check_if_arc_is_in_path arc path_1) new_flow_1 in

  (* iteration 2 *)

  let path_2 : int list = [0; 3; 4; 5] in
  Printf.printf "Path 2: [%s]\n" (String.concat "; " (List.map string_of_int path_2)) ;

  let new_flow_optional_2 : int option = find_max_flow_on_path graph flow_graph_1 path_2 in
  let new_flow_2 : int = Option.value new_flow_optional_2 ~default:0 in
  Printf.printf "Flow 2: %d\n" new_flow_2 ;

  let flow_graph_2 = update_flow_graph flow_graph_1 (fun arc -> check_if_arc_is_in_path arc path_2) new_flow_2 in

  (* iteration 3 *)

  let path_3 : int list = [0; 3; 4; 1; 2; 5] in
  Printf.printf "Path 3: [%s]\n" (String.concat "; " (List.map string_of_int path_3)) ;

  let new_flow_optional_3 : int option = find_max_flow_on_path graph flow_graph_2 path_3 in
  let new_flow_3 : int = Option.value new_flow_optional_3 ~default:0 in
  Printf.printf "Flow 3: %d\n" new_flow_3 ;

  let flow_graph_3 = update_flow_graph flow_graph_2 (fun arc -> check_if_arc_is_in_path arc path_3) new_flow_3 in

  (* save result to file *)

  let string_flow_graph = gmap flow_graph_3 (fun x -> string_of_int x) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile string_flow_graph in

  ()


  
  (*(* let g = clone_nodes graph in *)
  let () = aff (dfs graph 0 [] 3) ; 
  Printf.printf "\n\n" ;
  hash 
  in () *)

  


