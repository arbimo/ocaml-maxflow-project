open Gfile
open Tools
open Fulkerson
    
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
  let graph = from_file infile in

  let flow_graph_1 = create_flow_graph graph in

  let path : int list = [0; 2; 4; 5] in

  let flow_graph_2 = update_flow_graph flow_graph_1 (fun arc -> check_if_arc_is_in_path arc path) 1 in

  let string_flow_graph = gmap flow_graph_2 (fun x -> string_of_int x) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile string_flow_graph in


  
  (* let g = clone_nodes graph in *)
  let () = aff (dfs graph 0 [] 3) ; 
  Printf.printf "\n\n" ;
  hash 
  in () 

  


