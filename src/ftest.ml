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
  and source = int_of_string Sys.argv.(2)
  and target = int_of_string Sys.argv.(3)
  in

  Printf.printf "Source: %d\n" source ;
  Printf.printf "Target: %d\n" target ;

  (* Open file *)
  let string_graph = from_file infile in
  let graph = gmap string_graph (fun x -> int_of_string x) in

  let flow_graph = create_flow_graph graph in

  (* iteration 1 *)

  let flow_graph = iterate graph flow_graph source target in

  (* iteration 2 *)

  let flow_graph = iterate graph flow_graph source target in

  (* iteration 3 *)

  let flow_graph = iterate graph flow_graph source target in

  (* save result to file *)

  let string_flow_graph = gmap flow_graph (fun x -> string_of_int x) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile string_flow_graph in

  ()


  
  (*(* let g = clone_nodes graph in *)
  let () = aff (dfs graph 0 [] 3) ; 
  Printf.printf "\n\n" ;
  hash 
  in () *)