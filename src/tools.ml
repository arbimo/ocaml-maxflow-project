(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold new_fold empty_graph gr
    (*For every tuples in the list*)
let gmap _gr _f = assert false
(* Replace _gr and _f by gr and f when you start writing the real function. *)