(* Martin's algorithm to calculate multiobjective shortest path *)

open Printf
open Graph

let _ = Random.self_init ()

let nb_dimension = 2
(* We define the graph we want to use *)
(* The vertices have a list of array as labels *)
module VertexLabel = struct 
  type t = int * int
end

module EdgeLabel = struct
    type t = float array
    let compare = compare
    let default = Array.make nb_dimension 0.
end


module G = Imperative.Digraph.AbstractLabeled(VertexLabel)(EdgeLabel)
(* The graph is a grid *)
let x = 10
let y = 10

let rand_weight () = Array.init nb_dimension (fun _ -> Random.float 10.0)

let graph = G.create ~size:(x * y) ()
let vert =
    let new_vertex i j =
        let v = G.V.create (i,j) in
        G.add_vertex graph v; v in
    Array.init x (fun i -> Array.init y (new_vertex i)) 

let gen_vertex = 
    let add_edge u v = 
        let e = G.E.create u (rand_weight ()) v in
        G.add_edge_e graph e in

    for i = 0 to x - 1 do
        for j = 0 to y - 1 do
            if i > 0 then add_edge vert.(i).(j) vert.(i-1).(j);
            if j > 0 then add_edge vert.(i).(j) vert.(i).(j-1);
            if i < x -1 then add_edge vert.(i).(j) vert.(i+1).(j);
            if j < y -1 then add_edge vert.(i).(j) vert.(i).(j+1);
        done;
    done

module Elt = struct
    type t = G.V.t * float array
    let compare (_,w1) (_,w2) = compare w2 w1 
  end

module PQ = Heap.Imperative(Elt)
module H =  Hashtbl.Make(G.V)

let v_add v1 v2 = Array.mapi (fun i e -> v2.(i) +. e) v1

let rec domination_filter label = function
    | [] -> []
    | h::t ->
            if (h < label) then h::(domination_filter label t)
            else domination_filter label t

let p_label l = 
    printf "[";
    Array.iter (fun e -> printf "%f, " e) l;
    printf "]"

let p_all ll =
    printf "{";
    List.iter (fun e -> p_label e) ll;
    printf "}\n"

let (<~) v1 v2 =
    let l1, l2 = (Array.to_list v1), (Array.to_list v2) in
    (List.for_all2 (fun a b -> a <= b) l1 l2)
    && (List.exists2 (fun a b -> a < b) l1 l2)
    
let is_dominated label ll =
    List.exists (fun e -> e <~ label) ll

let martins start =
    let dist = H.create (G.nb_vertex graph) in
    let q = PQ.create (G.nb_vertex graph) in
    let rec loop () =
        if PQ.is_empty q then dist
        else
            let (u, d) = PQ.pop_maximum q in
                let x,y = (G.V.label u) in
                printf "Entering %d %d: " x y;
                p_label d;
                G.iter_succ_e
                (fun e -> let ev = G.E.dst e in
                    let succ_w = v_add d (G.E.label e) in
                    let succ_labels = try H.find dist ev with Not_found -> [] in
                    let succ_filtered = domination_filter succ_w succ_labels in
                    let improvement = not (is_dominated succ_w succ_labels) in
                    if improvement then begin
                        H.replace dist ev (succ_w::succ_filtered);
                        PQ.add q (ev, succ_w)
                    end
                )
                graph u;
                printf "\n";
            loop () in
    PQ.add q (start, Array.make nb_dimension 0.);
    H.add dist start [Array.make nb_dimension 0.];
    loop ()

let res = martins vert.(0).(0)

let nb_paths = H.fold (fun k v r -> (List.length v) + r) res 0

    
let _ = printf "coucou\n";
printf "Graph size: nodes=%d, edges=%d, paths=%d\n"
(G.nb_vertex graph) (G.nb_edges graph) nb_paths;
