(* On définit quelques structures utiles *)
type city = { x: float; y: float}
type ant = { solution: int list; fitness: float }
type param_t = {
    alpha: float;
    beta: float;
    rho: float;
    nb_ants: int;
    min_pheromones: float;
    max_pheromones: float;
}
type problem = {
    eval: int list -> float;
    att: int -> int -> float;
    candidates: int list;
    size: int;
}

(* Définition des params, Naren si tu nous lis... *)
let params = { alpha = 1.; beta = 0.5 ; rho = 0.01; nb_ants = 20;
min_pheromones= 0.1; max_pheromones = 100.}

(* Définition du problème *)
let cost a b = sqrt((a.x  -. b.x) ** 2. +. (a.y -. b.y) ** 2.) 

(* Parse le fichier contenant les villes (MOCHE!) *)
let cities =
    let f = open_in "/home/tristram/git/code/ants/berlin52.tsp" in
    let l = ref [] in
    try
        while true do
            l := (Scanf.sscanf  (input_line f) "%d %f %f" 
            (fun b c d -> { x=c; y=d })
            )::!l
        done;
        Array.of_list !l
    with End_of_file -> print_endline "finnished";
    Array.of_list !l

let tsp_candidates = 
    let rec city_list = function
        | 0 -> []
        | n -> (n-1)::(city_list (n - 1))
    in city_list (Array.length cities)


let att_mtrx = Array.map(
    fun e -> Array.map (fun e2 -> (1. /. ((cost e e2) +. 1.)) ** params.beta
    ) cities )cities

let tsp_eval solution =
    let rec length = function
        | [] -> 0.
        | [a] -> cost cities.(a) cities.(List.hd solution)
        | a::b::t -> (cost cities.(a) cities.(b)) +. (length (b::t))
    in length solution

let tsp_att a b = att_mtrx.(a).(b)

let tsp_size = Array.length cities

let p = {eval=tsp_eval; att=tsp_att;
candidates=tsp_candidates; size=tsp_size}

(* Partie à l'algo à proprement parler *)
let pheromones = Array.make_matrix p.size p.size 1.

let select_candidate n li =
    let sum = List.fold_right 
        (fun e r -> (pheromones.(n).(e) ** params.alpha) *. p.att n e   +.r )
        p.candidates 0. in
    let rec choose rest = function
        | [] -> failwith "Empty list"
        | [a] -> (a,[])
        | h::t -> let rest2 = rest -. ((pheromones.(n).(h) ** params.alpha) *.
        p.att n h)  in 
            if rest2 <= 0. then (h,t)
            else let (h2,t2) = choose rest2 t in (h2, h::t2)
    in choose (Random.float sum) li


let rec gen_solution start = function
    | [] -> []
    | [a] -> [a]
    | t -> let (h2,t2) = (select_candidate start t) in 
    h2::(gen_solution h2 t2) 

    (* Doit pouvoir se remplacer par un iter *)
let decrease_pheromones () = 
   for i = 0 to p.size - 1 do
      for j = 0 to p.size - 1 do
         pheromones.(i).(j) <- max params.min_pheromones pheromones.(i).(j) *. (1.-.params.rho)
      done
   done

let rec update_pheromones amount = function
    | [] -> ()
    | [a] -> ()
    | a::b::t -> pheromones.(a).(b) <- min params.max_pheromones (pheromones.(a).(b)+.amount);
    update_pheromones amount (b::t)

let rec all_solutions nb =
    if nb = 0 then [] else
        let sol = gen_solution 0 p.candidates in
        ({solution=sol; fitness=(p.eval sol)})::(all_solutions (nb -
        1))

let best_ever = ref infinity
let best_sol = ref []

(* Y'a du boulot.... devrait prender params et p en paramètre *)
let bar = 
    let oc = open_out "log" in
    for i = 0 to 5000 do
        let all = List.sort (fun s1 s2 -> compare  s1.fitness s2.fitness)
            (all_solutions params.nb_ants) in
        let best = List.hd all in 
        let a::b::c::d::_ = all in
        decrease_pheromones();
        update_pheromones 0.5 a.solution;
        update_pheromones 0.3 b.solution;
        update_pheromones 0.2 c.solution;
        update_pheromones 0.1 d.solution;
        print_endline ("Solution found: " ^ (string_of_float a.fitness));
        Printf.fprintf oc "%d %f\n" i a.fitness;
        if a.fitness < !best_ever then
            begin
                best_ever := a.fitness;
                best_sol := best.solution;
            end;

            update_pheromones 0.5 !best_sol;

    done;
    Printf.printf "Meilleure solution trouvée : %f" !best_ever


