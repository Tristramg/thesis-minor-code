(* Particle swarm optimizaiont *)

type opt_direction = Minimization | Maximization

type parameters_t = { dimensions : (float * float) array;
		    direction : opt_direction;
		    objective :  float array -> float }

let rec alpine vect =
  Array.fold_right (fun elt rest -> sin(elt) *. sqrt(elt) *. rest) vect  1.0 

let parameters = { dimensions = [|(0.,10.); (0.,10.); (0.,10.); (0.,10.); (0.,10.); (0.,10.)|] ;
		   direction = Maximization;
		   objective = alpine }

let print_vect v = Array.iter (fun x -> Printf.printf "%.3f; " x) v
let (<=>) = if parameters.direction = Minimization then (<) else (>)
let inf = if parameters.direction = Minimization then max_float else min_float

let copy_from_to arr1 arr2 = Array.blit arr1 0 arr2 0 (Array.length arr1)


type particle = { position : float array ;
		  speed : float array ;
  		  pbest : float array ;
		  mutable pfitness : float ;
		  mutable neighbors : particle list ;
		  gbest : float array  ;
		  mutable gfitness : float }

let cinetic_nrj particle = Array.fold_right (fun x r -> x**2. +. r) particle.speed 0.
let total_energy swarm = List.fold_right (fun p r -> cinetic_nrj p +. r) swarm 0.

let gen_position min max = (Random.float (max -. min)) +. min
let gen_speed min max =
  (if Random.bool() then (-.1.0) else (1.)) *. (Random.float (max -. min) /. 2.)

exception Empty
let rec best list = match list with
  | h::[] -> h;
  | h::t -> let tbest = best t in
      if h.pfitness <=> tbest.pfitness then h else tbest
  | _ -> raise Empty

let gen_particle () =
  let dim = Array.length parameters.dimensions in
  let particle = { position = Array.make dim 0. ;
		   speed = Array.make dim 0. ; 
		   pbest = Array.make dim 0. ;
		   pfitness = inf;
		   neighbors = [] ;
		   gbest = Array.make dim 0. ;
		   gfitness = inf } in
    Array.iteri
      ( fun i (min,max) ->
	  particle.position.(i) <- gen_position min max ;
	  particle.pbest.(i) <- particle.position.(i);
	  particle.speed.(i) <- gen_speed min max ;
	  particle.pfitness <- parameters.objective particle.position
      ) parameters.dimensions ;
    particle

let rec gen_swarm = function
  | 0 -> []
  | n -> gen_particle () :: gen_swarm (n-1)

let set_neighbors neighbors particle = particle.neighbors <- neighbors 

(* NOTE : a particle is also it's own neighbor... not satisfying *)
let full_neighborhood particles = 
  List.iter (set_neighbors particles ) particles 

let update_pbest particle =
  let fitness = parameters.objective particle.position in
    if fitness <=> particle.pfitness then
      ( copy_from_to particle.position particle.pbest ;
	particle.pfitness <- fitness ) 

let update_gbest particle =
  let best_neighbor = best particle.neighbors in 
    if best_neighbor.pfitness <=> particle.gfitness then
      ( copy_from_to best_neighbor.position particle.gbest ;
	particle.gfitness <- best_neighbor.pfitness ) 

let update_position p =
  let c1 = 2.0 and c2 = 2.0 in
    for i = 0 to Array.length p.position - 1 do
      p.speed.(i) <- 0.8 *. p.speed.(i) +.
	c1 *. (Random.float 1.) *. (p.pbest.(i) -. p.position.(i)) +.
	c2 *. (Random.float 1.) *. (p.gbest.(i) -. p.position.(i)) ;
      p.position.(i) <-  let lower,upper = parameters.dimensions.(i) in
	max lower (min upper (p.speed.(i) +. p.position.(i) ) )
    done ;; 

let run swarm =
  List.iter update_position swarm;
  List.iter update_pbest swarm;
  List.iter update_gbest swarm;
  let p = best swarm in 
    print_vect p.gbest; print_string (" " ^ string_of_float (p.pfitness) ^ "\n")

let _ =
  Random.self_init();
  let swarm = gen_swarm 10 in
    full_neighborhood swarm;
    let i = ref 0 in
      while total_energy swarm > 0.1 do
	run swarm;
	print_string ("It: " ^ string_of_int !i ^ " Energy : " ^ string_of_float (total_energy swarm) ^ "\n");
	  incr i;
      done;;
