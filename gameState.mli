open Snek

type state = Game | Lose

module type StateSig = sig 
  type t

  (** Initializes state [st] given the state [st], the walls [w], the snake [s], 
  the food [f], and the obstacles [o]. *)
  val init : state -> (int*int) -> Snake.t -> Point.t -> Point.t list -> t 

  (** Takes in [st] and returns its contents. Returns 
  [(state, w, s, f, o)], where:
  [state] is either [Game] or [Lose] and represents the state of the game;
  [w] represents the walls;
  [s] represents the snake;
  [f] represents the food;
  [o] represents the obstacles.*)
  val retrieve : t -> state * (int*int) * Snake.t * Point.t * Point.t list

  (** Takes in snake [s], walls [w], obstacles [o], and checks if 
      the next move is valid. *)
  val check_move : Snake.t -> (int*int) -> Point.t list-> bool

  (** Randomly generates food, with the given walls and obstacles. *)
  val generate_food : (int*int) -> Point.t list -> Point.t

  (** Takes in [st] and [d] and attempts to move the snake in [st] in the 
  direction [d]. Returns a new  *)
  val attempt_move : t -> dir -> t

  (** Takes in [p] and checks if [p] is in [o]. 
  Returns true if [p] is NOT in [o], and false if [p] is in [o].*)
  val check_obstacle : Point.t -> Point.t list -> bool

end

module State : StateSig


(** Returns a list of randomly located obstacles within the given walls 
[w]. The first parameter [n] determines the total numebr of the obstacles in 
the returned [Point.t list]. *)
val fill_obstacles : int -> (int * int) -> Point.t list

(** Draws the given state [st]. *)
val draw_state : State.t -> unit 
