open Snek
open Unix 

type state = Game | Lose

module type StateSig = sig 
  type t

  val init : state -> (int*int) -> Snake.t -> Point.t ->Point.t list -> t
  val retrieve : t -> state * (int*int) * Snake.t * Point.t * Point.t list
  val check_move : Snake.t -> (int*int) -> Point.t list-> bool
  val generate_food : (int*int) -> Point.t list -> Point.t
  val attempt_move : t -> dir -> t
  val check_obstacle : Point.t -> Point.t list -> bool

end

module State : StateSig = struct
  type t = state * (int*int) * Snake.t * Point.t * Point.t list

  let init st w s f o = (st, w, s, f, o)

  let retrieve (q:t) = match q with 
    | (st, w, s, f, o) -> (st, w, s , f, o)

  let check_food s f = 
    let next_pt = next_point s in 
    next_pt = f



  let check_obstacle p o = not (List.mem p o)


  let check_move s w o = 
    let next_pt = next_point s in 
    (check_wall next_pt (fst w, snd w))
    && (check_self next_pt s) && (check_obstacle next_pt o)

  let rec generate_food w o = 
    let x = fst w in 
    let y = snd w in 
    let x_random = Random.int (x-1) in 
    let y_random = Random.int (y-1) in 
    let f = Point.init (x_random+1) (y_random+2) in 
    if List.mem f o then generate_food w o else f

  let attempt_move q d = 
    match q with 
    | Game, w, s, f, o ->
      let s_new = turn s d in 
      if not (check_move s_new w o) then init Lose w s_new f o
      else 
        let next_pt = next_point s_new in 
        let is_grow = (next_pt = f) in 
        let s' = move s_new is_grow in 
        let f' = if is_grow then (
          ignore(output_char Stdlib.stdout '\007');
          generate_food w o )
            else f in init Game w s' f' o
    | Lose, _, _, _, _ -> raise (Failure "Attempted to move after lose! ")


end

let fill_obstacles n w = 
  let rec loop n w l = 
    if (List.length l < n) then (
      let x = fst w in 
      let y = snd w in 
      let x_random = Random.int (x-1) in 
      let y_random = Random.int (y-1) in 
      let p = Point.init (x_random+1) (y_random+2) in
      loop n w (p::l) )
    else l in 
  loop n w []

  

(* prints top and bottom walls *)
let rec print_horizontal (i:int)(x:int) = 
  if (i = x) then print_endline "*";
  if (i < x) then (
    print_string "* "; 
   print_horizontal (i+1) x)
   
(* prints the middle rows *)
let rec print_sides (i:int) (x:int) y' sn_list p o = 
  let p' = Point.to_pair p in 
  if (i <= x) then 
    if (List.mem (Point.init i y') o) then (
      print_string "X "; 
      print_sides (i+1) x y' sn_list p o)
    else
    if (List.mem (Point.init i y')sn_list) then (
      print_string "O "; 
      print_sides (i+1) x y' sn_list p o)
    else
    if ((i, y') = p') then (
      print_string "$ "; 
      print_sides (i+1) x y' sn_list p o)
    else
    if (i = 0) then (
      print_string "* "; 
      print_sides (i+1) x y' sn_list p o)
    else 
    if (i = x) then (
      print_endline "* "; 
      print_sides (i+1) x y' sn_list p o)
    else
      (print_string "  "; 
      print_sides (i+1) x y' sn_list p o)


let rec draw_game (i:int) (x:int) (y:int) y' list p o= 
  if (i = y ||  i = 0) then print_horizontal 0 x 
    else print_sides 0 x y' list p o;
  if (i < y) then ignore(draw_game (i+1) x y (y'+1) list p o)
  else if (i = y) then (
    print_string "\t  Score:   "; 
    print_int ((List.length list - 1)* (List.length list) * 5/2); 
    print_newline() 
    )

let draw_state (st: State.t) = match State.retrieve st with 
  | (_, (x,y), s, p, o) -> draw_game 0 x y 1 (Snake.body s) p o







