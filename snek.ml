open Queue


type dir = Up | Down | Left | Right
(* Module: Point
   Values:
   (x, y) pair of coordinates *)

module type PointSig = sig 
  type t 

  val init : int -> int -> t
  val from_list : (int * int) list -> t list
  val to_pair : t -> (int * int)
  val x : t -> int 
  val y : t -> int
end

module Point : PointSig = struct 

  type t = int * int

  let init x y = (x, y)

  let from_list lst = List.map (fun (x, y) -> init x y) lst

  let to_pair p = (fst p, snd p)

  let x p = fst p

  let y p = snd p 
end

(* Module: Snake
   Implementation:
    List of [Point.t] objects
   Values:
    [body], [direction]. *)

module type SnakeSig = sig

  type t
  (** Returns the list of points occupied by [s]. The last element of the 
      list is the 'head' of the snake, and the first element of the list is 
      the 'tail' of the snake (that is about to disappear if the move is 
      allowed).
  *)
  val body : t -> (Point.t) list
  (** Returns an object of type [dir] that corresponds to the current 
      direction of [s]. *)
  val direction : t -> dir
  (** Takes in a [Point.t list] object and a [dir] object.
      Initializes [s] of type [Snake.t] with the given inputs. *)
  val init : Point.t list -> dir -> t
end

module Snake : SnakeSig = struct 

  (* Each [Snake.t] object is a pair of [Point.t] list and 
     a [dir] object. *)
  type t = Point.t list * dir

  let body t = fst t
  let direction t = snd t

  let init lst direction = (lst, direction)

end


let next_point s : Point.t = 
  let head = s |> Snake.body |> List.rev |> List.hd in 
  let direction = s |> Snake.direction in
  let x = Point.x head in 
  let y = Point.y head in 
  match direction with 
  | Up -> Point.init x (y-1)
  | Down -> Point.init x (y+1)
  | Left -> Point.init (x-1) y
  | Right -> Point.init (x+1) y

let move (s:Snake.t) is_grow : Snake.t = 
  let next_pt = next_point s in 
  let p = Snake.body s in 
  match p with 
  | [] -> raise (Failure "Case matching failure.")
  | _::t when not is_grow -> 
    let pts = (List.append t [next_pt]) in 
    s |> Snake.direction |> Snake.init pts
  | p when is_grow -> 
    let pts = (List.append p [next_pt]) in 
    s |> Snake.direction |> Snake.init pts
  | _ -> raise (Failure "Case matching failure.")




let check_wall p (x_wall, y_wall) : bool = 
  let x = Point.x p in 
  let y = Point.y p in 
  if (x <= 0 || y <= 1 || x >= x_wall || y >= y_wall+1) then false
  else true

let check_self p s = 
  if s |> Snake.body |> List.mem p then false else true


let turn s new_dir : Snake.t = 
  match new_dir, Snake.direction s with 
  | Up, Down
  | Down, Up
  | Left, Right
  | Right, Left -> s
  | _, _ -> Snake.init (Snake.body s)  new_dir

let init_dir d = 
  if d = "Up" then Up 
  else if d = "Down" then Down
  else if d = "Left" then Left
  else if d = "Right" then Right
  else raise (Failure "Wrong input.")
