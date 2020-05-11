type dir = Up | Down | Left | Right

module type PointSig = sig 
  type t 
  (** Initializes a [Point.t] object given its x- and y-coordinates, 
      respectively. *)
  val init : int -> int -> t

  (** Returns a [Point.t list] from the given [(int * int) list].*)
  val from_list : (int * int) list -> t list

  (** Returns a pair of integers representing the coordinates of the 
  given [Point.t] object. *)
  val to_pair : t -> (int * int)

  (** Returns the x-coordinate of [p]. *)
  val x : t -> int 

  (** Returns the y-coordinate of [p]. *)
  val y : t -> int
end

module Point : PointSig

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

module Snake : SnakeSig

(** Returns the next point where [s] would go. *)
val next_point : Snake.t -> Point.t

(** Updates [s] as if [s] moved in the direction that it's 'looking' at.
    [is_grow] is a Boolean that determines whether [s] grows as a result 
    of eating food. *)
val move : Snake.t -> bool -> Snake.t

(** Checks if the given point is inside the boundary defined by [(x, y)]. *)
val check_wall : Point.t -> int * int -> bool

(** Checks if the given point is NOT among the points occupied by [s].
    In other words, if [s] will NOT run into itself by going to the point [p],
    returns [true]. *)
val check_self : Point.t -> Snake.t -> bool

(** Makes [s] 'look' in the given direction [dir]. *)
val turn : Snake.t -> dir -> Snake.t

(** Initialized direction from the given [string]. *)
val init_dir : string -> dir