open GameState
open Snek
open Unix

exception Timeout

let pp_instructions () = 
  ANSITerminal.(print_string[green]"\nInstructions: \n");
  print_endline ("Use w, a, s, and d keys to navigate the snake");
  print_endline ("Press the navigation keys to speed up the snake");
  print_endline("The bigger your snake grows, the faster your score increases");
  print_endline ("Press spacebar to pause and unpause");
  print_endline ("Good luck!")

let start () = ANSITerminal.(print_string[green]"\n\nWelcome to Snake! \n");
  pp_instructions ();
  ANSITerminal.(print_string[green]"\nLevels: \n");
  print_endline "1 - Playtime";
  print_endline "2 - Into the Real World"; 
  print_endline "3 - Getting Confident";
  print_endline "4 - Showoff";
  print_endline "5 - Impossible";
  print_endline "Pick a level (1,2,3,4,5) and press enter to start... \n";
  Stdlib.input_line Stdlib.stdin

let quit () = exit 0

let pp_pause () = 
  ANSITerminal.(print_string[yellow]"\n\t\t GAME PAUSED ");
  ANSITerminal.(print_string[yellow]"\n\t\t press space to keep playing... ")



let rec pause () = 
  pp_pause();
  print_endline "";
  match input_char Stdlib.stdin with 
   |' ' -> ()
   | _ -> print_endline ""; pause ()


let get_char () =
  try 
    let io = Unix.tcgetattr Unix.stdin in
    let () =
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { io with Unix.c_icanon = false } in
    let res = input_char Stdlib.stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN io; res
  with End_of_file -> (' ')

(* Interactive part: Timed input. *)
let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout f arg time default_value =
  let _ = Sys.signal Sys.sigalrm sigalrm_handler in
  let old_behavior = Sys.Signal_handle (fun _ -> ()) in 
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior 
  in ignore (Unix.alarm time) ;
  try  let res = f arg in reset_sigalrm () ; res  
  with exc -> reset_sigalrm () ;
    if exc=Timeout then default_value else raise exc ;;

let read_move sn = 
  match timeout get_char () 1 '_' with
  |'w' -> Up
  |'s' -> Down
  |'a' -> Left
  |'d' -> Right 
  |' ' -> pause (); Snake.direction sn
  | _  -> Snake.direction sn

let rec play (st: State.t) : unit = match State.retrieve st with
  | (Game, w, sn, f, o)  -> 
    ignore (Sys.command ("clear")); 
    draw_state st;
    sleepf 0.1; 
    let d = read_move sn in 
    let new_st = State.attempt_move st d in
    ignore (play (new_st))
  | (Lose, w, s, p, o)  ->   
  ANSITerminal.(print_string[red]"\n\nYou lost... \n");
    print_newline()


let rec play_game1 () =
  ignore (Sys.command ("clear"));
  let w = (70, 20) in
  let (sn: Snake.t) = Snake.init [Point.init 35 18] Up in
  let (fd: Point.t) = Point.init 25 16 in
  let (o: Point.t list) = fill_obstacles 5 w in
  let (st: State.t) = State.init Game w sn fd o in
  ignore (play st)

let rec play_game2 () =
  ignore (Sys.command ("clear"));
  let w = (60, 20) in
  let (sn: Snake.t) = Snake.init [Point.init 30 18] Up in
  let (fd: Point.t) = Point.init 30 16 in
  let (o: Point.t list) = fill_obstacles 30 w in
  let (st: State.t) = State.init Game w sn fd o in
  ignore (play st)

let rec play_game3 () =
  ignore (Sys.command ("clear"));
  let w = (45, 15) in
  let (sn: Snake.t) = Snake.init [Point.init 22 12] Up in
  let (fd: Point.t) = Point.init 22 10 in
  let (o: Point.t list) = fill_obstacles 50 w in
  let (st: State.t) = State.init Game w sn fd o in
  ignore (play st)

let rec play_game4 () =
  ignore (Sys.command ("clear"));
  let w = (40, 20) in
  let (sn: Snake.t) = Snake.init [Point.init 20 18] Up in
  let (fd: Point.t) = Point.init 20 16 in
  let (o: Point.t list) = fill_obstacles 75 w in
  let (st: State.t) = State.init Game w sn fd o in
  ignore (play st)

let rec play_game5 () =
  ignore (Sys.command ("clear"));
  let w = (20, 10) in
  let (sn: Snake.t) = Snake.init [Point.init 10 8] Up in
  let (fd: Point.t) = Point.init 12 8 in
  let (o: Point.t list) = fill_obstacles 100 w in
  let (st: State.t) = State.init Game w sn fd o in
  ignore (play st)



(* set default level to 3 *)
let select_level = match (start ()) with  
  | "1" -> "1"
  | "2" -> "2"
  | "3" -> "3"
  | "4" -> "4"
  | "5" -> "5"
  |  _ -> "3"


let game_countdown level = 
  (* for some reason timing and clearing won't work without print_endline *)
  ignore (Sys.command ("clear"));  
  ANSITerminal.(print_string[green]("\n\tLevel: "^level)); 
  print_endline ("");
  ANSITerminal.(print_string[green]"\tStarting in 3... "); 
  print_endline ("");
  sleepf 1.0;
  ignore (Sys.command ("clear"));
  ANSITerminal.(print_string[green]("\n\tLevel: "^level)); 
  print_endline ("");
  ANSITerminal.(print_string[green]"\tStarting in 2... "); 
  print_endline ("");
  sleepf 1.0;
  ignore (Sys.command ("clear"));
  ANSITerminal.(print_string[green]("\n\tLevel: "^level)); 
  print_endline ("");
  ANSITerminal.(print_string[green]"\tStarting in 1... "); 
  print_endline ("");
  sleepf 1.0;
  ignore (Sys.command ("clear"))


let rec main () =
  (match (select_level) with
   | "1" -> game_countdown "1"; play_game1 ()
   | "2" -> game_countdown "2"; play_game2 ()
   | "3" -> game_countdown "3"; play_game3 ()
   | "4" -> game_countdown "4"; play_game4 ()
   | "5" -> game_countdown "5"; play_game5 ()
   |  _ -> failwith "what happened" );
  ANSITerminal.(print_string[red]"\n\nPlay again? (enter y to play) >> ");
  (match read_line() with 
   |"y" -> main()
   | _ -> ignore (Sys.command ("clear")); ())

(* Execute the game engine. *)
let () = main ()
