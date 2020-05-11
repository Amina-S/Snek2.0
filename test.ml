open Main
open OUnit
open Snek
open GameState

(* TEST PLAN.

All functions in [Snek.mli], as well as most of the functions in 
[gameState.mli], were automatically tested using OUnit. There is no simple 
way to test functions in [gameState.mli] that involve either generating 
random points ([State.generate_food], [State.attempt_move], [fill_obstacle]) or 
printing in the terminal ([draw_state]). These functions were tested manually, 
using [make_play] and tweaking different parameters.

The test cases were developed using black box testing, i.e. only functions 
allowed by .mli files were used, without any further information on
implementation of these functions or modules like [Snake] or [Point]. There 
are helper functions inside [snek.ml] and [gameState.ml] that were not tested. 
Instead, if the black box testing produced the expected results, those were 
assumed to work correctly.

The black box testing was implemented for multiple reasons. First, as mentioned
above, the .mli files don't provide all the information on the implementation
nuances.
 *)

let make_fun_test_1 (name : string) expected_output f x = 
  name >:: 
  (fun _ -> assert_equal expected_output (f x))

let make_fun_test_2 (name : string) expected_output f x1 x2 = 
  name >:: 
  (fun _ -> assert_equal expected_output (f x1 x2))

let make_fun_test_3 (name : string) expected_output f x1 x2 x3 = 
  name >:: 
  (fun _ -> assert_equal expected_output (f x1 x2 x3))

let make_compare_test (name : string) expected_output received_output = 
  name >:: 
  (fun _ -> assert_equal expected_output received_output)



let p1 = Point.init 85 90
let p2 = Point.init 85 91
let p3 = Point.init 85 92
let p4 = Point.init 85 93 
let p5 = Point.init 85 94 
let p6 = Point.init 85 95 
let p7 = Point.init 85 96 
let p8 = Point.init 85 97
let p9 = Point.init 86 97
let p10 = Point.init 86 96
let p11 = Point.init 86 95
let p12 = Point.init 86 94

let s1 = Snake.init [p1; p2; p3] Down
let s2 = Snake.init [p2; p3; p4] Down 
let s3 = Snake.init [p1; p2; p3; p4] Down
let s4 = Snake.init [p1; p2; p3] Right
let s5 = Snake.init [p1; p2; p3] Left
let s6 = Snake.init [p3; p2; p1] Up


let snek_tests = [
  make_fun_test_1 "Point x." 85 Point.x p1;
  make_fun_test_1 "Point y." 90 Point.y p1;
  make_fun_test_2 "Point init." p1 Point.init 85 90;
  make_fun_test_1 "Point to_pair." (85, 90) Point.to_pair p1;
  make_fun_test_1 "Point from_list [1]." [p1; p2; p3] 
    Point.from_list [(85, 90); (85, 91); (85, 92)];
  make_fun_test_1 "Point from_list [2]." [] 
    Point.from_list [];
  make_fun_test_1 "Point from_list [3]." [p1] 
    Point.from_list [(85, 90)];
  make_fun_test_1 "Snake body." [p1; p2; p3] Snake.body s1;
  make_fun_test_1 "Snake direction [1]." Down Snake.direction s1;
  make_fun_test_1 "Snake direction [2]." Right Snake.direction s4;
  make_fun_test_2 "Snake init [1]." s1 Snake.init [p1; p2; p3] Down;
  make_fun_test_2 "Snake init [2]." s4 Snake.init [p1; p2; p3] Right;
  make_fun_test_1 "next_point [1]." p4 next_point s1;
  make_fun_test_1 "next_point [2]." (Point.init 86 92) next_point s4;
  make_fun_test_2 "move [1]." s2 move s1 false; 
  make_fun_test_2 "move [2]." s3 move s1 true;
  make_fun_test_2 "check_wall [1]." true check_wall p1 (100, 100);
  make_fun_test_2 "check_wall [2]." false check_wall p1 (30, 30);
  make_fun_test_2 "check_wall [3]." true check_wall p1 (86, 90);
  make_fun_test_2 "check_wall [4]." false check_wall p1 (86, 89);
  make_fun_test_2 "check_self [1]." false 
    check_self (Point.init 85 91) s1;
  make_fun_test_2 "check_self [2]." true 
    check_self (Point.init 86 91) s1;
  make_fun_test_2 "Turn possible direction." s4 turn s1 Right;
  make_fun_test_2 "Turn impossible direction [1]." s1 turn s1 Up;
  make_fun_test_2 "Turn impossible direction [2]." s4 turn s4 Left;
  make_fun_test_2 "Turn impossible direction [3]." s5 turn s5 Right;
  make_fun_test_2 "Turn impossible direction [4]." s6 turn s6 Down;
  make_fun_test_1 "init_dir [1]." Up init_dir "Up";
  make_fun_test_1 "init_dir [2]." Down init_dir "Down";
  make_fun_test_1 "init_dir [3]." Right init_dir "Right";
  make_fun_test_1 "init_dir [4]." Left init_dir "Left";
]

let s5 = Snake.init [p2; p3; p4; p5] Down
let s6 = Snake.init [p3; p4; p5; p6] Down
let s7 = Snake.init [p4; p5; p6; p7] Down
let s8 = Snake.init [p6; p7; p8; p9; p10; p11] Left
let s9 = Snake.init [p7; p8; p9; p10; p11; p12] Up

let st1 = State.init Game (96, 96) s1 p5 [] 
let st2 = State.init Game (96, 96) s2 p5 []
let st3 = State.init Game (100, 100) s8 p1 [] 
let st4 = State.init Lose (100, 100) s8 p1 []
let st5 = State.init Game (100, 100) s9 p1 []


(** Helper function for testing [fill_obstacles]. *)
let rec within_walls (w : (int * int)) (o : Point.t list) : bool = 
  match o with 
  | [] -> true
  | h::t -> if not (check_wall h w) then false 
    else within_walls w t

let gameState_tests = [
  make_fun_test_1 "State retrieve [1]." 
    (Game, (96, 96), s1, p5, []) State.retrieve st1;
  make_fun_test_1 "State retrieve [2]." 
    (Game, (96, 96), s2, p5, []) State.retrieve st2;
  make_fun_test_1 "State retrieve [3]." 
    (Lose, (100, 100), s8, p1, []) State.retrieve st4;
  make_fun_test_3 "Check move [1]. Can move." true 
    State.check_move s1 (100, 100) [];
  make_fun_test_3 "Check move [2]. Run into self." false 
    State.check_move s8 (100, 100) [];
  make_fun_test_3 "Check move [3]. Run into the x-wall." false 
    State.check_move s4 (86, 86) [];
  make_fun_test_3 "Check move [4]. Run into the y-wall." false 
    State.check_move s1 (92, 92) [];
  make_fun_test_3 "Check move [5]. Run into the obstacle" false 
    State.check_move s5 (100, 100) [p6];
  make_fun_test_2 "Move snake [1]. Able to move."
    st2 State.attempt_move st1 Down;
  make_fun_test_2 "Move snake [2]. Run into self." 
    st4 State.attempt_move st3 Left;
  make_fun_test_2 "Move snake [3]. Turn right before running into self." 
    st5 State.attempt_move st3 Up;
  make_fun_test_2 "Check obstacle [1]. Not in the list." 
    true State.check_obstacle p1 [p2; p3; p4];
  make_fun_test_2 "Check obstacle [2]. Not in the empty list." 
    true State.check_obstacle p1 [];
  make_fun_test_2 "Check obstacle [3]. In the list." 
    false State.check_obstacle p1 [p2; p3; p1];
  make_fun_test_2 "Check obstacle [4]. In the list twice." 
    false State.check_obstacle p1 [p1; p3; p1];
  make_fun_test_2 "Check obstacle [5]. Not in the list; has a point with 
  swapped coordinates." 
    true State.check_obstacle p1 [(Point.init (Point.y p1) (Point.x p1))];
  make_compare_test "Check fill_obstacles [1]. Empty obstacles list length." 
    0 ((fill_obstacles 0 (100, 100)) |> List.length);
  make_compare_test "Check fill_obstacles [2]. Non-empty obstacles list length." 
    5 ((fill_obstacles 5 (100, 100)) |> List.length);
  make_compare_test "Check fill_obstacles [3]. No obstacles outside or
  on the walls." true (within_walls (100, 100) (fill_obstacles 10 (100, 100)));

]

let suite = "suite" >::: List.flatten [
    snek_tests;
    gameState_tests;
  ]

let _ = run_test_tt_main suite
