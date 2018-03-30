(*  
                                CS 51
                             Spring 2018
                        Problem Set 5: Search

                     Testing Tile and Maze Puzzles

In this file, we provide some tests of the game solver by generating
random tile and maze puzzles and running the various solving methods
(depth-first, breadth-first, etc.) on the examples. This code requires
working versions of the Collections and Gamesolve modules, so it won't
compile until you've completed those parts of the problem set. Once
those are done, however, you can build tests.byte and run it to watch
some puzzles being solved and get some timings. This will be useful in
designing your own experiments, as required in Problem 3 of the problem
set.  *)

open CS51

open Collections
open Tiles
open Mazes
open Gamedescription
open Gamesolve
        
(*......................................................................
                       SAMPLE TILE GAME TESTING
*)

(* initialize to known seed for reproducibility *)
let _  = Random.init (0)     

(* A solved tile game board for comparison*)
let dims = 3, 3 ;;
let solved : board =
  [| [|Tile 1; Tile 2; Tile 3|];
     [|Tile 4; Tile 5; Tile 6|];
     [|Tile 7; Tile 8; EmptyTile|]; |] ;;
                
(* rand_elt -- Return a random state out of a list returned by the
   neighbors function of a tile game description *)
let rand_elt l : board = 
    fst (List.nth l (Random.int (List.length l))) ;;

(* random_tileboard -- Generate a random TileBoard by performing some
   random moves on the solved board *)
let random_tileboard () : board =
  let cINITIAL_MOVE_COUNT = 45 in
  let module G : (GAMEDESCRIPTION with type state = Tiles.board
                                   and type move = Tiles.direction) = 
    MakeTileGameDescription (struct
                              let initial = solved
                              let dims = dims
                            end) in
  let rec make_moves n b = 
    if n <= 0 then b
    else make_moves (n - 1) (rand_elt (G.neighbors b)) in
  make_moves cINITIAL_MOVE_COUNT G.initial_state ;;

(* test_tile_game -- generate a random board and solve it, reporting
   results with various solvers *)
let test_tile_game () : unit =

  (* Generate a game with a random initial position *)
  let module G : (GAMEDESCRIPTION with type state = Tiles.board
                    and type move = Tiles.direction) = 
    MakeTileGameDescription 
      (struct
          let initial = random_tileboard () 
          let dims = dims
      end) in
  
  Printf.printf("TESTING RANDOMLY GENERATING TILEGAME...\n");
  (* Guarantee that the initial state is not the goal state *)
  assert (not (G.is_goal G.initial_state));
  
  (* Create some solvers *)
  let module DFSG = DFSSolver(G) in 
  let module BFSG = BFSSolver(G) in
  let module FastBFSG = FastBFSSolver(G) in

  (* Run the solvers and report the results *)
  Printf.printf("Regular BFS time:\n");
  let (bfs_path, bfs_expanded) = call_reporting_time BFSG.solve ()  in
  flush stdout;
  assert (G.is_goal (G.execute_moves bfs_path));

  Printf.printf("Faster BFS time:\n");
  let (fbfs_path, bfs_expanded) = call_reporting_time FastBFSG.solve () in
  (* For breadth first search, you should also check the length *)
  flush stdout;
  assert (G.is_goal (G.execute_moves bfs_path));
  assert (G.is_goal (G.execute_moves fbfs_path));
  assert (List.length fbfs_path = List.length bfs_path);

  (* We skip the depth-first search for lack of time :) *)
  (*
  Printf.printf("Depth First Searching\n");
  let dfs_path, dfs_expanded = call_reporting_time DFSG.solve () in 
  flush stdout;
  DFSG.draw dfs_expanded dfs_path;
  *)
  Printf.printf("DONE TESTING RANDOMLY GENERATED TILE GAME\n");

  (* Display the path found by one of the solvers *)
  BFSG.draw bfs_expanded bfs_path ;;
  
let _ = test_tile_game() ;;

(*......................................................................
                       SAMPLE MAZE GAME TESTING
*)

let init_maze = [|
    [| EmptySpace; EmptySpace; Wall; EmptySpace; EmptySpace|];
    [| Wall; EmptySpace; EmptySpace; EmptySpace; EmptySpace|];
    [| Wall; Wall; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; EmptySpace; EmptySpace; Wall; EmptySpace|];
    [| EmptySpace; Wall; EmptySpace; EmptySpace; EmptySpace|];
   |] ;;

(* square_maze -- Given the 5 * 5 initial maze above, and a "ct"
   number of times to square it, generates a maze that is of size (5 *
   ct) x (5 * ct), with the initial maze tiled on it *)
let square_maze (ct : int) : maze = 
  let new_maze = Array.make_matrix (5 * ct) (5 * ct) EmptySpace in 
  let col_bound = (5 * ct) in 
  let row_bound = (5 * ct) - 5 in 
  (* helper function that tiles the original maze in to the new maze *)
  let rec copy_maze (crow: int) (ccol: int) : maze =     
    if (ccol = col_bound && crow = row_bound) then new_maze
    else if (ccol = col_bound) then 
      copy_maze (crow + 5) (0)
    else
      (* This is atrocious and should probably be done with one fold *)
      let _ = 
        (Array.blit init_maze.(crow mod 5) 0 new_maze.(crow) ccol 5;
         Array.blit init_maze.((crow + 1) mod 5) 0 new_maze.(crow + 1) ccol 5;
         Array.blit init_maze.((crow + 2) mod 5) 0 new_maze.(crow + 2) ccol 5;
         Array.blit init_maze.((crow + 3) mod 5) 0 new_maze.(crow + 3) ccol 5;
         Array.blit init_maze.((crow + 4)mod 5) 0 new_maze.(crow + 4) ccol 5;)
      in
      (* Keep on recursing *)
      copy_maze (crow) (ccol + 5) in
  copy_maze 0 0 ;;
  
(* Note that once the mazes get too big, the OCaml graphics module can't 
   properly render them *)
  
module TestMazeI : MAZEINFO = 
  struct
    let maze = square_maze 1
    let initial_pos =  (0,0)
    let goal_pos = (4,4)
    let dims = (5, 5)
  end
    
module TestMazeII : MAZEINFO = 
  struct
    let maze = square_maze 2
    let initial_pos =  (0,0)
    let goal_pos = (9,9)
    let dims = (10, 10)
  end
    
module TestMazeIII : MAZEINFO = 
  struct
    let maze = square_maze 3
    let initial_pos =  (0,0)
    let goal_pos = (14,14)
    let dims = (15, 15)
  end
    
(* TestMazeGame functor, returns a module that has one function (run_tests)
 *)
module TestMazeGame(M : MAZEINFO) = 
  struct
    let run_tests () = 
      (* Make a MazeGameDescription using the MAZEINFO passed in to our functor *)
      let module MGame = MakeMazeGameDescription(M) in 
      
      (* Generate two solvers -- a BFS solver and a DFS solver *)
      let module DFSG = DFSSolver(MGame) in 
      let module FastBFSG = FastBFSSolver(MGame) in 
      let module BFSG = BFSSolver(MGame) in 
      Printf.printf("TESTING MAZE GAME...\n");
      
      (* Solve the BFS maze and make sure that the path reaches the goal *)
      Printf.printf("Regular BFS time:\n");
      let (bfs_path, bfs_expanded) = call_reporting_time BFSG.solve ()  in
      assert (MGame.is_goal (MGame.execute_moves bfs_path));
      
      (* Solve the BFS maze with the efficient queue and make sure the
         path reaches the goal *)
      Printf.printf("Fast BFS time:\n");
      let (fbfs_path, bfs_expanded) = call_reporting_time FastBFSG.solve ()  in
      assert (MGame.is_goal (MGame.execute_moves fbfs_path));
      
      (* Assert the length of the fast BFS and regular BFS path are the
         same, as BFS always finds the shortest path *)
      assert ((List.length fbfs_path) = (List.length bfs_path));
      
      (* Solve the DFS maze and make sure the path reaches the goal *)
      Printf.printf("DFS time:\n");
      let (dfs_path, dfs_expanded) = call_reporting_time DFSG.solve ()  in
      assert (MGame.is_goal (MGame.execute_moves dfs_path));

      Printf.printf("DONE TESTING MAZE GAME, DISPLAYING MAZE NOW\n");
      BFSG.draw bfs_expanded bfs_path;
      DFSG.draw dfs_expanded dfs_path    
  end ;;
  
(* Run the testing for each of our test mazes *)
module MI   = TestMazeGame(TestMazeI)
module MII  = TestMazeGame(TestMazeII)
module MIII = TestMazeGame(TestMazeIII)

(* A test for MakeQueueStack *)
let test_MakeQueueStack () =
  let module IntMQS = MakeQueueStack(struct type t = int end) in
  let c = IntMQS.empty in
  assert (IntMQS.is_empty c);
  let x = Random.int 1000 in
  let y = Random.int 1000 in
  let z = Random.int 1000 in
  let c = IntMQS.add x c in
  let l = IntMQS.length c in 
  assert (l = 1);
  assert (not (IntMQS.is_empty c));
  let c = IntMQS.add y c in 
  let l = IntMQS.length c in 
  assert (l = 2);
  let c = IntMQS.add z c in 
  let l = IntMQS.length c in 
  assert (l = 3);
  let n, c = IntMQS.take c in 
  assert (n = x);
  let l = IntMQS.length c in 
  assert (l = 2);
  let c = IntMQS.add y (IntMQS.add z c) in 
  let l = IntMQS.length c in 
  assert (l = 4);
  let n, c = IntMQS.take c in
  assert (n = y); 
  let l = IntMQS.length c in 
  assert (l = 3);
  let n, c = IntMQS.take c in
  assert (n = z); 
  let l = IntMQS.length c in 
  assert (l = 2);
  let n, c = IntMQS.take c in
  assert (n = z); 
  let l = IntMQS.length c in 
  assert (l = 1);
  let n, c = IntMQS.take c in
  assert (n = y); 
  assert (IntMQS.is_empty c);
  let error =
    try
      Some (IntMQS.take c)
    with
      IntMQS.Empty -> None in
  assert (error = None) 

(* A test for solve *)
let test_solve () =
  let init_maze = [|
      [| EmptySpace; EmptySpace; Wall|];
      [| Wall; EmptySpace; Wall|];
      [| EmptySpace; EmptySpace; EmptySpace|];
     |] in
  let module M : MAZEINFO = 
    struct
      let maze = init_maze
      let initial_pos = (0,0)
      let goal_pos = (2, 2)
      let dims = (3, 3)
    end in
  let module MGame = MakeMazeGameDescription(M) in
  let module DFSG = DFSSolver(MGame) in  
  let module BFSG = BFSSolver(MGame) in
  assert (DFSG.solve () = ([Left; Down; Down; Left],
                              [(2, 2);(2, 0);(2, 1);(1, 1);(0, 1);(0, 0)]));
  assert (BFSG.solve () = ([Left; Down; Down; Left], 
                              [(2, 2);(2, 1);(1, 1);(0, 1);(0, 0)]));
  let x, _ = DFSG.solve () in
  assert (MGame.is_goal (MGame.execute_moves x));
  let x, _ = BFSG.solve () in
  assert (MGame.is_goal (MGame.execute_moves x));
  let init_maze = [|
      [| EmptySpace; Wall; Wall|];
      [| Wall; Wall; Wall|];
      [| EmptySpace; EmptySpace; EmptySpace|];
     |] in
  let module M : MAZEINFO = 
    struct
      let maze = init_maze
      let initial_pos = (0,0)
      let goal_pos = (2, 2)
      let dims = (3, 3)
    end in
  let module MGame = MakeMazeGameDescription(M) in
  let module DFSG = DFSSolver(MGame) in  
  let error =
    try
      Some (DFSG.solve ())
    with
      DFSG.CantReachGoal -> None in
  assert (error = None) 

let _ =
  MI.run_tests();
  MII.run_tests();
  MIII.run_tests();
  test_MakeQueueStack();
  test_solve();
