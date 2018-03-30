(*                                      
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
                             Experiments
  
*)	

open CS51

open Collections
open Tiles
open Mazes
open Gamedescription
open Gamesolve

(*......................................................................
                       SAMPLE TILE GAME TESTING
*)

(* Create two basic tile boards that are small enough to be easily tested in *)
module TestTileI : TILEINFO = 
  struct
    let dims = 2, 2
    let initial : board =
      [| [|Tile 1; Tile 2|];
         [|Tile 3; EmptyTile|]; |]
  end

module TestTileII : TILEINFO = 
  struct
    let dims = 3, 3
    let initial : board =
      [| [|Tile 1; Tile 2; Tile 3|];
         [|Tile 4; Tile 5; Tile 6|];
         [|Tile 7; Tile 8; EmptyTile|]; |]
  end   

(* TestTileGame functor, returns a module that has one function (test_tile) *)
module TestTileGame(T : TILEINFO) =
  struct
    let test_tile (init_moves : int) : unit =
      (* rand_elt is imported from Tests because opening Tests prompted 
         it to be evaluated when evaluating Experiments *)	
      let rand_elt l : board = fst (List.nth l (Random.int (List.length l))) in
      (* rndom_tileboard -- Generate a random TileBoard by performing the
         initial moves on the initial board. *)
      let rec rndom_tileboard (n : int) (b : board) : board = 
      	let module TGame = MakeTileGameDescription(T) in
        if n <= 0 then b
        else rndom_tileboard (n - 1) (rand_elt (TGame.neighbors b)) in
      (* Generate a game with a random initial position *)  	
      let module G = 
        MakeTileGameDescription 
          (struct
            let initial = rndom_tileboard init_moves T.initial
            let dims = T.dims
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
      let (bfs_path, _) = call_reporting_time BFSG.solve ()  in
      flush stdout;
      assert (G.is_goal (G.execute_moves bfs_path));

      Printf.printf("Faster BFS time:\n");
      let (fbfs_path, _) = call_reporting_time FastBFSG.solve ()  in
      (* For breadth first search, check the length *)
      flush stdout;
      assert (G.is_goal (G.execute_moves bfs_path));
      assert (G.is_goal (G.execute_moves fbfs_path));
      assert (List.length fbfs_path = List.length bfs_path);

      (* This experiment will not use DFS due to its long runtimes when the 
         the tiles get more and more complex *)

      Printf.printf("DONE TESTING RANDOMLY GENERATED TILE GAME\n");
  end ;;

(* Run the testing for each of our test mazes *)
module TI   = TestTileGame(TestTileI)
module TII  = TestTileGame(TestTileII)

let _ =
  TI.test_tile 15;
  TI.test_tile 30;
  TI.test_tile 45;
  TII.test_tile 15;
  TII.test_tile 30;
  TII.test_tile 45;;

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
  
(* Create some sample mazes to be solved, both forwards and backwards *)
module TestMazeI : MAZEINFO = 
  struct
    let maze = square_maze 1
    let initial_pos =  (0,0)
    let goal_pos = (4,4)
    let dims = (5, 5)
  end

module TestMazeI' : MAZEINFO = 
  struct
    let maze = square_maze 1
    let initial_pos =  (4,4)
    let goal_pos = (0,0)
    let dims = (5, 5)
  end  
    
module TestMazeII : MAZEINFO = 
  struct
    let maze = square_maze 2
    let initial_pos =  (0,0)
    let goal_pos = (9,9)
    let dims = (10, 10)
  end

module TestMazeII' : MAZEINFO = 
  struct
    let maze = square_maze 2
    let initial_pos =  (9,9)
    let goal_pos = (0,0)
    let dims = (10, 10)
  end  
    
module TestMazeIII : MAZEINFO = 
  struct
    let maze = square_maze 3
    let initial_pos =  (0,0)
    let goal_pos = (14,14)
    let dims = (15, 15)
  end

module TestMazeIII' : MAZEINFO = 
  struct
    let maze = square_maze 3
    let initial_pos =  (14,14)
    let goal_pos = (0,0)
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
      let (fbfs_path, bfs_expanded) = call_reporting_time FastBFSG.solve () in
      assert (MGame.is_goal (MGame.execute_moves fbfs_path));
      
      (* Assert the length of the fast BFS and regular BFS path are the
         same, as BFS always finds the shortest path *)
      assert ((List.length fbfs_path) = (List.length bfs_path));
      
      (* Solve the DFS maze and make sure the path reaches the goal *)
      Printf.printf("DFS time:\n");
      let (dfs_path, dfs_expanded) = call_reporting_time DFSG.solve () in
      assert (MGame.is_goal (MGame.execute_moves dfs_path));

      Printf.printf("DONE TESTING MAZE GAME\n");  
  end ;;
  
(* Run the testing for each of our test mazes *)
module MI = TestMazeGame(TestMazeI)
module MI' = TestMazeGame(TestMazeI')
module MII = TestMazeGame(TestMazeII)
module MII' = TestMazeGame(TestMazeII')
module MIII = TestMazeGame(TestMazeIII)
module MIII' = TestMazeGame(TestMazeIII')

let _ =
  MI.run_tests();
  MI'.run_tests();
  MII.run_tests();
  MII'.run_tests();
  MIII.run_tests();
  MIII'.run_tests();
