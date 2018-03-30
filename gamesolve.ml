(*                       
                                CS 51
                             Spring 2018
                        Problem Set 5: Search
 
                             Game Solving

This file contains the signature for a GAMESOLVER module as well as a
higher-order functor, MakeGameSolver. A GAMESOLVER module solves the
game by searching for a path from the initial state to the goal state.

The MakeGameSolver functor takes in a collection functor and a
GAMEDESCRIPTION and returns a GAMESOLVER. The collection specified by
the functor is used to store the states that have been reached so
far. Thus, the ordering in which states are delivered by the
collection (with the take function) determines the order in which the
states are searched. A stack regime gives depth-first search, a queue
regime breadth-first search.

At the bottom of the file are definitions for a depth-first search and
breadth-first game solvers, partially applied versions of the
MakeGameSolver functor that use certain collections to engender
different search methods.
 *)

open Set
open Collections
open Gamedescription

(* GAMESOLVER -- a module type that provides for solving games and
   graphically drawing the results *)
       
module type GAMESOLVER = 
  sig
    exception CantReachGoal
    type state
    type move
    val solve : unit -> move list * state list
    val draw : state list -> move list -> unit
    val print_state: state -> unit
  end

(* MakeGameSolver -- a higher-order functor that generates game solvers, with type
 
  (functor(sig type t end -> COLLECTION)) -> GAMEDESCRIPTION -> GAMESOLVER

   A functor that given a functor from a GAMEDESCRIPTION to a
   game description-specific Collection, as well as a GAMEDESCRIPTION,
   returns a full GAMESOLVER module. 
 *)

module MakeGameSolver (DSFunc : functor(Element : sig type t end) -> 
                                       (COLLECTION with type elt = Element.t))
                      (G : GAMEDESCRIPTION)                  
       : (GAMESOLVER with type state = G.state
                      and type move = G.move) =
  struct
    exception CantReachGoal
    type state = G.state
    type move = G.move

    let solve () : move list * state list =
      let module SolveColl = DSFunc(struct type t = state * (move list) end) in
      let module SolveSetArg =
        struct
          type t = state
          let compare = G.compare_states
        end in
      let module SolveSet = Make(SolveSetArg) in
      let pending = SolveColl.add (G.initial_state, []) SolveColl.empty in
      let visited = SolveSet.empty in
      let rec gamesolver (p : SolveColl.collection) (v : SolveSet.t) (expanded : state list) : move list * state list = 
        if p <> SolveColl.empty then
          let (current_state, mlst), coll = SolveColl.take p in   
          if SolveSet.mem current_state v then 
            gamesolver coll v expanded
          else  
            if G.is_goal current_state then mlst, (current_state :: expanded)
            else   
              let new_v = SolveSet.add current_state v in
              let new_e = current_state :: expanded in
              let nbor_states = 
                List.map (fun (s, m) -> s, mlst @ [m]) (G.neighbors current_state) in
              let new_p = List.fold_left (fun x y -> SolveColl.add y x) coll nbor_states in
              gamesolver new_p new_v new_e
        else raise CantReachGoal in
        gamesolver pending visited []    
    
    let draw = G.draw

    let print_state = G.print_state

  end ;;
     
(* DFSSolver and BFSSolver: Higher-order Functors that take in a
   GAMEDESCRIPTION, and will return Games that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)
module DFSSolver = MakeGameSolver(MakeStackList) ;;
module BFSSolver = MakeGameSolver(MakeQueueList) ;;
module FastBFSSolver = MakeGameSolver(MakeQueueStack) ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) the problem set took you to complete.  We care about your
responses and will use them to help guide us in creating future
assignments.
......................................................................*)

let minutes_spent_gamesolve () : int = 300 ;;
