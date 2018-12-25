open Core

let players = 423

let last_marble = 71944

type playerScores = (int, int, Int.comparator_witness) Map.t

type gameState = {
  current_player: int;
  num_players: int;
  player_scores: playerScores;
  circle: Circle.t;
}

let add_scores (m: playerScores) (player: int) (num: int): playerScores =
  let change_fn = function
    | Some x -> Some (x + num)
    | None -> Some num
  in
  Map.change m player ~f:change_fn

let do_turn (state: gameState) (curr_num: int): gameState =
  let {current_player; num_players; player_scores; circle} = state in
  let next_player = (current_player + 1) % num_players in
  if (curr_num % 23) = 0 then
    let (new_circle, removed_marble) = Circle.remove circle in
    let score_add = curr_num + removed_marble in
    let new_scores = add_scores player_scores current_player score_add in
    {
      current_player = next_player;
      num_players = num_players;
      player_scores = new_scores;
      circle = new_circle;
    }
  else 
    let new_circle = Circle.add circle curr_num in
    {
      current_player = next_player;
      num_players = num_players;
      player_scores = player_scores;
      circle = new_circle;
    }

let part1 (): int =
  let init = {
    current_player = 0;
    num_players = players;
    player_scores = Map.empty (module Int);
    circle = Circle.empty ();
  }
  in
  let rec helper state curr_num =
    let new_state = do_turn state curr_num in
    if curr_num = last_marble
    then new_state
    else helper new_state (curr_num + 1)
  in
  let {player_scores; _} = helper init 1 in
  match Util.max_by player_scores ~key_fn:ident with 
  | Some (_, score) -> score
  | None -> failwith "This shouldn't happen"

let part2 () =
  let init = {
    current_player = 0;
    num_players = players;
    player_scores = Map.empty (module Int);
    circle = Circle.empty ();
  }
  in
  let rec helper state curr_num =
    let new_state = do_turn state curr_num in
    if curr_num = (last_marble * 100)
    then new_state
    else helper new_state (curr_num + 1)
  in
  let {player_scores; _} = helper init 1 in
  match Util.max_by player_scores ~key_fn:ident with 
  | Some (_, score) -> score
  | None -> failwith "This shouldn't happen"

let () =
  ExtLib.print (part1 ());
  ExtLib.print (part2 ());
;;
