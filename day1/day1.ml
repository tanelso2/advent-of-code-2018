open Core
open ExtLib

let input_file = "input.txt"

let read_file (filename : string) : int list =
  let chan = In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter (fun x -> String.equal "" x |> not) raw_data in
  List.map int_of_string filtered_data

let input = read_file input_file

let part1 (input: int list) : int =
  List.fold_left (+) 0 input

let part2 (input : int list): int =
  let rec part2_helper (l : int list) (seen : int list) (curr_freq : int) : int =
    match l with
    | x::xs ->
      let new_freq = x + curr_freq in
      if List.mem new_freq seen
      then new_freq
      else part2_helper xs (new_freq :: seen) new_freq
    | [] -> part2_helper input seen curr_freq
  in
  part2_helper input []  0

type intSet = (int, Core.Int.comparator_witness) Set.t

let part2_set (input : int list): int =
  let rec part2_helper (l : int list) (seen : intSet) (curr_freq : int) : int =
    match l with
    | x::xs ->
      let new_freq = x + curr_freq in
      if Set.mem seen new_freq
      then new_freq
      else part2_helper xs (Set.add seen new_freq) new_freq
    | [] -> part2_helper input seen curr_freq
  in
  part2_helper input (Set.empty (module Int))  0

let () =
  print (part1 input);
  print (part2_set input);
;;
