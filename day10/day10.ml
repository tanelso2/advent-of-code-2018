open Core

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  filtered_data

let parsed_input = read_file input_file |> List.map ~f:Star.parse_line

let () =
  ExtLib.print parsed_input

(* let () = *)
(*   ExtLib.print (part1 ()); *)
(*   ExtLib.print (part2 ()); *)
(* ;; *)
