open Core

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  filtered_data

let parsed_input = read_file input_file |> List.map ~f:Star.parse_line


let () =
  ExtLib.print parsed_input;
  let stars = ref parsed_input in
  let collection = ref [] in
  for i = 1 to 100000 do 
    stars := List.map !stars ~f:Star.move;
    let bb = Star.bounding_box_size !stars in
    collection := (i, bb, !stars)::!collection
  done;
  let manhattan_distance (x, y) = x + y in
  let f (_, bb, _) = manhattan_distance bb in
  let (i, _, l) = Util.List.min_by_exn !collection ~key_fn:f in
  print_string "Part1 is:\n";
  Star.draw_chart l |> print_string;
  print_string "\nPart2 is:\n";
  ExtLib.print i


(* let () = *)
(*   ExtLib.print (part1 ()); *)
(*   ExtLib.print (part2 ()); *)
(* ;; *)
