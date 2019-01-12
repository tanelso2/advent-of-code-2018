open Core

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  filtered_data

let (initial_config, conversions) =
  let raw = read_file input_file in
  match raw with
  | init_str::con_strs -> (Flowerbed.parse_initial_config init_str, Flowerbed.parse_conversion_lines con_strs)
  | _ -> failwith "Unable to parse input file"

let part1 (): int =
  let r = List.range 0 20 in
  let final = List.fold_left r ~init:initial_config ~f:(fun fb _ -> Flowerbed.get_next_flowerbed conversions fb) in
  Flowerbed.sum final

(* let part2 (): int = *)
(*   let current = ref initial_config in *)
(*   let i = ref 0 in *)
(*   while !i <= 100000000000 do *)
(*     current := Flowerbed.get_next_flowerbed conversions !current; *)
(*     incr i *)
(*   done; *)
(*   !i *)

let part2 (): int = (50000000000 - 99) * 51 + 5932

let () =
  ExtLib.print (part1 ());
  ExtLib.print (part2 ())


