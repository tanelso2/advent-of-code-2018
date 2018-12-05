open Core

let input_file = "input.txt"

let read_file (filename : string) : string =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  List.hd_exn filtered_data

let input = read_file input_file

let should_be_removed (a: char) (b: char): bool =
    not (a = b) && ((a = Char.uppercase b) || (Char.uppercase a = b))

let rec remove_reactions (s: string): string =
    let initial_length = String.length s in
    let rec f cl =
        match cl with
        | a::b::xs -> if should_be_removed a b
                      then f xs
                      else a::(f (b::xs))
        | [x] -> [x]
        | [] -> []
    in
    let new_s = String.to_list s |> f |> String.of_char_list in
    let new_length = String.length new_s in
    if new_length = initial_length
    then new_s
    else remove_reactions new_s (* Keep removing reactions until they're all gone *)

let part1 (input : string) : int =
    remove_reactions input |> String.length

let remove_all_of_type (s: string) ~t:(t:char): string =
    String.to_list s |>
    List.filter ~f:(fun x -> not (x = t || x = Char.uppercase t || x = Char.lowercase t)) |>
    String.of_char_list

let part2 (input : string) : int =
    let all_characters_in_input = String.to_list input |> Set.of_list (module Char) |> Set.to_list in
    List.map all_characters_in_input ~f:(fun t -> remove_all_of_type input ~t:t |> remove_reactions |> String.length) |>
    List.min_elt ~compare:Int.compare |>
    Option.value ~default:(-1)

let () =
  ExtLib.print (part1 input);
  ExtLib.print (part2 input);
;;
