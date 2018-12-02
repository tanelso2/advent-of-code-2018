open Core
open ExtLib

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter (fun x -> String.equal "" x |> not) raw_data in
  filtered_data

let input = read_file input_file

let get_char_counts (s: string): (char * int) list =
  let module S = Core.String in
  let module L = Core.List in
  S.to_list s |>
  L.sort ~compare:(Core.Char.compare) |>
  L.group ~break:(<>) |>
  L.map ~f:(fun x -> (L.hd_exn x, L.length x))

let char_count_has_count ~n (char_counts: (char * int) list) : bool =
  Core.List.exists char_counts ~f:(fun (_, x) -> x = n)

let part1 (input: string list) : int =
  let char_counts = List.map get_char_counts input in
  let group2 = Core.List.count ~f:(char_count_has_count ~n:2) char_counts in
  let group3 = Core.List.count ~f:(char_count_has_count ~n:3) char_counts in
  group2 * group3

let get_number_of_different_letters (a : string) (b : string) : int =
  let module L = Core.List in
  let a_list = Core.String.to_list a in
  let b_list = Core.String.to_list b in
  let zipped = L.zip a_list b_list in
  match zipped with
  | Some z -> L.count ~f:(fun (x, y) -> x <> y) z
  | None -> -1 (* Houston, we have a problem *)

let find_matching_pair (input : string list) : (string * string) option =
  let pairs = Core.List.cartesian_product input input in
  Core.List.find ~f:(fun (x, y) -> 1 = (get_number_of_different_letters x y)) pairs

let get_common_letters (a : string) (b : string) : string =
  let module L = Core.List in
  let a_list = Core.String.to_list a in
  let b_list = Core.String.to_list b in
  let zipped = L.zip a_list b_list in
  match zipped with
  | Some z ->
    L.filter ~f:(fun (x, y) -> x = y) z |>
    L.map ~f:fst |>
    Core.String.of_char_list
  | None -> "Error error" (* Houston, we have a problem *)

let part2 (input : string list): string =
  match find_matching_pair input with
  | Some (a, b) -> get_common_letters a b
  | None -> "There's been a problem here"

let () =
  print (part1 input);
  print (part2 input);
;;
