open Core
open ExtLib
module Map = Core.Map.Poly

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter (fun x -> String.equal "" x |> not) raw_data in
  filtered_data

type elf_claim = {
  id: int;
  start_point: (int * int);
  size: (int * int);
}

let claim_pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
let claim_re = Re.Posix.compile_pat claim_pattern

let parse_claim (line: string): elf_claim =
  let matches = Re.exec claim_re line in
  let id = Re.get matches 1 |> Int.of_string in
  let start_x = Re.get matches 2 |> Int.of_string in
  let start_y = Re.get matches 3 |> Int.of_string in
  let size_w = Re.get matches 4 |> Int.of_string in
  let size_h = Re.get matches 5 |> Int.of_string in
  { id;
    start_point = (start_x, start_y);
    size = (size_w, size_h);
  }

let input = read_file input_file |> List.map parse_claim

let get_all_squares_covered (claim: elf_claim): (int * int) list =
  let { start_point = (start_x, start_y); size = (size_w, size_h); _ } = claim in
  let x_range =  Core.List.range start_x (start_x + size_w) in
  let y_range = Core.List.range start_y (start_y + size_h) in
  Core.List.cartesian_product x_range y_range

type squaresCountMap = ((int * int), int) Map.t

let get_squares_count (counter : squaresCountMap) (claim : elf_claim) : squaresCountMap =
  let squares_covered = get_all_squares_covered claim in
  let change_fn x =
    match x with
    | Some y -> Some (y + 1)
    | None -> Some 1
  in
  Core.List.fold ~init:counter ~f:(fun counter x -> Map.change counter x ~f:change_fn) squares_covered

let part1 (input: elf_claim list) : int =
  let squares_count = Core.List.fold ~init:Map.empty ~f:get_squares_count input in
  Map.filter ~f:(fun x -> x > 1) squares_count |>
  Map.length

let has_overlapping_squares (squares_count : squaresCountMap) (claim : elf_claim): bool =
  let squares_covered = get_all_squares_covered claim in
  let pred x = (Map.find_exn squares_count x) = 1 in
  Core.List.for_all ~f:pred squares_covered

let part2 (input : elf_claim list): int =
  let squares_count = Core.List.fold ~init:Map.empty ~f:get_squares_count input in
  let filtered = Core.List.filter ~f:(has_overlapping_squares squares_count) input in
  let head = Core.List.hd_exn filtered in
  head.id

let () =
  print (part1 input);
  print (part2 input);
;;
