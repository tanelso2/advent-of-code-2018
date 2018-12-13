open Core

let input_file = "input.txt"

let read_file (filename : string) : int list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_all chan in
  String.split ~on:' ' raw_data |>
  List.map ~f:(fun x -> String.strip x |> Int.of_string)

let input_list = read_file input_file

type node =
  | Node of (node list * int list)

let construct_tree (input: int list): node =
  let l = ref input in
  let rec construct () =
    match !l with
    | num_children::num_datum::rest ->
      l := rest;
      let children =
        List.range 0 num_children |>
        List.map ~f:(fun _ -> construct ())
      in
      let (datum, r) = List.split_n !l num_datum in
      l := r;
      (Node (children, datum))
    | _ -> Node ([], [])
  in
  construct ()

let input = construct_tree input_list

let part1 (input: node): int =
  let rec helper (Node (children, datum)) =
    let sum = List.fold ~init:0 ~f:(+) datum in
    let child_sum = List.map children ~f:helper |> List.fold ~init:0 ~f:(+) in
    sum + child_sum
  in
  helper input

let part2 (input: node): int =
  let rec helper (Node (children, datum)) =
    let size = List.length children in
    if size = 0
    then List.fold ~init:0 ~f:(+) datum
    else List.map datum ~f:(fun x -> List.nth children (x - 1)) |>
         List.filter_opt |>
         List.map ~f:(fun x -> helper x) |>
         List.fold ~init:0 ~f:(+)
  in
  helper input

let () =
  ExtLib.print (part1 input);
  ExtLib.print (part2 input);
;;
