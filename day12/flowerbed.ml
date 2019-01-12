open Core

type flowerbed = (int, Int.comparator_witness) Set.t

type t = flowerbed

type pot =
  | Empty
  | Flower

type five_pots = (pot * pot * pot * pot * pot)
type conversions = (five_pots, pot) Map.Poly.t

let get_pot_contents (plants: flowerbed) (pos: int): pot =
  if Set.mem plants pos
  then Flower
  else Empty

let list_to_5_tup (l: 'a list): ('a * 'a * 'a * 'a * 'a) =
  match l with
  | [a;b;c;d;e] -> (a,b,c,d,e)
  | _ -> failwith "Not a list of 5"

let has_flower_next (plants: flowerbed) (mapping: conversions) (pos: int): bool =
  let r = List.range (pos - 2) (pos + 2) ~stop:`inclusive in
  let flowers = List.map r ~f:(get_pot_contents plants) in
  let tup = list_to_5_tup flowers in
  match Map.find_exn mapping tup with
  | Flower -> true
  | Empty -> false

let is_flower (p: pot): bool =
  match p with
  | Flower -> true
  | Empty -> false


let get_next_flowerbed (mapping: conversions) (plants: flowerbed): flowerbed =
  let min_pot = Set.min_elt_exn plants in
  let max_pot = Set.max_elt_exn plants in
  let r = List.range (min_pot - 3) (max_pot + 3) ~stop:`inclusive in
  List.filter r ~f:(has_flower_next plants mapping) |>
  Set.of_list (module Int)

let get_num_flowers (plants: flowerbed): int = Set.length plants

let parse_pot (c: char): pot =
  match c with
  | '#' -> Flower
  | '.' -> Empty
  | _ -> failwith "Weird character. Shouldn't happen"

let initial_config_pattern = "initial state:\\s([#.]*)"
let initial_config_re = Re.Perl.compile_pat initial_config_pattern

let parse_initial_config (line: string): flowerbed =
  let matches = Re.exec initial_config_re line |> Re.get_all in
  match matches with
  | [| _; initial_list |] ->
    let mapi_fn i x =
      let pot = parse_pot x in
      if is_flower pot
      then Some i
      else None
    in
    String.to_list initial_list |>
    List.filter_mapi ~f:mapi_fn |>
    Set.of_list (module Int)
  | _ -> failwith "Initial config did not parse"

let line_pattern = "([#.]{5})\\s=>\\s([#.])"
let line_re = Re.Perl.compile_pat line_pattern

let parse_conversion_line (line: string): (five_pots * pot) =
  let matches = Re.exec line_re line |> Re.get_all in
  let (in_str, out_str) =
    match matches with
    | [| _; input; output |] -> (input, output)
    | _ -> failwith "Line did not parse. That shouldn't happen"
  in
  let input = String.to_list in_str |> List.map ~f:parse_pot |> list_to_5_tup in
  let output = String.to_list out_str |> List.hd_exn |> parse_pot in
  (input, output)

let parse_conversion_lines (lines: string list): conversions =
  List.map lines ~f:parse_conversion_line |>
  Map.Poly.of_alist_exn

let sum (plants: flowerbed): int =
  Set.fold plants ~init:0 ~f:(+)

let max (plants: flowerbed): int =
  Set.max_elt_exn plants

let min (plants: flowerbed): int =
  Set.min_elt_exn plants

module S = struct
  type t = flowerbed
  let compare a b = Set.compare_direct a b
  let sexp_of_t _ = failwith "NOIMPL"
end

include Comparator.Make(S)

