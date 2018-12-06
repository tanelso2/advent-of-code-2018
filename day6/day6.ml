open Core

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  filtered_data


let line_pattern = "(\\d+), (\\d+)"
let line_re = Re.Perl.compile_pat line_pattern

type point = {
    id: int;
    x: int;
    y: int;
}

let parse_line_exn (line_num: int) (line: string): point =
    let matches = Re.exec line_re line |> Re.get_all in
    match matches with
    | [| _; x; y; |] -> {
        x = Int.of_string x;
        y = Int.of_string y;
        id = line_num;
    }
    | _ -> failwith "Line did not parse. That shouldn't happen"

let input = read_file input_file |> List.mapi ~f:parse_line_exn

let get_distance ({x = px; y = py; _}: point) (x: int) (y: int): int =
    let max_x = max x px in
    let max_y = max y py in
    let min_x = min x px in
    let min_y = min y py in
    (max_x - min_x) + (max_y - min_y)

let find_nearest (points: point list) (x: int) (y: int): int option =
    let f ({id; _} as pt) = (id, get_distance pt x y) in
    let distances = List.map ~f:f points in
    let min_dist = List.min_elt ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2) distances in
    match min_dist with
    | None -> None
    | Some (_, m) ->
        let closest_points = List.filter ~f:(fun (_, d) -> m = d) distances in
        match closest_points with
        | [(id, _)] -> Some id
        | _ -> None

type pointsCounts = (int, int, Int.comparator_witness) Map.t

let get_points_count (start: int) (fin: int) (points: point list): pointsCounts =
    let r = List.range start fin in
    List.cartesian_product r r |>
    List.map ~f:(fun (x, y) -> find_nearest points x y) |>
    List.filter ~f:Option.is_some |>
    List.map ~f:(fun x -> Option.value_exn x) |> (* value_exn should not fail *)
    List.sort ~compare:Int.compare |>
    List.group ~break:(<>) |>
    List.map ~f:(fun x -> (List.hd_exn x, List.length x)) |>
    Map.of_alist_exn (module Int)

let get_noninfinite_areas (points: point list): pointsCounts =
    let first_go = get_points_count 0 500 points in
    let second_go = get_points_count (-10) 510 points in
    let f ~key:k ~data:v1 =
        match Map.find second_go k with
        | None -> None
        | Some v2 -> if v2 > v1
                     then None
                     else (Some v1)
    in
    Map.mapi ~f:f first_go |>
    Map.filter ~f:Option.is_some |>
    Map.map ~f:(fun x -> Option.value_exn x)

let max_by (m: ('k, 'v, _) Map.t) ~key_fn:(f:('v -> int)): ('k * 'v) option =
  let elem = ref None in
  let max_val = ref Int.min_value in
  let iter_fn ~key:k ~data:v =
    let curr_val = f v in
    if curr_val > !max_val
    then
      (max_val := curr_val;
       elem := (Some (k, v)))
    else ()
  in
  let () = Map.iteri m ~f:iter_fn in
  !elem

let part1 (input : point list) : int =
    get_noninfinite_areas input |>
    max_by ~key_fn:ident |>
    function
    | Some (_, v) -> v
    | None -> failwith "no"

let safe_point (points: point list) (x: int) (y: int): bool =
    List.map ~f:(fun pt -> get_distance pt x y) points |>
    List.fold_left ~init:0 ~f:(+) |>
    (>) 10000

let part2 (input : point list) : int =
    let r = List.range (-500) 500 in
    List.cartesian_product r r |>
    List.filter ~f:(fun (x, y) -> safe_point input x y) |>
    List.length

let () =
  ExtLib.print (part1 input);
  ExtLib.print (part2 input);
;;
