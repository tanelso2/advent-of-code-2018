open Core

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  filtered_data


let line_pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin"
let line_re = Re.Perl.compile_pat line_pattern

type depends = | Depends of (string * string)

let parse_line_exn (_: int) (line: string): depends =
  let matches = Re.exec line_re line |> Re.get_all in
  match matches with
  | [| _; x; y; |] -> Depends (x, y)
  | _ -> failwith "Line did not parse. That shouldn't happen"

let input = read_file input_file |> List.mapi ~f:parse_line_exn

type dependencyMap = (string, string list, String.comparator_witness) Map.t

let seed_empty_map (): dependencyMap =
    let range = List.range ~stop:`inclusive (Char.to_int 'A') (Char.to_int 'Z') in
    let int_to_char_string x = Char.of_int_exn x |> Char.to_string in
    List.fold range ~init:(Map.empty (module String)) ~f:(fun m c -> Map.add_exn m ~key:(int_to_char_string c) ~data:[])

let build_dependency_map (input: depends list): dependencyMap =
    let f m (Depends (a, b)) =
        Map.add_multi m ~key:b ~data:a
    in
    List.fold_left input ~init:(seed_empty_map ()) ~f:f

let dependencies_done (complete: string list) (dependencies: string list): bool =
    List.filter dependencies ~f:(fun x -> List.mem complete ~equal:(=) x |> not) |>
    List.length |>
    (=) 0

let get_next_to_process (complete: string list) (dep_map: dependencyMap): string option =
    Map.filter dep_map ~f:(dependencies_done complete) |>
    Map.keys |>
    List.filter ~f:(fun x -> List.mem ~equal:(=) complete x |> not) |>
    List.hd

let part1 (_ : depends list) : string =
    let dep_map = build_dependency_map input in
    let rec helper complete =
        match get_next_to_process complete dep_map with
        | Some x -> helper (x::complete)
        | None -> complete
    in
    helper [] |>
    String.concat |>
    String.rev

let part2 (_ : depends list) : int = 0

let () =
  ExtLib.print (part1 input);
  ExtLib.print (part2 input);
;;
