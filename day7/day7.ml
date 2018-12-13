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

let get_available_to_process (complete: string list) (dep_map: dependencyMap): string list =
    Map.filter dep_map ~f:(dependencies_done complete) |>
    Map.keys |>
    List.filter ~f:(fun x -> List.mem ~equal:(=) complete x |> not)

let get_next_to_process (complete: string list) (dep_map: dependencyMap): string option =
    get_available_to_process complete dep_map |>
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

type worker = {
    processing: string;
    finish_time: int;
}

let divide_workers (workers: worker list) (current_time: int): (worker list * string list) =
    let f { finish_time; _} = finish_time <= current_time in
    let (fin, not_fin) = List.partition_tf ~f:f workers in
    let fin_letters = List.map fin ~f:(fun {processing; _} -> processing) in
    (not_fin, fin_letters)

let get_new_letters (num: int) (in_progress: string list) (complete: string list) (dep_map: dependencyMap): string list =
    get_available_to_process complete dep_map |>
    List.filter ~f:(fun x -> List.mem ~equal:(=) in_progress x |> not) |>
    (fun y -> List.take y num)


let max_workers = 5
let buffer = 60

let get_finish_time (current_time: int) (letter: string): int =
    let c_int = Char.of_string letter |> Char.to_int in
    let a_int = Char.to_int 'A' in
    (c_int - a_int + 1 + buffer) + current_time


let get_new_workers (time: int) (letters: string list): worker list =
    let f letter =
        {
            processing = letter;
            finish_time = (get_finish_time time letter);
        }
    in
    List.map letters ~f:f


let part2 (_ : depends list) : int =
    let dep_map = build_dependency_map input in
    let rec step (time: int) (workers: worker list) (complete: string list): int =
        let (not_fin, fin) = divide_workers workers time in
        let num_workers = max_workers - (List.length not_fin) in
        let curr_complete = List.append fin complete in
        let in_progress = List.map not_fin ~f:(fun {processing; _} -> processing) in
        let new_letters = get_new_letters num_workers in_progress curr_complete dep_map in
        let new_workers = get_new_workers time new_letters in
        let curr_workers = List.append not_fin new_workers in
        if curr_workers = []
        then time
        else step (time + 1) curr_workers curr_complete
    in
    step 0 [] []

let () =
  ExtLib.print (part1 input);
  ExtLib.print (part2 input);
;;
