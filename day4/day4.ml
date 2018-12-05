open Core

let input_file = "input.txt"

let read_file (filename : string) : string list =
  let chan = Core.In_channel.create filename in
  let raw_data = ExtLib.input_list chan in
  let filtered_data : string list = List.filter ~f:(fun x -> String.equal "" x |> not) raw_data in
  filtered_data

let line_pattern = "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] (.*)"
let line_re = Re.Perl.compile_pat line_pattern

type guard_action =
  | FallsAsleep
  | WakesUp
  | ShiftChange of int

let shift_change_pattern = "Guard #(\\d+) begins shift$"
let shift_change_re = Re.Perl.compile_pat shift_change_pattern

let parse_guard_action_exn (action: string): guard_action =
  let open ExtLib.String in
  if exists action "wakes up"then WakesUp
  else if exists action "falls asleep" then FallsAsleep
  else if exists action "begins shift" then
    let guard_num = Re.exec shift_change_re action |>
                    (fun x -> Re.get x 1) |>
                    Int.of_string
    in
    (ShiftChange guard_num)
  else failwith "That's not a real guard action!"

type guard_event = {
  year: int;
  month: int;
  day: int;
  hour: int;
  minute: int;
  action: guard_action;
}

let parse_line_exn (line: string): guard_event =
  let matches = Re.exec line_re line |> Re.get_all in
  match matches with
  | [| _; year; month; day; hour; minute; action |] ->
    {
      year = Int.of_string year;
      month = Int.of_string month;
      day = Int.of_string day;
      hour = Int.of_string hour;
      minute = Int.of_string minute;
      action = parse_guard_action_exn action;
    }
  | _ -> failwith "Line did not parse. That shouldn't happen"

let input: guard_event list =
  let unsorted_input = read_file input_file |> List.map ~f:parse_line_exn in
  (* This is liable to breakage. But at least I don't have to write a comparison function *)
  List.sort ~compare:Polymorphic_compare.compare unsorted_input

type sleepPeriodList = (int * int) list
type sleepingPeriodsMap = (int, sleepPeriodList, Int.comparator_witness) Map.t

let add_sleeping_period (sleeping_periods_map: sleepingPeriodsMap) (period: (int * int)) (guard_id: int) =
  let change_fn result =
    match result with
    | Some l -> (Some (period::l))
    | None -> (Some [period])
  in
  Map.change sleeping_periods_map guard_id ~f:change_fn

type guard_status =
  | Asleep of int
  | Awake

type processing_state = {
  current_guard: int;
  status: guard_status;
  sleeping_periods_map: sleepingPeriodsMap;
}

let get_sleeping_periods (input: guard_event list): sleepingPeriodsMap =
  let fold_fn {current_guard; status; sleeping_periods_map} {minute; action; _} =
    match action with
    | FallsAsleep -> {current_guard; sleeping_periods_map; status = (Asleep minute);}
    | WakesUp ->
      (match status with
       | Asleep start -> {
           current_guard;
           status = Awake;
           sleeping_periods_map = add_sleeping_period sleeping_periods_map (start, minute) current_guard;
         }
       | Awake -> {current_guard; sleeping_periods_map; status} (* No change. Probably shouldn't happen *)
      )
    | ShiftChange new_guard -> {
        current_guard = new_guard;
        sleeping_periods_map;
        status = Awake;
      }
  in
  let init = {current_guard = -1; status = Awake; sleeping_periods_map = (Map.empty (module Int))} in
  (List.fold_left ~init:init ~f:fold_fn input).sleeping_periods_map

let calculate_time_spent_asleep (periods: sleepPeriodList): int =
  List.fold ~init:0 ~f:(fun total (a, b) -> total + (b - a)) periods

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

let get_biggest_sleeper (sleeping_periods_map: sleepingPeriodsMap): (int * sleepPeriodList) =
  match max_by sleeping_periods_map ~key_fn:calculate_time_spent_asleep with
  | Some x -> x
  | None -> failwith "There should be at least one sleeper..."

let calculate_times_sleeping_each_minute (periods: sleepPeriodList): (int, int, Int.comparator_witness) Map.t =
  let change_fn x = 1 + (Option.value x ~default:0) |> Option.some in
  let fold_fn accum (a, b) =
    let r = List.range a b in
    List.fold ~init:accum ~f:(fun accum x -> Map.change accum x ~f:change_fn) r
  in
  List.fold ~init:(Map.empty (module Int)) ~f:fold_fn periods

let calculate_minute_most_slept (periods: sleepPeriodList): (int * int) =
  let times_sleeping = calculate_times_sleeping_each_minute periods in
  match max_by times_sleeping ~key_fn:ident with
  | Some x -> x
  | None -> failwith "Gah critical failure"

let part1 (input : guard_event list) : int =
  let sleeping_periods = get_sleeping_periods input in
  let (guard_id, periods) = get_biggest_sleeper sleeping_periods in
  let (minute_most_slept, _) = calculate_minute_most_slept periods in
  guard_id * minute_most_slept

let part2 (input : guard_event list) : int =
  let sleeping_periods = get_sleeping_periods input in
  let guards_to_minutes_most_slept = Map.map sleeping_periods ~f:calculate_minute_most_slept in
  let (guard_id, (minute_most_slept, _)) =
    match max_by guards_to_minutes_most_slept ~key_fn:(fun (_, y) -> y) with
    | Some x -> x
    | None -> failwith "Too lazy to handle failure"
  in
  guard_id * minute_most_slept

let () =
  ExtLib.print (part1 input);
  ExtLib.print (part2 input);
;;
