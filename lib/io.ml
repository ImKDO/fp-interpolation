open Methods

module Algo = Methods.Algorithm
module Runner = Methods.Runner
module Dataset = Methods.Dataset

type config =
  { methods : Algo.kind list
  ; step : float
  }

type state =
  { runners : (string * Runner.t) list
  ; last_point : Methods.point option
  }

let default_config =
  { methods = [ Algo.Linear ]
  ; step = 1.0
  }

let parse_methods raw =
  try
    let parsed = Algo.parse_list raw in
    if parsed = [] then invalid_arg "--methods: список не должен быть пустым";
    parsed
  with
  | Invalid_argument msg -> raise (Arg.Bad msg)

let parse_step s =
  try
    let value = float_of_string s in
    if value <= 0. then raise (Arg.Bad "--step: значение должно быть > 0");
    value
  with
  | Failure _ ->
      raise
        (Arg.Bad
           (Printf.sprintf "--step: невозможно прочитать число из %S" s))

let parse_args () =
  let methods_arg = ref None in
  let step_arg = ref None in
  let usage =
    "Использование: fp-interpolation [--methods linear,newton:4] [--step 0.5]"
  in
  let set_methods s = methods_arg := Some (parse_methods s) in
  let set_step s = step_arg := Some (parse_step s) in
  let speclist =
    [ ("--methods", Arg.String set_methods, "Методы через запятую (linear,newton:k)")
    ; ("-m", Arg.String set_methods, "Синоним --methods")
    ; ("--step", Arg.String set_step, "Шаг дискретизации результатов (> 0)")
    ; ("-s", Arg.String set_step, "Синоним --step")
    ]
  in
  let anon arg =
    raise
      (Arg.Bad (Printf.sprintf "Неизвестный позиционный аргумент: %S" arg))
  in
  Arg.parse speclist anon usage;
  let methods =
    match !methods_arg with
    | Some ms -> ms
    | None -> default_config.methods
  in
  let step =
    match !step_arg with
    | Some s -> s
    | None -> default_config.step
  in
  { methods; step }

let report_config config =
  let methods =
    config.methods |> List.map Algo.name |> String.concat ", "
  in
  Printf.eprintf "Методы: %s\nШаг: %.6g\n%!" methods config.step

let emit label (p : Methods.point) =
  Printf.printf "%s: %.10g %.10g\n%!" label p.x p.y

let update_runner label runner point =
  let outputs, runner' = Runner.feed runner point in
  List.iter (emit label) outputs;
  label, runner'

let flush_runner label runner =
  let outputs, runner' = Runner.flush runner in
  List.iter (emit label) outputs;
  label, runner'

let integrate_point state point =
  (match state.last_point with
   | Some prev -> Dataset.ensure_sorted prev point
   | None -> ());
  let runners =
    List.map (fun (label, runner) -> update_runner label runner point) state.runners
  in
  { runners; last_point = Some point }

let rec stream state =
  match input_line stdin with
  | line ->
      let next_state =
        match Dataset.parse_line line with
        | None -> state
        | Some point -> integrate_point state point
      in
      stream next_state
  | exception End_of_file -> state

let flush_all state =
  let runners = List.map (fun (label, runner) -> flush_runner label runner) state.runners in
  { state with runners }

let build_initial_state config =
  let runners =
    List.map
      (fun kind ->
        let runner = Runner.create kind ~step:config.step in
        Algo.name kind, runner)
      config.methods
  in
  { runners; last_point = None }


let run_pipeline config =

  let state = config |> build_initial_state |> stream |> flush_all in

  ignore state



let run () =
  let config =
    try parse_args () with

    | Arg.Bad msg ->

        prerr_endline msg;

        exit 1

  in

  report_config config;

  try run_pipeline config with

  | Dataset.Parse_error msg ->

      prerr_endline msg;

      exit 2

  | Invalid_argument msg ->

      prerr_endline msg;

      exit 2

let start = run
