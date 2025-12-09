open Fp_interpolation

module Algo = Methods.Algorithm
module Runner = Methods.Runner
module Dataset = Methods.Dataset

type test = { name : string; run : unit -> unit }

let float_equal ?(eps = 1e-9) a b = Float.abs (a -. b) <= eps

let assert_option_some name = function
  | Some v -> v
  | None -> failwith (name ^ ": expected Some, got None")

let assert_raises_invalid_argument name f =
  try
    f ();
    failwith (name ^ ": expected Invalid_argument, but no exception was raised")
  with
  | Invalid_argument _ -> ()
  | exn ->
      failwith
        (Printf.sprintf "%s: expected Invalid_argument, got %s" name
           (Printexc.to_string exn))

let assert_dataset_parsing () =
  let check line expected_x expected_y =
    let point =
      Dataset.parse_line line |> assert_option_some ("parse " ^ line)
    in
    if not (float_equal point.x expected_x) then
      failwith
        (Printf.sprintf "parse %s: expected x=%g, got %g" line expected_x point.x);

    if not (float_equal point.y expected_y) then
      failwith
        (Printf.sprintf "parse %s: expected y=%g, got %g" line expected_y
           point.y)
  in
  check "0 1" 0. 1.;
  check "  2.5\t3.5 " 2.5 3.5;
  check "4,5" 4. 5.;
  check "6;7" 6. 7.;
  (match Dataset.parse_line "   " with
  | None -> ()
  | Some _ -> failwith "blank line should be ignored");
  (try
     ignore (Dataset.parse_line "oops");
     failwith "parse_line should reject malformed input"
   with
   | Dataset.Parse_error _ -> ()
   | exn ->
       failwith
         (Printf.sprintf "parse_line raised unexpected exception: %s"
            (Printexc.to_string exn)))

let assert_dataset_ordering () =
  let open Methods in
  let p1 = { x = 0.; y = 0. } in
  let p2 = { x = 1.; y = 1. } in
  Dataset.ensure_sorted p1 p2;
  assert_raises_invalid_argument "ensure_sorted"
    (fun () -> Dataset.ensure_sorted p2 p1)

let assert_linear_interpolation () =
  let open Methods in
  let points = [ { x = 0.; y = 0. }; { x = 4.; y = 8. } ] in
  let value =
    Algo.compute_with_list Algo.Linear points 1.5
  in
  if not (float_equal value 3.) then
    failwith
      (Printf.sprintf "linear interpolation: expected 3, got %g" value)

let assert_newton_interpolation () =
  (* Interpolate f(x) = x^2 + 1 using 3-point Newton polynomial *)
  let open Methods in
  let points =
    [ { x = 0.; y = 1. }
    ; { x = 1.; y = 2. }
    ; { x = 2.; y = 5. }
    ]
  in
  let value =
    Algo.compute_with_list (Algo.Newton 3) points 1.5
  in
  let expected = (1.5 *. 1.5) +. 1. in
  if not (float_equal value expected) then
    failwith
      (Printf.sprintf "newton interpolation: expected %g, got %g" expected
         value)

let assert_runner_stream () =
  let open Methods in
  let runner = Runner.create Algo.Linear ~step:0.5 in
  let p0 = { x = 0.; y = 0. } in
  let p1 = { x = 1.; y = 1. } in
  let p2 = { x = 2.; y = 2. } in
  let outputs, runner = Runner.feed runner p0 in
  if outputs <> [] then failwith "runner: expected no output after first point";
  let outputs, runner = Runner.feed runner p1 in
  let expected1 =
    [ { x = 0.; y = 0. }; { x = 0.5; y = 0.5 }; { x = 1.; y = 1. } ]
  in
  let compare_points a b =
    float_equal a.x b.x && float_equal a.y b.y
  in
  let assert_points label actual expected =
    if List.length actual <> List.length expected then
      failwith
        (Printf.sprintf "%s: expected %d points, got %d" label
           (List.length expected) (List.length actual));
    List.iter2
      (fun a b ->
        if not (compare_points a b) then
          failwith
            (Printf.sprintf "%s: expected (%.2f, %.2f), got (%.2f, %.2f)"
               label b.x b.y a.x a.y))
      actual expected
  in
  assert_points "runner chunk 1" outputs expected1;
  let outputs, _runner = Runner.feed runner p2 in
  let expected2 =
    [ { x = 1.5; y = 1.5 }; { x = 2.; y = 2. } ]
  in
  assert_points "runner chunk 2" outputs expected2

let tests =
  [ { name = "dataset parsing"; run = assert_dataset_parsing }
  ; { name = "dataset ordering"; run = assert_dataset_ordering }
  ; { name = "linear interpolation"; run = assert_linear_interpolation }
  ; { name = "newton interpolation"; run = assert_newton_interpolation }
  ; { name = "runner streaming"; run = assert_runner_stream }
  ]

let () =
  List.iter
    (fun test ->
      try
        test.run ();
        Printf.printf "✔ %s\n%!" test.name
      with
      | exn ->
          Printf.eprintf "✖ %s: %s\n%!" test.name (Printexc.to_string exn);
          exit 1)
    tests;
  Printf.printf "\nAll %d tests passed.\n%!" (List.length tests)
