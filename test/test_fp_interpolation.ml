open Fp_interpolation
open Methods

module Algo = Methods.Algorithm
module Dataset = Methods.Dataset
module Runner = Methods.Runner
module Point = Methods.Point

let float_equal ?(eps = 1e-9) a b = Float.abs (a -. b) <= eps
let float_pp fmt v = Format.fprintf fmt "%.12g" v
let float_testable = Alcotest.testable float_pp (float_equal ~eps:1e-9)

let point_pp fmt p = Format.fprintf fmt "(%g, %g)" p.x p.y
let point_equal a b = float_equal a.x b.x && float_equal a.y b.y
let point_testable = Alcotest.testable point_pp point_equal
let option_point_testable = Alcotest.option point_testable

let expect_invalid_argument label f =
  match f () with
  | () -> Alcotest.failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()
  | exception exn ->
      Alcotest.failf "%s: expected Invalid_argument, got %s" label
        (Printexc.to_string exn)

let expect_parse_error label f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Dataset.Parse_error" label
  | exception Dataset.Parse_error _ -> ()
  | exception exn ->
      Alcotest.failf "%s: expected Dataset.Parse_error, got %s" label
        (Printexc.to_string exn)

let dataset_parses_valid () =
  let cases =
    [ "0 1", Point.make 0. 1.
    ; "  2.5\t3.5 ", Point.make 2.5 3.5
    ; "4,5", Point.make 4. 5.
    ; "6;7", Point.make 6. 7.
    ]
  in
  List.iter
    (fun (line, expected) ->
      let actual =
        match Dataset.parse_line line with
        | Some point -> point
        | None -> Alcotest.failf "line %S produced None" line
      in
      Alcotest.check point_testable (Printf.sprintf "parsed %S" line) expected
        actual)
    cases

let dataset_ignores_blank_lines () =
  Alcotest.check option_point_testable "blank lines are ignored" None
    (Dataset.parse_line "   ")

let dataset_rejects_malformed_lines () =
  expect_parse_error "malformed dataset row" (fun () ->
      ignore (Dataset.parse_line "oops"))

let dataset_enforces_ordering () =
  let p0 = Point.make 0. 0. in
  let p1 = Point.make 1. 1. in
  Dataset.ensure_sorted p0 p1;
  expect_invalid_argument "unsorted dataset" (fun () ->
      Dataset.ensure_sorted p1 p0)

let linear_interpolation () =
  let points = [ Point.make 0. 0.; Point.make 4. 8. ] in
  let actual = Algo.compute_with_list Algo.Linear points 1.5 in
  Alcotest.check float_testable "linear interpolation" 3. actual

let newton_interpolation () =
  let points =
    [ Point.make 0. 1.
    ; Point.make 1. 2.
    ; Point.make 2. 5.
    ]
  in
  let actual = Algo.compute_with_list (Algo.Newton 3) points 1.5 in
  let expected = (1.5 *. 1.5) +. 1. in
  Alcotest.check float_testable "newton interpolation" expected actual

let runner_streaming () =
  let runner = Runner.create Algo.Linear ~step:0.5 in
  let p0 = Point.make 0. 0. in
  let p1 = Point.make 1. 1. in
  let p2 = Point.make 2. 2. in
  let outputs, runner = Runner.feed runner p0 in
  Alcotest.check (Alcotest.list point_testable)
    "no output until enough points" [] outputs;
  let outputs, runner = Runner.feed runner p1 in
  let expected_chunk1 =
    [ Point.make 0. 0.
    ; Point.make 0.5 0.5
    ; Point.make 1. 1.
    ]
  in
  Alcotest.check (Alcotest.list point_testable) "first chunk"
    expected_chunk1 outputs;
  let outputs, _runner = Runner.feed runner p2 in
  let expected_chunk2 =
    [ Point.make 1.5 1.5
    ; Point.make 2. 2.
    ]
  in
  Alcotest.check (Alcotest.list point_testable) "second chunk"
    expected_chunk2 outputs

let dataset_tests =
  [ Alcotest.test_case "parses valid lines" `Quick dataset_parses_valid
  ; Alcotest.test_case "ignores blank lines" `Quick dataset_ignores_blank_lines
  ; Alcotest.test_case "rejects malformed lines" `Quick
      dataset_rejects_malformed_lines
  ; Alcotest.test_case "enforces ordering" `Quick dataset_enforces_ordering
  ]

let interpolation_tests =
  [ Alcotest.test_case "linear" `Quick linear_interpolation
  ; Alcotest.test_case "newton" `Quick newton_interpolation
  ]

let runner_tests =
  [ Alcotest.test_case "streaming" `Quick runner_streaming ]

let () =
  Alcotest.run "fp-interpolation"
    [ "Dataset", dataset_tests
    ; "Interpolation", interpolation_tests
    ; "Runner", runner_tests
    ]
