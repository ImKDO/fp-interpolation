type point =
  { x : float
  ; y : float
  }

module Point = struct
  type t = point

  let make x y = { x; y }
  let to_tuple p = p.x, p.y
  let compare_by_x a b = Float.compare a.x b.x
  let to_string p = Printf.sprintf "%g %g" p.x p.y
end

module Dataset = struct
  exception Parse_error of string

  let sanitize line =
    let buf = Bytes.of_string line in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | ',' | ';' | '\t' -> Bytes.set buf i ' '
      | _ -> ()
    done;
    Bytes.to_string buf

  let tokens line =
    line
    |> sanitize
    |> String.split_on_char ' '
    |> List.filter_map (fun raw ->
           let trimmed = String.trim raw in
           if trimmed = "" then None else Some trimmed)

  let parse_line line =
    match tokens line with
    | [] -> None
    | [ sx; sy ] ->
        (try
           let x = float_of_string sx in
           let y = float_of_string sy in
           Some { x; y }
         with Failure _ ->
           raise (Parse_error (Printf.sprintf "Неверные числа: %S" line)))
    | _ ->
        raise
          (Parse_error
             (Printf.sprintf "Ожидалось две колонки (x y), получено: %S" line))

  let ensure_sorted prev curr =
    if Float.compare curr.x prev.x <= 0 then
      invalid_arg
        (Printf.sprintf
           "Точки должны идти по возрастанию x (нарушение: %g после %g)"
           curr.x
           prev.x)
end

module Sliding_window = struct
  type t =
    { capacity : int
    ; length : int
    ; data : point list
    }

  let create capacity =
    if capacity <= 0 then
      invalid_arg "Sliding_window.create: capacity must be positive";
    { capacity; length = 0; data = [] }

  let length t = t.length
  let is_empty t = t.length = 0

  let rec drop n lst =
    if n <= 0 then lst
    else
      match lst with
      | [] -> []
      | _ :: tl -> drop (n - 1) tl

  let push t p =
    let data = t.data @ [ p ] in
    let length = t.length + 1 in
    if length <= t.capacity then
      { t with data; length }
    else
      let excess = length - t.capacity in
      { t with data = drop excess data; length = t.capacity }

  let take_last t count =
    if count <= 0 then
      invalid_arg "Sliding_window.take_last: count must be positive";
    if count > t.length then
      None
    else
      let skip = t.length - count in
      Some (drop skip t.data)

  let rec last_exn = function
    | [] -> invalid_arg "Sliding_window.last_exn: empty window"
    | [ x ] -> x
    | _ :: tl -> last_exn tl
end

module Algorithm = struct
  type spec =
    { name : string
    ; arity : int
    ; evaluate : point array -> float -> float
    }

  type kind =
    | Linear
    | Newton of int

  let ensure_distinct dx context =
    if Float.abs dx < Float.epsilon then
      invalid_arg (Printf.sprintf "%s: duplicate x values" context)

  let linear_spec =
    let evaluate pts x =
      let p0 = pts.(0) in
      let p1 = pts.(1) in
      let dx = p1.x -. p0.x in
      ensure_distinct dx "linear interpolation";
      let t = (x -. p0.x) /. dx in
      p0.y +. t *. (p1.y -. p0.y)
    in
    { name = "linear"; arity = 2; evaluate }

  let newton_spec order =
    if order < 2 then
      invalid_arg "newton interpolation requires at least two points";
    let evaluate pts x =
      let n = Array.length pts in
      if n <> order then
        invalid_arg
          (Printf.sprintf "newton[%d]: expected %d points, got %d" order order n);
      let coeffs = Array.init n (fun i -> pts.(i).y) in
      for j = 1 to n - 1 do
        for i = n - 1 downto j do
          let denom = pts.(i).x -. pts.(i - j).x in
          ensure_distinct denom "newton interpolation";
          coeffs.(i) <- (coeffs.(i) -. coeffs.(i - 1)) /. denom
        done
      done;
      let acc = ref coeffs.(n - 1) in
      for i = n - 2 downto 0 do
        acc := !acc *. (x -. pts.(i).x) +. coeffs.(i)
      done;
      !acc
    in
    { name = Printf.sprintf "newton[%d]" order; arity = order; evaluate }

  let spec = function
    | Linear -> linear_spec
    | Newton n -> newton_spec n

  let name kind = (spec kind).name
  let arity kind = (spec kind).arity

  let compute kind points x =
    let spec = spec kind in
    if Array.length points <> spec.arity then
      invalid_arg
        (Printf.sprintf
           "%s: expected %d points, got %d"
           spec.name
           spec.arity
           (Array.length points));
    spec.evaluate points x

  let compute_with_list kind points_list x =
    compute kind (Array.of_list points_list) x

  let parse_kind raw =
    let trimmed = String.trim raw in
    if trimmed = "" then
      invalid_arg "parse_kind: empty string"
    else if String.equal trimmed "linear" then
      Linear
    else if String.starts_with ~prefix:"newton" trimmed then
      let payload = String.sub trimmed 6 (String.length trimmed - 6) in
      let order =
        match String.split_on_char ':' payload with
        | "" :: value :: _ -> int_of_string value
        | value :: _ when value <> "" -> int_of_string value
        | _ -> invalid_arg "parse_kind: use newton:order (e.g. newton:4)"
      in
      Newton order
    else
      invalid_arg (Printf.sprintf "parse_kind: unknown method %S" raw)

  let parse_list raw =
    raw
    |> String.split_on_char ','
    |> List.filter_map (fun token ->
           let trimmed = String.trim token in
           if trimmed = "" then None else Some (parse_kind trimmed))
end

module Runner = struct
  type t =
    { kind : Algorithm.kind
    ; step : float
    ; cursor : float option
    ; window : Sliding_window.t
    }

  let create kind ~step =
    if step <= 0. then
      invalid_arg "Runner.create: step must be positive";
    let window = Sliding_window.create (Algorithm.arity kind) in
    { kind; step; cursor = None; window }

  let eps = 1e-9

  let next_cursor t points =
    match t.cursor with
    | Some cursor -> cursor
    | None ->
        (match points with
         | p :: _ -> p.x
         | [] -> invalid_arg "next_cursor: empty points list")

  let last_point points = Sliding_window.last_exn points

  let produce_until kind step points cursor upper =
    let rec loop acc x =
      if x > upper +. eps then
        List.rev acc, x
      else
        let y = Algorithm.compute_with_list kind points x in
        loop ({ x; y } :: acc) (x +. step)
    in
    loop [] cursor

  let feed t point =
    let window = Sliding_window.push t.window point in
    match Sliding_window.take_last window (Algorithm.arity t.kind) with
    | None -> [], { t with window }
    | Some points ->
        let cursor = next_cursor t points in
        let upper = (last_point points).x in
        if cursor > upper +. eps then
          [], { t with window }
        else
          let produced, cursor' =
            produce_until t.kind t.step points cursor upper
          in
          produced, { t with window; cursor = Some cursor' }

  let flush t =
    if Sliding_window.is_empty t.window then
      [], t
    else
      match Sliding_window.take_last t.window (Algorithm.arity t.kind) with
      | None -> [], t
      | Some points ->
          let cursor =
            match t.cursor with
            | Some c -> c
            | None -> (match points with p :: _ -> p.x | [] -> 0.)
          in
          let upper = (last_point points).x in
          if cursor > upper +. eps then
            [], t
          else
            let produced, cursor' =
              produce_until t.kind t.step points cursor upper
            in
            produced, { t with cursor = Some cursor' }
end
