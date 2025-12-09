let methods = ref []
let step = ref 1.0

let set_methods s =
  methods := String.split_on_char ',' s

let set_step s =
  step := float_of_string s

let speclist = [
  ("--methods", Arg.String set_methods, "Список методов (через запятую)");
  ("--step", Arg.String set_step, "Шаг для метода");
]

let utf8_length s =
  s
  |> String.to_seq
  |> Seq.fold_left (fun acc c ->
       if (Char.code c land 0xC0) <> 0x80 then acc + 1 else acc
     ) 0

let make_start_phrase phrase =
  let len_phrase = utf8_length phrase in
  let char_out = "+" ^ (String.make len_phrase '-') ^ "+" in
  char_out ^ "\n" ^ "|" ^ phrase ^ "|" ^ "\n" ^ char_out

let start =
  let usage_msg = make_start_phrase "Добро пожаловать на сервер шизофрения" in
  Printf.printf "%s\n" usage_msg;
  Arg.parse speclist (fun _ -> ()) usage_msg;
  Printf.printf "Выбраны методы:\n";
  List.iter (Printf.printf "- %s\n") !methods;
  (Printf.printf "- %f\n") !step;
