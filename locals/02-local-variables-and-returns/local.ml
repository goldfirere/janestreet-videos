let string_ref : string ref = ref "something"

let ignore : local_ 'a -> unit = fun _ -> ()

let greet : local_ string -> local_ string list = fun z -> exclave_ ["Hello,"; z]

let f () =
  let local_ x = "you" in
  let local_ y = "me" in
  let local_ greet_x = greet x in
  let local_ greet_y = greet y in
  (* scary imperative code *)
  ignore (x, y, greet_x, greet_y);
  ()

(* A local value does not escape its region. *)
(* Every function defines a region. *)
(* exclave_ ends a region early. *)
(* Return positions (also called "tail positions") are treated specially: watch out! *)