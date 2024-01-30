let init : 'a. int -> local_ (int -> local_ 'a) -> local_ 'a list = fun len f ->
  exclave_
  let rec local_ loop : int -> local_ 'a list -> local_ 'a list = fun n (local_ acc) ->
    if n < 0
    then acc
    else exclave_ loop (n-1) (f n :: acc)
  in
  loop (len-1) []

let rec iter : 'a. local_ (local_ 'a -> unit) -> local_ 'a list -> unit = fun f -> function
  | [] -> ()
  | x :: xs -> f x; iter f xs

let do_it () =
  let count = ref 0 in
  for i = 1 to 10000 do
    let len = Random.int 10 in
    let local_ list = init len (fun _ -> Random.int 100) in
    iter (fun n -> if n mod 5 = 0 then count := !count + 1) list
  done;
  Format.printf "%d\n" !count

let () =
  Random.init 12345;
  do_it ()

(* A local value does not escape its region. *)
(* Every function and every loop body defines a region. *)
(* exclave_ ends a region early. *)
(* Return positions (also called "tail positions") are treated specially: watch out! *)
(* Inference: prefer local parameters; prefer global result *)
(* ints mode-cross: they are always global *)

(* "local" and "global" are *modes*. *)
(* global < local: a global expression can always be used in a local context *)
(* "local_" on a parameter is a promise about the implementation of a function,
   *not* a requirement at usage sites *)
