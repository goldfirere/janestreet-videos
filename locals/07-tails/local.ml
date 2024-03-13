let lid : local_ 'a -> local_ 'a = fun x -> x

(* count the number of occurrences of the first element of a list *)
let count_firsts : local_ 'a list -> int = function
  | [] -> 0
  | x :: xs ->
    let rec count_occs : local_ 'a list -> int = function
      | [] -> 1
      | y :: ys when y = x -> let local_ zs = lid ys in 1 + count_occs zs
      | _ :: (* regional *) ys -> count_occs ys
    in
    count_occs xs [@nontail]

let _ = Printf.printf "%d\n" (count_firsts [3; 1; 3; 8; 3; 3; 4])

(* A local value does not escape its region. *)
(* Every function and every loop body defines a region. *)
(* exclave_ ends a region early. *)
(* A function's region ends right *before* its tail call (if any) *)
(* We can mark a tail call as a non tail-call with [@nontail] *)
(* Inference: prefer local parameters; prefer global result *)
(* ints mode-cross: they are always global *)

(* "local" and "global" are *modes*. *)
(* global < local: a global expression can always be used in a local context *)
(* "local_" on a parameter is a promise about the implementation of a function,
   *not* a requirement at usage sites *)
