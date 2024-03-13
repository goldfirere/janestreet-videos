let rec append_ints : local_ int list -> int list -> int list = fun xs ys ->
  match xs with
  | [] -> ys
  | x :: xs -> x :: append_ints xs ys

let _ = List.map (append_ints [1;2;3]) [ [4;5]; [6;7] ]

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
