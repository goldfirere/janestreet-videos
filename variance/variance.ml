type foo = [ `Foo ]
type foo_bar = [ `Foo | `Bar ]

let f x = (x : foo list :> foo_bar list)

(* This is an example of *contravariance* *)
let higher_order (f : foo_bar -> 'a) = (f : foo_bar -> 'a :> foo -> 'a) `Foo

let _ = higher_order (function `Foo -> 5 | `Bar -> 6)

(* let g x = (x : foo_bar ref :> foo ref) *)

module M : sig
  type (-'a, +'b) t
end = struct
  type ('a, 'b) t = 'a -> 'b
end

let g x = (x : (foo_bar, foo) M.t :> (foo, foo_bar) M.t)