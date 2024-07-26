type foo = [ `Foo of foo ]
type foo_bar = [ `Foo of foo_bar | `Bar ]

module M : sig
  (* INVARIANT: t is positive *)
  type t = private int
  val mk : int -> t
end = struct
  type t = int
  let mk x =
    if x > 0 then x else raise (Invalid_argument "negative")
end

let f x = (x : int :> M.t)