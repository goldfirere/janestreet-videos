module type Mappable_minimal = sig
  type key
  type 'a t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val iteri : (key -> 'a -> unit) -> 'a t -> unit
end

module type Mappable = sig
  include Mappable_minimal

  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
end

module Make_map_iter (M : Mappable_minimal) = struct
  let map f arr =
    M.mapi (fun _ -> f) arr

  let iter f arr =
    M.iteri (fun _ -> f) arr
end

module Array_map : Mappable = struct
  type key = int
  type 'a t = 'a array

  let map = Array.map
  let iter = Array.iter
  let mapi = Array.mapi
  let iteri = Array.iteri
end

module Rev_array_map : Mappable = struct
  type key = int
  type 'a t = 'a array

  let mapi f arr =
    let len = Array.length arr in
    if len = 0 then [| |] else
    let last_index = Array.length arr - 1 in
    let last_elt = f last_index arr.(last_index) in
    let result = Array.make len last_elt in
    for i = last_index - 1 downto 0 do
      result.(i) <- f i arr.(i)
    done;
    result

  let iteri f arr =
    for i = Array.length arr - 1 downto 0 do
      f i arr.(i)
    done

  include functor Make_map_iter
end
