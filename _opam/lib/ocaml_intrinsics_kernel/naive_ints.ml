module type Intlike = sig
  type t

  val logand : t -> t -> t
  val zero : t
  val one : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val shift_right_logical : t -> int -> t
  val shift_left : t -> int -> t
  val bitwidth : int
end

module Make (Int : Intlike) = struct
  let least_significant_bit n = Int.logand n Int.one

  let is_least_significant_bit_set n =
    let lsb = least_significant_bit n in
    if Int.equal lsb Int.one
    then true
    else if Int.equal lsb Int.zero
    then false
    else assert false
  ;;

  let is_most_significant_bit_set n = if Int.compare n Int.zero < 0 then true else false

  let count_trailing_zeros n =
    let rec loop ~acc ~mask =
      if is_least_significant_bit_set mask
      then acc
      else (
        let mask = Int.shift_right_logical mask 1 in
        let acc = acc + 1 in
        loop ~mask ~acc)
    in
    if Int.equal n Int.zero then Int.bitwidth else loop ~acc:0 ~mask:n
  ;;

  let count_leading_zeros n =
    let rec loop ~acc ~mask =
      if is_most_significant_bit_set mask
      then acc
      else (
        let mask = Int.shift_left mask 1 in
        let acc = acc + 1 in
        loop ~mask ~acc)
    in
    if Int.equal n Int.zero then Int.bitwidth else loop ~acc:0 ~mask:n
  ;;

  let count_set_bits n =
    let rec loop ~acc ~mask =
      if Int.equal mask Int.zero
      then acc
      else (
        let acc = if is_least_significant_bit_set mask then acc + 1 else acc in
        let mask = Int.shift_right_logical mask 1 in
        loop ~mask ~acc)
    in
    loop ~acc:0 ~mask:n
  ;;
end
