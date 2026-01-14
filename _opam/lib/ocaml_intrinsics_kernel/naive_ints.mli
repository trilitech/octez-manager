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

module Make (I : Intlike) : sig
  (** See documentation of [Int]. *)
  val count_leading_zeros : I.t -> int

  val count_set_bits : I.t -> int
  val count_trailing_zeros : I.t -> int
end
