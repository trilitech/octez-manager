(** The are two versions of [count_leading_zeros] and [count_set_bits]. They have the
    same types, but their native code implementations differ.

    The first version inputs a tagged integer and the second version inputs an untagged
    integer. Generally, the first version (operating on a tagged integer) is faster, but
    if the untagged integer is already available in the surrounding context,
    the second version may be faster. *)

(** [count_leading_zeros n] returns the number of most-significant
    zero bits before the most significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 31 or 63, depending on the target. *)
val count_leading_zeros : int -> int

(** [count_leading_zeros2 n] computes the same result as [count_leading_zeros n].

    The functions only differ in optimizations that the compiler may be able to perform
    around the call. In particular, the implementation of [count_leading_zeros n] may
    operate directly on tagged n. *)
val count_leading_zeros2 : int -> int

(** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
val count_set_bits : int -> int

(** [count_set_bits2 n] computes the same result as [count_set_bits n].

    The functions only differs in optimizations that the compiler may be able to perform
    around the call. In particular, the implementation of [count_set_bits n] may operate
    directly on tagged n.  *)
val count_set_bits2 : int -> int

(** [count_trailing_zeros n] returns the number of least-significant
    zero bits before the least significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 31 or 63, depending on the target. *)
val count_trailing_zeros : int -> int
