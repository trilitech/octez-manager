(** [count_leading_zeros n] returns the number of most-significant
    zero bits before the most significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 32 or 64, depending on the target. *)
val count_leading_zeros : nativeint -> int

(** Same as [count_leading_zeros] except if the argument is zero,
    then the result is undefined. Emits more efficient code.

    This is no longer needed when using an flambda-backend compiler,
    which translates [count_leading_zeros] to LZCNT by default (amd64).
*)
val count_leading_zeros_nonzero_arg : nativeint -> int

(** [count_trailing_zeros n] returns the number of least-significant
    zero bits before the least significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 32 or 64, depending on the target. *)
val count_trailing_zeros : nativeint -> int

(** Same as [count_trailing_zeros] except if the argument is zero,
    then the result is undefined. Emits more efficient code.

    This is no longer needed when using an flambda-backend compiler,
    which translates [count_trailing_zeros] to TZCNT by default (amd64).
*)
val count_trailing_zeros_nonzero_arg : nativeint -> int

(** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
val count_set_bits : nativeint -> int
