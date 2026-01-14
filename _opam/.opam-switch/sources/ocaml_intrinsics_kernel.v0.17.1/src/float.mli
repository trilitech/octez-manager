(* X86 docs say:

   If only one value is a NaN (SNaN or QNaN) for this instruction, the second source
   operand, either a NaN or a valid floating-point value
   is written to the result.

   So we have to be VERY careful how we use these!
*)
(** Equivalent to [if x < y then x else y].

    On an x86-64 machine, this compiles to [minsd xmm0, xmm1].
    On ARM, this calls a C implementation. *)
external min
  :  (float[@unboxed])
  -> (float[@unboxed])
  -> (float[@unboxed])
  = "caml_sse2_float64_min_bytecode" "caml_sse2_float64_min"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Equivalent to [if x > y then x else y].

    On an x86-64 machine, this compiles to [maxsd xmm0, xmm1].
    On ARM, this calls a C implementation. *)
external max
  :  (float[@unboxed])
  -> (float[@unboxed])
  -> (float[@unboxed])
  = "caml_sse2_float64_max_bytecode" "caml_sse2_float64_max"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
