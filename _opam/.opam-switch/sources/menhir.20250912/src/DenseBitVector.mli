(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a simple implementation of bit vectors,
   that is, dynamic arrays of bits.

   Although the length of the vector is measured in bits, several of the
   functions access the vector using addresses that are measured in words.
   A word is a group of [w] bits, where the constant [w] is [A.bound],
   that is, typically 63. *)

(**A mutable vector of bits. *)
type vector

(**A word is a group of [w] bits, where the constant [w] is
   [AtomicBitSet.bound], that is, typically 63. *)
type word =
  AtomicBitSet.t

(**[create()] returns a fresh empty vector. *)
val create : unit -> vector

(**[length v] returns the current length of the vector [v] in words. *)
val length : vector -> int

(**[grow v a] grows the vector [v], if necessary, so that the address [a]
   is a valid address in this vector. When the vector is grown, the new
   space is initialized with 0 bits. *)
val grow : vector -> int -> unit

(**[get v a] reads a word at address [a] in the vector [v].
   The address [a] must be a valid address in the vector [v]. *)
val get : vector -> int -> word

(**[set v a w] writes the word [w] at address [a] in the vector [v].
   The address [a] must be a valid address in the vector [v]. *)
val set : vector -> int -> word -> unit

(**[width v] returns the width of the vector [v]. If no bit is set in
   the vector [v] then its width is zero. If the most significant bit
   that is set in the vector [v] lies at bit index [i] then the width
   of the vector [v] is [i+1]. *)
val width : vector -> int
