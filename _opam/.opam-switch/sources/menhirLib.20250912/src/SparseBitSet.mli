(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include GSet.S with type element = int

val diff: t -> t -> t
(** [diff s1 s2] is the set difference of [s1] and [s2].
    In other words, this set contains the elements of [s1]
    that are not elements of [s2]. *)

val above: element -> t -> t
(** [above x s] is the set of the elements of [s] that are greater
    than [x]. *)

val find_first_opt: (element -> bool) -> t -> element option
(** [find_first_opt f s] returns the least element [x] of [s] such that
    [f x] is true. It returns [None] if no such element exists. *)

(**The type [view] offers a view of a set as a list of words.

   The constructor [N] represents the empty set.

   The constructor [C (o, word, s)] represents the disjoint union of
   the set [word], whose inhabitants must be shifted up by [o], and
   the set [s].

   The offset [o] is always a multiple of the word size,
   [AtomicBitSet.bound]. *)
type view =
  | N
  | C of offset * word * t

and offset =
  int (* a multiple of [AtomicBitSet.bound] *)

and word =
  AtomicBitSet.t

(**The function [view] offers a view of a sparse bit set as a list of words. *)
val view : t -> view
