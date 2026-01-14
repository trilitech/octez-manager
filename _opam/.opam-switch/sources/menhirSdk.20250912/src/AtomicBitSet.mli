(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module offers bitsets that fit within an OCaml integer. This can be
   used to represent sets of integers in the semi-open interval [0, bound),
   where [bound] is [Sys.word_size - 1], that is, usually 63. *)

val bound: int

include GSet.S with type element = int and type t = private int

(* [iter_delta] and [fold_delta] are slightly generalized variants of [iter]
   and [fold]. They add the constant [delta] on the fly to each set element
   before presenting this set element to the user function [f]. *)

val iter_delta: int -> (element -> unit) -> t -> unit
val fold_delta: int -> (element -> 'b -> 'b) -> t -> 'b -> 'b

val maximum: t -> element
(** [maximum s] returns the largest element of the set [s].
    If the set [s] is empty, the exception [Not_found] is raised. *)

val diff: t -> t -> t
(** [diff s1 s2] is the set difference of [s1] and [s2].
    In other words, this set contains the elements of [s1]
    that are not elements of [s2]. *)

val above: element -> t -> t
(** [above x s] is the set of the elements of [s] that are greater
    than [x]. *)

val shift : t -> int -> t * t
(**[shift s delta] shifts the elements of the set [s] up by [delta],
   where [0 <= delta <= bound] must hold.
   The result is returned as pair of sets [(l, r)]:
     [l = { x + delta | x ∈ set, x + delta < bound }]
   and
     [r = { x + delta - bound | x ∈ set, x + delta >= bound }] *)
