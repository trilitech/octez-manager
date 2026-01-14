(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a fast replacement for the table compression algorithm
   in MenhirLib's [RowDisplacement] module. Its representation of compressed
   tables is the same, so it is compatible with [RowDisplacement]: a table
   that has been constructed by this module can be looked up by the accessor
   functions in the module [RowDisplacement]. *)

open MenhirLib

(**[compress insignificant dummy t] turns the two-dimensional table [t] into
   a compressed table. The parameter [insignificant] determines which data
   values are insignificant, and can thus be overwritten with other values.
   The parameter [dummy] is used to fill holes in the data array. The type
   ['a] must support OCaml's polymorphic equality and hash functions. *)
val compress :
  ('a -> bool) ->
  'a ->
  'a array array ->
  'a RowDisplacement.table
