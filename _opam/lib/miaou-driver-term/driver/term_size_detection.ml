(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Size detection for lambda-term driver - wrapper around Terminal_raw.
    Returns LTerm_geom.size for compatibility with existing code. *)

module Raw = Miaou_driver_common.Terminal_raw

(* Cached terminal handle for size detection *)
let terminal_ref : Raw.t option ref = ref None

let get_terminal () =
  match !terminal_ref with
  | Some t -> t
  | None ->
      (* Create a terminal just for size detection - this is safe because
         we only use it for size queries, not for raw mode or signals *)
      let t = Raw.setup () in
      terminal_ref := Some t ;
      t

let invalidate_cache () =
  match !terminal_ref with Some t -> Raw.invalidate_size_cache t | None -> ()

let detect_size () =
  let t = get_terminal () in
  let rows, cols = Raw.size t in
  {LTerm_geom.rows; cols}
