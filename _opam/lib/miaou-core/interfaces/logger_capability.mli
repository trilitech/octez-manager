(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* (c) 2025 Nomadic Labs <contact@nomadic-labs.com> *)

(** Logger capability: application-provided logging API for Miaou.

    The core emits log messages through this capability. Implementations
    should support at least the provided levels, and may choose to
    decorate, filter, or persist messages (console, rotating file, system
    logger, etc.). The [set_logfile] function allows callers to request a
    log file path; the implementation may open, rotate, or reject the
    request.

    Example
    {[
      let logf level msg = Printf.eprintf "%s: %s\n" (match level with Debug->"DBG"|Info->"INF"|Warning->"WRN"|Error->"ERR") msg in
      let set_enabled _ = () in
      let set_logfile = fun _ -> Ok () in
      Miaou_core.Logger_capability.set { logf; set_enabled; set_logfile }
    ]}
*)

type level = Debug | Info | Warning | Error

type t = {
  logf : level -> string -> unit;
  set_enabled : bool -> unit;
  set_logfile : string option -> (unit, string) result; (* open/rotate file *)
}

val key : t Capability.key

val set : t -> unit

val get : unit -> t option

val require : unit -> t
