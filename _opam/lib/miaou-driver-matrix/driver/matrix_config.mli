(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Configuration for the Matrix driver.

    Settings can be configured via environment variables:
    - MIAOU_MATRIX_FPS: Frame rate cap for rendering (default: 60)
    - MIAOU_MATRIX_TPS: Tick rate for effects/input (default: 30)
    - MIAOU_MATRIX_DEBUG: Enable debug logging (default: false)
*)

type t = {
  fps_cap : int;  (** Maximum frames per second for render domain (1-120) *)
  frame_time_ms : float;  (** Minimum time between frames in ms *)
  tps_cap : int;  (** Maximum ticks per second for effects domain (1-120) *)
  tick_time_ms : float;  (** Minimum time between ticks in ms *)
  debug : bool;  (** Enable debug logging *)
}

(** Default configuration: 60 FPS, 30 TPS, no debug. *)
val default : t

(** Load configuration from environment variables. *)
val load : unit -> t

(** Minimum time in ms for given rate. *)
val time_of_rate : int -> float
