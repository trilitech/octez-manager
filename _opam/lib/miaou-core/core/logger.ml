(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module Tui_logger = struct
  let set_enabled b =
    match Miaou_interfaces.Logger_capability.get () with
    | Some logger -> logger.set_enabled b
    | None -> ()

  let set_logfile path_opt =
    match Miaou_interfaces.Logger_capability.get () with
    | Some logger -> logger.set_logfile path_opt
    | None -> Ok ()

  let logf fmt =
    match Miaou_interfaces.Logger_capability.get () with
    | Some logger -> Printf.ksprintf (fun msg -> logger.logf Info msg) fmt
    | None -> Printf.ifprintf () fmt
end
