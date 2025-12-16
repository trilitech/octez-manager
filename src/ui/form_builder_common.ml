(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_manager_lib

(** {1 Configuration Types} *)

type core_service_config = {
  instance_name : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

type client_config = {
  base_dir : string;
  node : [`Service of string | `Endpoint of string | `None];
  node_endpoint : string;
}

type node_config = {
  network : string;
  history_mode : string;
  data_dir : string;
  rpc_addr : string;
  p2p_addr : string;
}

(** {1 Common Validators} *)

let is_nonempty s = String.trim s <> ""

let normalize s = String.lowercase_ascii (String.trim s)

let instance_in_use ~states name =
  let target = normalize name in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalize s.service.Service.instance))
       states

let service_user_valid ~user =
  if Common.is_root () then true
  else Result.is_ok (System_user.validate_user_for_service ~user)

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        if p > 0 && p < 65536 && String.trim host <> "" then Some (host, p)
        else None
      with _ -> None)
  | _ -> None

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

(** {1 Helpers} *)

let prepare_extra_args s =
  if String.trim s = "" then []
  else
    String.split_on_char ' ' s
    |> List.filter (fun s -> String.trim s <> "")
