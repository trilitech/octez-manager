(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(** {1 Helpers} *)

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop idx =
    if idx + nlen > hlen then false
    else if String.sub haystack idx nlen = needle then true
    else loop (idx + 1)
  in
  if nlen = 0 then true else loop 0

(** {1 Cache} *)

let cache : External_service.t list ref = ref []

let cache_lock = Mutex.create ()

let get_cached () = Mutex.protect cache_lock (fun () -> !cache)

let clear_cache () = Mutex.protect cache_lock (fun () -> cache := [])

(** {1 Filtering} *)

(** Check if unit name matches octez-manager's naming convention.
    Pattern: octez-<role>@<instance>.service *)
let is_managed_unit_name unit_name =
  (* Must start with "octez-" *)
  if not (String.starts_with ~prefix:"octez-" unit_name) then false
  else
    (* Must contain exactly one @ symbol *)
    match String.split_on_char '@' unit_name with
    | [_role_part; instance_part] ->
        (* instance_part should end with .service *)
        String.ends_with ~suffix:".service" instance_part
    | _ -> false

let is_in_registry ~unit_name =
  (* Extract instance name from unit name *)
  match String.split_on_char '@' unit_name with
  | [_; instance_part] -> (
      (* Remove .service suffix *)
      let instance =
        if String.ends_with ~suffix:".service" instance_part then
          String.sub instance_part 0 (String.length instance_part - 8)
        else instance_part
      in
      (* Check registry *)
      match Service_registry.find ~instance with
      | Ok (Some _) -> true
      | Ok None -> false
      | Error _ -> false)
  | _ -> false

(** {1 Systemd Queries} *)

let systemctl_cmd () =
  if Common.is_root () then ["systemctl"] else ["systemctl"; "--user"]

let list_all_service_units () =
  (* List all service units, including inactive and disabled ones *)
  let cmd =
    systemctl_cmd ()
    @ ["list-units"; "--type=service"; "--all"; "--no-legend"; "--no-pager"]
  in
  match Common.run_out cmd with
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      let units =
        List.filter_map
          (fun line ->
            let trimmed = String.trim line in
            if trimmed = "" then None
            else
              (* Line format: "unit.service   loaded   active   running   Description" *)
              (* Extract first field (unit name) *)
              match String.split_on_char ' ' trimmed with
              | unit_name :: _
                when String.ends_with ~suffix:".service" unit_name ->
                  Some unit_name
              | _ -> None)
          lines
      in
      Ok units
  | Error (`Msg msg) -> Error msg

let get_exec_start ~unit_name =
  let cmd =
    systemctl_cmd () @ ["show"; unit_name; "-p"; "ExecStart"; "--value"]
  in
  match Common.run_out cmd with
  | Ok output ->
      let trimmed = String.trim output in
      if trimmed = "" || trimmed = "[not set]" then None else Some trimmed
  | Error _ -> None

let get_unit_properties ~unit_name ~props =
  (* Build comma-separated property list *)
  let prop_list = String.concat "," props in
  let cmd = systemctl_cmd () @ ["show"; unit_name; "-p"; prop_list] in
  match Common.run_out cmd with
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      List.filter_map
        (fun line ->
          match String.split_on_char '=' line with
          | [] | [_] -> None
          | prop :: rest ->
              let value = String.concat "=" rest in
              Some (String.trim prop, String.trim value))
        lines
  | Error _ -> []

let get_unit_content ~unit_name =
  let cmd = systemctl_cmd () @ ["cat"; unit_name] in
  match Common.run_out cmd with
  | Ok content -> Ok content
  | Error (`Msg msg) ->
      let msg_lower = String.lowercase_ascii msg in
      if string_contains ~needle:"permission denied" msg_lower then
        Error `Permission_denied
      else Error (`Error msg)

(** {1 Detection Logic} *)

(** Check if ExecStart contains an octez binary *)
let contains_octez_binary exec_start =
  let lower = String.lowercase_ascii exec_start in
  string_contains ~needle:"octez-node" lower
  || string_contains ~needle:"octez-baker" lower
  || string_contains ~needle:"octez-accuser" lower
  || string_contains ~needle:"octez-dal-node" lower
  || string_contains ~needle:"tezos-baker" lower
  || string_contains ~needle:"tezos-accuser" lower

(** Extract binary path from ExecStart command.
    ExecStart can be complex (shell scripts, etc.), so we try to find
    the first octez binary mentioned. *)
let extract_binary_from_exec_start exec_start =
  (* Split by spaces and quotes, look for octez binary paths *)
  let words = String.split_on_char ' ' exec_start in
  List.find_opt
    (fun word ->
      let word_lower = String.lowercase_ascii word in
      string_contains ~needle:"octez-" word_lower
      || string_contains ~needle:"tezos-baker" word_lower
      || string_contains ~needle:"tezos-accuser" word_lower)
    words

(** Build a minimal External_service.t from a unit name and ExecStart.
    Full configuration parsing will be implemented in later issues. *)
let build_external_service ~unit_name ~exec_start ~properties =
  (* Get basic systemd properties *)
  let user = List.assoc_opt "User" properties in
  let group = List.assoc_opt "Group" properties in
  let working_dir = List.assoc_opt "WorkingDirectory" properties in
  let active_state =
    List.assoc_opt "ActiveState" properties |> Option.value ~default:"unknown"
  in
  let sub_state =
    List.assoc_opt "SubState" properties |> Option.value ~default:"unknown"
  in
  let unit_file_state = List.assoc_opt "UnitFileState" properties in

  (* Determine if enabled *)
  let enabled =
    match unit_file_state with
    | Some "enabled" -> Some true
    | Some "disabled" -> Some false
    | _ -> None
  in

  let unit_state : External_service.unit_state =
    {active_state; sub_state; enabled}
  in

  (* Get environment files *)
  let environment_files =
    match List.assoc_opt "EnvironmentFile" properties with
    | Some files_str ->
        (* Can be multiple files separated by spaces or ; *)
        String.split_on_char ' ' files_str
        |> List.filter (fun s -> String.trim s <> "")
    | None -> []
  in

  (* Try to detect binary path and role *)
  let binary_path_opt = extract_binary_from_exec_start exec_start in
  let role_field =
    match binary_path_opt with
    | Some binary ->
        let role = External_service.role_of_binary_name binary in
        External_service.detected ~source:"ExecStart" role
    | None -> External_service.unknown ()
  in

  let binary_field =
    match binary_path_opt with
    | Some binary -> External_service.detected ~source:"ExecStart" binary
    | None -> External_service.unknown ()
  in

  (* Build config *)
  let config =
    {
      (External_service.empty_config ~unit_name ~exec_start ~unit_state) with
      user;
      group;
      working_dir;
      environment_files;
      role = role_field;
      binary_path = binary_field;
    }
  in

  let suggested_instance_name =
    External_service.suggest_instance_name ~unit_name
  in

  {External_service.config; suggested_instance_name}

let detect () =
  try
    (* List all service units *)
    let* all_units = list_all_service_units () in

    (* Filter and process each unit *)
    let external_services =
      List.filter_map
        (fun unit_name ->
          (* Skip managed units *)
          if is_managed_unit_name unit_name && is_in_registry ~unit_name then
            None
          else
            (* Get ExecStart *)
            match get_exec_start ~unit_name with
            | Some exec_start when contains_octez_binary exec_start ->
                (* Get additional properties *)
                let properties =
                  get_unit_properties
                    ~unit_name
                    ~props:
                      [
                        "User";
                        "Group";
                        "WorkingDirectory";
                        "ActiveState";
                        "SubState";
                        "UnitFileState";
                        "EnvironmentFile";
                      ]
                in
                Some (build_external_service ~unit_name ~exec_start ~properties)
            | _ -> None)
        all_units
    in

    (* Update cache *)
    Mutex.protect cache_lock (fun () -> cache := external_services) ;

    Ok external_services
  with e ->
    let msg = Printf.sprintf "Detection failed: %s" (Printexc.to_string e) in
    Error msg
