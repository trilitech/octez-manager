(* Copyright 2025 Trilitech <contact@trili.tech>
   Copyright 2025 Functori <contact@functori.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)

(** Mock systemd for testing service operations without real systemd

    This module provides a mock implementation of systemd that allows testing
    service lifecycle operations (start, stop, restart, enable, disable) and
    error scenarios (permission denied, timeouts, etc.) without requiring an
    actual systemd installation.
    
    Usage:
      1. Enable test mode: Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1"
      2. Register mock services: register_service "my-node" ~state:Stopped
      3. Run your code that calls systemctl
      4. Verify: assert_service_running "my-node"
    
    See: Issue #458, TESTING_ROADMAP.md
*)

(** Service states *)
type service_state =
  | Stopped
  | Running
  | Failed of string  (** Failed with error message *)
  | Unknown

(** Mock service record *)
type mock_service = {
  name : string;
  state : service_state;
  enabled : bool;
  active_since : float option;
  pid : int option;
  restart_count : int;
}

(** Failure modes for testing error scenarios *)
type failure_mode =
  | NoFailure
  | StartFails of string  (** e.g., "port already in use" *)
  | StopFails of string
  | PermissionDenied
  | ServiceNotFound
  | Timeout

(** Global state *)

(* Registry of mock services *)
let services : (string, mock_service) Hashtbl.t = Hashtbl.create 17

(* Configured failure modes per service *)
let failure_modes : (string, failure_mode) Hashtbl.t = Hashtbl.create 17

(* Lock for thread safety *)
let lock = Mutex.create ()

(** {1 Service Registration} *)

let register_service ?(enabled = false) ?(state = Stopped) ?(pid = None)
    ?(active_since = None) name =
  Mutex.protect lock (fun () ->
      let service =
        {name; state; enabled; active_since; pid; restart_count = 0}
      in
      Hashtbl.replace services name service)

let reset () =
  Mutex.protect lock (fun () ->
      Hashtbl.clear services ;
      Hashtbl.clear failure_modes)

(** {1 Failure Injection} *)

let set_failure_mode name mode =
  Mutex.protect lock (fun () -> Hashtbl.replace failure_modes name mode)

let clear_failure_mode name =
  Mutex.protect lock (fun () -> Hashtbl.remove failure_modes name)

let get_failure_mode name =
  Mutex.protect lock (fun () ->
      Hashtbl.find_opt failure_modes name |> Option.value ~default:NoFailure)

(** {1 Service Operations} *)

let start_service name =
  Mutex.protect lock (fun () ->
      match get_failure_mode name with
      | PermissionDenied -> Error "Permission denied"
      | StartFails msg -> Error msg
      | Timeout -> Error "Operation timed out"
      | ServiceNotFound -> Error "Service not found"
      | _ -> (
          match Hashtbl.find_opt services name with
          | None -> Error "Service not found"
          | Some service -> (
              match service.state with
              | Running -> Error "Service already running"
              | Failed _ | Stopped | Unknown ->
                  let updated =
                    {
                      service with
                      state = Running;
                      active_since = Some (Unix.time ());
                      pid = Some (Random.int 65535 + 1000);
                    }
                  in
                  Hashtbl.replace services name updated ;
                  Ok ())))

let stop_service name =
  Mutex.protect lock (fun () ->
      match get_failure_mode name with
      | PermissionDenied -> Error "Permission denied"
      | StopFails msg -> Error msg
      | Timeout -> Error "Operation timed out"
      | ServiceNotFound -> Error "Service not found"
      | _ -> (
          match Hashtbl.find_opt services name with
          | None -> Error "Service not found"
          | Some service -> (
              match service.state with
              | Stopped -> Error "Service already stopped"
              | Running | Failed _ | Unknown ->
                  let updated =
                    {
                      service with
                      state = Stopped;
                      active_since = None;
                      pid = None;
                    }
                  in
                  Hashtbl.replace services name updated ;
                  Ok ())))

let restart_service name =
  Mutex.protect lock (fun () ->
      match stop_service name with
      | Error e -> Error e
      | Ok () -> (
          match start_service name with
          | Error e -> Error e
          | Ok () ->
              (* Increment restart count *)
              (match Hashtbl.find_opt services name with
              | Some service ->
                  let updated =
                    {service with restart_count = service.restart_count + 1}
                  in
                  Hashtbl.replace services name updated
              | None -> ()) ;
              Ok ()))

let enable_service name =
  Mutex.protect lock (fun () ->
      match get_failure_mode name with
      | PermissionDenied -> Error "Permission denied"
      | ServiceNotFound -> Error "Service not found"
      | _ -> (
          match Hashtbl.find_opt services name with
          | None -> Error "Service not found"
          | Some service ->
              let updated = {service with enabled = true} in
              Hashtbl.replace services name updated ;
              Ok ()))

let disable_service name =
  Mutex.protect lock (fun () ->
      match get_failure_mode name with
      | PermissionDenied -> Error "Permission denied"
      | ServiceNotFound -> Error "Service not found"
      | _ -> (
          match Hashtbl.find_opt services name with
          | None -> Error "Service not found"
          | Some service ->
              let updated = {service with enabled = false} in
              Hashtbl.replace services name updated ;
              Ok ()))

let fail_service name error_msg =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt services name with
      | None -> ()
      | Some service ->
          let updated =
            {
              service with
              state = Failed error_msg;
              active_since = None;
              pid = None;
            }
          in
          Hashtbl.replace services name updated)

(** {1 State Queries} *)

let get_service_state name =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt services name with
      | None -> Unknown
      | Some service -> service.state)

let get_service name =
  Mutex.protect lock (fun () -> Hashtbl.find_opt services name)

let service_exists name =
  Mutex.protect lock (fun () -> Hashtbl.mem services name)

let is_service_running name =
  match get_service_state name with Running -> true | _ -> false

let is_service_enabled name =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt services name with
      | None -> false
      | Some service -> service.enabled)

let list_services () =
  Mutex.protect lock (fun () -> Hashtbl.to_seq_values services |> List.of_seq)

(** {1 Test Assertions} *)

let assert_service_running name =
  match get_service_state name with
  | Running -> ()
  | state ->
      let msg =
        Printf.sprintf
          "Expected service '%s' to be running, but it is: %s"
          name
          (match state with
          | Stopped -> "stopped"
          | Failed e -> "failed: " ^ e
          | Unknown -> "unknown"
          | Running -> "running")
      in
      failwith msg

let assert_service_stopped name =
  match get_service_state name with
  | Stopped -> ()
  | state ->
      let msg =
        Printf.sprintf
          "Expected service '%s' to be stopped, but it is: %s"
          name
          (match state with
          | Running -> "running"
          | Failed e -> "failed: " ^ e
          | Unknown -> "unknown"
          | Stopped -> "stopped")
      in
      failwith msg

let assert_service_enabled name =
  if not (is_service_enabled name) then
    failwith (Printf.sprintf "Expected service '%s' to be enabled" name)

let assert_service_disabled name =
  if is_service_enabled name then
    failwith (Printf.sprintf "Expected service '%s' to be disabled" name)

let assert_service_failed name expected_error =
  match get_service_state name with
  | Failed actual_error when actual_error = expected_error -> ()
  | Failed actual_error ->
      failwith
        (Printf.sprintf
           "Service '%s' failed with '%s', expected '%s'"
           name
           actual_error
           expected_error)
  | state ->
      let state_str =
        match state with
        | Running -> "running"
        | Stopped -> "stopped"
        | Unknown -> "unknown"
        | Failed _ -> "failed"
      in
      failwith
        (Printf.sprintf
           "Expected service '%s' to be failed, but it is: %s"
           name
           state_str)

(** {1 systemctl Output Generation} *)

(** Generate realistic systemctl status output in JSON format *)
let generate_status_json name =
  match get_service name with
  | None -> `Assoc [("error", `String "Service not found")]
  | Some service ->
      let active_state =
        match service.state with
        | Running -> "active"
        | Stopped -> "inactive"
        | Failed _ -> "failed"
        | Unknown -> "unknown"
      in
      let sub_state =
        match service.state with
        | Running -> "running"
        | Stopped -> "dead"
        | Failed _ -> "failed"
        | Unknown -> "unknown"
      in
      `Assoc
        [
          ("name", `String (name ^ ".service"));
          ("load_state", `String "loaded");
          ("active_state", `String active_state);
          ("sub_state", `String sub_state);
          ("pid", `Int (Option.value service.pid ~default:0));
          ( "enabled",
            `String (if service.enabled then "enabled" else "disabled") );
          ( "active_enter_timestamp",
            match service.active_since with None -> `Null | Some t -> `Float t
          );
        ]

(** Generate systemctl list output *)
let generate_list_output () =
  let services = list_services () in
  let lines =
    List.map
      (fun service ->
        let state =
          match service.state with
          | Running -> "active"
          | Stopped -> "inactive"
          | Failed _ -> "failed"
          | Unknown -> "unknown"
        in
        Printf.sprintf
          "%s.service loaded %s %s"
          service.name
          state
          (if service.enabled then "enabled" else "disabled"))
      services
  in
  String.concat "\n" lines

(** {1 Mock systemctl Command Handler} *)

(** Parse and execute a systemctl command
    
    This function should be called from a hook in Common.run when
    OCTEZ_MANAGER_TEST_MODE is set.
    
    Returns: (exit_code, stdout, stderr)
*)
let handle_systemctl_command args =
  match args with
  | "systemctl" :: "start" :: service :: _ -> (
      match start_service service with
      | Ok () -> (0, "", "")
      | Error msg -> (1, "", "Failed to start " ^ service ^ ": " ^ msg))
  | "systemctl" :: "stop" :: service :: _ -> (
      match stop_service service with
      | Ok () -> (0, "", "")
      | Error msg -> (1, "", "Failed to stop " ^ service ^ ": " ^ msg))
  | "systemctl" :: "restart" :: service :: _ -> (
      match restart_service service with
      | Ok () -> (0, "", "")
      | Error msg -> (1, "", "Failed to restart " ^ service ^ ": " ^ msg))
  | "systemctl" :: "enable" :: service :: _ -> (
      match enable_service service with
      | Ok () -> (0, "", "")
      | Error msg -> (1, "", "Failed to enable " ^ service ^ ": " ^ msg))
  | "systemctl" :: "disable" :: service :: _ -> (
      match disable_service service with
      | Ok () -> (0, "", "")
      | Error msg -> (1, "", "Failed to disable " ^ service ^ ": " ^ msg))
  | "systemctl" :: "show" :: "--property=ActiveState" :: service :: _ ->
      let state =
        match get_service_state service with
        | Running -> "ActiveState=active"
        | Stopped -> "ActiveState=inactive"
        | Failed _ -> "ActiveState=failed"
        | Unknown -> "ActiveState=unknown"
      in
      (0, state, "")
  | "systemctl" :: "is-enabled" :: service :: _ ->
      if is_service_enabled service then (0, "enabled\n", "")
      else (1, "disabled\n", "")
  | "systemctl" :: "status" :: service :: _ ->
      let json = generate_status_json service in
      (0, Yojson.Basic.to_string json, "")
  | "systemctl" :: "list-units" :: _ ->
      let output = generate_list_output () in
      (0, output, "")
  | _ ->
      (* Unknown command - return error *)
      (1, "", "Unknown systemctl command: " ^ String.concat " " args)

(** {1 Integration Hook} *)

(** Check if test mode is enabled *)
let is_test_mode () =
  match Sys.getenv_opt "OCTEZ_MANAGER_TEST_MODE" with
  | Some "1" | Some "true" -> true
  | _ -> false

(** Hook to be called from Common.run 
    
    Add this to src/common.ml:
    
    {[
      let run ?quiet cmd =
        if Mock_systemd.is_test_mode () && String.starts_with ~prefix:"systemctl" cmd then
          Mock_systemd.run_hook ~quiet cmd
        else
          (* original implementation *)
    ]}
*)
let run_hook ?quiet cmd =
  let _quiet = Option.value quiet ~default:false in
  let args = String.split_on_char ' ' cmd in
  let exit_code, stdout, stderr = handle_systemctl_command args in
  if exit_code <> 0 then Error stderr else Ok stdout
