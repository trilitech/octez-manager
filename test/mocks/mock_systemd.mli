(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

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

(** Mock systemd for testing service operations without real systemd *)

(** {1 Service States} *)

type service_state = Stopped | Running | Failed of string | Unknown

type mock_service = {
  name : string;
  state : service_state;
  enabled : bool;
  active_since : float option;
  pid : int option;
  restart_count : int;
}

type failure_mode =
  | NoFailure
  | StartFails of string
  | StopFails of string
  | PermissionDenied
  | ServiceNotFound
  | Timeout

(** {1 Setup and Registration} *)

(** Register a mock service for testing *)
val register_service :
  ?enabled:bool ->
  ?state:service_state ->
  ?pid:int option ->
  ?active_since:float option ->
  string ->
  unit

(** Clear all registered services and failure modes *)
val reset : unit -> unit

(** {1 Failure Injection} *)

(** Configure a service to fail operations in specific ways *)
val set_failure_mode : string -> failure_mode -> unit

(** Remove failure mode for a service *)
val clear_failure_mode : string -> unit

(** {1 Service Operations} *)

val start_service : string -> (unit, string) result

val stop_service : string -> (unit, string) result

val restart_service : string -> (unit, string) result

val enable_service : string -> (unit, string) result

val disable_service : string -> (unit, string) result

(** Manually put a service into failed state *)
val fail_service : string -> string -> unit

(** {1 State Queries} *)

val get_service_state : string -> service_state

val get_service : string -> mock_service option

val service_exists : string -> bool

val is_service_running : string -> bool

val is_service_enabled : string -> bool

val list_services : unit -> mock_service list

(** {1 Test Assertions} *)

(** Raises Failure if service is not running *)
val assert_service_running : string -> unit

(** Raises Failure if service is not stopped *)
val assert_service_stopped : string -> unit

(** Raises Failure if service is not enabled *)
val assert_service_enabled : string -> unit

(** Raises Failure if service is not disabled *)
val assert_service_disabled : string -> unit

(** [assert_service_failed name error] raises Failure if service is not failed
    with the expected error message *)
val assert_service_failed : string -> string -> unit

(** {1 Integration Hook} *)

(** Check if OCTEZ_MANAGER_TEST_MODE is set *)
val is_test_mode : unit -> bool

(** Hook to be called from Common.run for systemctl commands *)
val run_hook : ?quiet:bool -> string -> (string, string) result
