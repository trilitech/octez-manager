(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type file_read = string -> (string, string) result

type file_write = string -> string -> (unit, string) result

type run_result = {exit_code : int; stdout : string; stderr : string}

type t = {
  file_exists : string -> bool;
  is_directory : string -> bool;
  read_file : file_read;
  write_file : file_write;
  mkdir : string -> (unit, string) result;
  run_command :
    argv:string list -> cwd:string option -> (run_result, string) result;
  get_current_user_info : unit -> (string * string, string) result;
      (* user, homedir *)
  get_disk_usage : path:string -> (int64, string) result;
      (* bytes used on path *)
  list_dir : string -> (string list, string) result;
      (* list entries in directory *)
  probe_writable : path:string -> (bool, string) result;
      (* attempt to determine if path is writable *)
  get_env_var : string -> string option; (* get environment variable *)
}

val key : t Capability.key

val set : t -> unit

val get : unit -> t option

val require : unit -> t
