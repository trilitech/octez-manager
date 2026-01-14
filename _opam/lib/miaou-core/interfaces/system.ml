(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* System capability implementation living in miaou_interfaces.
   This lets other libraries depend on the small interfaces package. *)

module Capability = Capability

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
  get_disk_usage : path:string -> (int64, string) result;
  list_dir : string -> (string list, string) result;
  probe_writable : path:string -> (bool, string) result;
  get_env_var : string -> string option;
}

let key : t Capability.key = Capability.create ~name:"System"

let set v = Capability.set key v

let get () = Capability.get key

let require () = Capability.require key
