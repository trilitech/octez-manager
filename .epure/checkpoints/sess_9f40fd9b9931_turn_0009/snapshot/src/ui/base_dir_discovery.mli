(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Discovery of octez-client base directories.

    Combines three sources, in this order:
    - the default location ([~/.tezos-client]);
    - directories registered in {!Octez_manager_lib.Directory_registry}
      with type [Client_base_dir];
    - directories referenced by installed baker/accuser service env files
      ([OCTEZ_BAKER_BASE_DIR] / [OCTEZ_CLIENT_BASE_DIR]).

    Used by both the Wallets page and the [Add custom baker] flow so they
    list the same set of directories. Performs I/O — call only outside
    render functions. *)

(** Default client base directory ([~/.tezos-client]). *)
val default_client_base_dir : unit -> string

(** Strip a single trailing [/] from a path (unless it is just ["/"]). *)
val normalize_path : string -> string

(** Read installed baker/accuser service env files and return the base
    directories they reference. *)
val discover_from_services : unit -> string list

(** Return all known client base directories: default, registered, and
    service-discovered. Paths are normalized and deduplicated, then sorted
    alphabetically. *)
val list_all : unit -> string list
