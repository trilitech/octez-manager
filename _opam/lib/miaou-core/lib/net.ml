(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  http_get_string :
    env:Eio_unix.Stdenv.base ->
    rpc_addr:string ->
    app_bin_dir:string ->
    string ->
    (string, string) result;
  http_get_url :
    env:Eio_unix.Stdenv.base ->
    rpc_addr:string ->
    app_bin_dir:string ->
    string ->
    (string, string) result;
}

let key : t Miaou_interfaces.Capability.key =
  Miaou_interfaces.Capability.create ~name:"miaou.net"

let create
    ~(http_get_string :
       env:Eio_unix.Stdenv.base ->
       rpc_addr:string ->
       app_bin_dir:string ->
       string ->
       (string, string) result)
    ~(http_get_url :
       env:Eio_unix.Stdenv.base ->
       rpc_addr:string ->
       app_bin_dir:string ->
       string ->
       (string, string) result) =
  {http_get_string; http_get_url}

let register v = Miaou_interfaces.Capability.set key v

let get () = Miaou_interfaces.Capability.get key

let require () = Miaou_interfaces.Capability.require key

let http_get_string t ~rpc_addr ~app_bin_dir path =
  match Miaou_helpers.Fiber_runtime.env_opt () with
  | Some env -> t.http_get_string ~env ~rpc_addr ~app_bin_dir path
  | None ->
      Error
        "Eio runtime not initialized; call Fiber_runtime.init inside \
         Eio_main.run"

let http_get_url t ~rpc_addr ~app_bin_dir path =
  match Miaou_helpers.Fiber_runtime.env_opt () with
  | Some env -> t.http_get_url ~env ~rpc_addr ~app_bin_dir path
  | None ->
      Error
        "Eio runtime not initialized; call Fiber_runtime.init inside \
         Eio_main.run"
