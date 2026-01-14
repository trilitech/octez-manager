(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Rresult

let http_get_string ~env ~rpc_addr:_ ~app_bin_dir:_ path :
    (string, string) result =
  try
    Eio.Switch.run @@ fun sw ->
    let client = Cohttp_eio.Client.make ~https:None env#net in
    let uri = Uri.of_string path in
    let resp, body = Cohttp_eio.Client.get client ~sw uri in
    let status = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
    let body_str = Eio.Flow.read_all body in
    if Cohttp.Code.is_success status then Ok body_str
    else Error (Printf.sprintf "HTTP %d: %s" status body_str)
  with exn -> Error (Printexc.to_string exn)

let http_get_url ~env ~rpc_addr:_ ~app_bin_dir:_ url : (string, string) result =
  http_get_string ~env ~rpc_addr:"" ~app_bin_dir:"" url

let try_register () =
  try
    let _ =
      (* Create and register the provider *)
      Net.register (Net.create ~http_get_string ~http_get_url)
    in
    Ok ()
  with exn -> Error (Printexc.to_string exn)
