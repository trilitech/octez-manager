(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(** Validate unix:// socket path format *)
let validate_unix_socket_path path =
  let trimmed = String.trim path in
  if trimmed = "" then R.error_msg "Unix socket path cannot be empty"
  else if not (String.starts_with ~prefix:"/" trimmed) then
    R.error_msgf "Unix socket path must be absolute: %s" trimmed
  else Ok trimmed

(** Validate http:// or https:// URL format *)
let validate_http_url url =
  let trimmed = String.trim url in
  let lower = String.lowercase_ascii trimmed in
  if
    not
      (String.starts_with ~prefix:"http://" lower
      || String.starts_with ~prefix:"https://" lower)
  then R.error_msgf "HTTP URL must start with http:// or https://: %s" trimmed
  else
    (* Parse as URI and check for host:port *)
    let uri = Uri.of_string trimmed in
    match (Uri.host uri, Uri.port uri) with
    | Some _host, Some port when port > 0 && port <= 65535 -> Ok trimmed
    | Some _host, None ->
        (* Default port: 80 for http, 443 for https *)
        Ok trimmed
    | Some _host, Some port ->
        R.error_msgf "Invalid port %d in URL: %s (must be 1-65535)" port trimmed
    | None, _ -> R.error_msgf "HTTP URL missing host: %s" trimmed

let validate_uri uri =
  let trimmed = String.trim uri in
  let lower = String.lowercase_ascii trimmed in
  if String.starts_with ~prefix:"unix:" lower then
    let path = String.sub trimmed 5 (String.length trimmed - 5) in
    validate_unix_socket_path path >>| fun _ -> trimmed
  else if
    String.starts_with ~prefix:"http://" lower
    || String.starts_with ~prefix:"https://" lower
  then validate_http_url trimmed
  else
    R.error_msgf
      "Remote signer URI must start with http://, https://, or unix:: %s"
      trimmed

let resolve_signatory_instance instance =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None ->
      R.error_msgf "Signatory instance '%s' not found in registry" instance
  | Some svc ->
      if svc.Service.role <> "signatory" then
        R.error_msgf
          "Instance '%s' is a %s service, not a signatory"
          instance
          svc.role
      else
        let rpc_addr = Rpc_addr.to_string svc.Service.rpc_addr in
        Ok ("http://" ^ rpc_addr)

let validate_and_resolve = function
  | Signer_types.Local_keys -> Ok None
  | Signer_types.Remote_signer {instance = Some name; uri = _} ->
      (* Managed instance: resolve from registry *)
      let* resolved_uri = resolve_signatory_instance name in
      Ok (Some resolved_uri)
  | Signer_types.Remote_signer {instance = None; uri} ->
      (* External URI: validate format only *)
      let* validated = validate_uri uri in
      Ok (Some validated)
