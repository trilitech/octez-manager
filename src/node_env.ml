(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Rresult.R.bind

(** Documentation for common environment variables *)
let get_var_doc = function
  | "OCTEZ_DATA_DIR" ->
      Some
        "Path to the Octez data directory containing blockchain data and \
         configuration"
  | "OCTEZ_RPC_ADDR" ->
      Some
        "RPC server listen address in format 'host:port' (e.g., \
         'localhost:8732')\n\
         # Use '0.0.0.0:PORT' to listen on all interfaces, or 'localhost:PORT' \
         for local only"
  | "OCTEZ_NET_ADDR" ->
      Some
        "P2P network listen address in format 'host:port' (e.g., ':9732')\n\
         # Use ':PORT' to listen on all interfaces on the specified port"
  | "OCTEZ_NETWORK" ->
      Some "Network name (e.g., 'mainnet', 'ghostnet', 'nairobinet')"
  | "APP_BIN_DIR" ->
      Some "Directory containing Octez binaries (octez-node, octez-baker, etc.)"
  | "OCTEZ_NODE_ENDPOINT" ->
      Some
        "Node RPC endpoint for baker/accuser/DAL (e.g., \
         'http://localhost:8732')"
  | "OCTEZ_CLIENT_BASE_DIR" ->
      Some
        "Octez client base directory containing keys and configuration\n\
         # This is where your baker keys are stored"
  | "OCTEZ_DAL_DATA_DIR" -> Some "Path to the DAL node data directory"
  | "OCTEZ_DAL_RPC_ADDR" ->
      Some "DAL node RPC listen address (e.g., '127.0.0.1:10732')"
  | "OCTEZ_DAL_NET_ADDR" ->
      Some "DAL node P2P listen address (e.g., '127.0.0.1:11732')"
  | _ -> None

(** Escape a value for safe use in shell environment files.
    Wraps the value in double quotes and escapes special characters.
    This prevents shell expansion of glob patterns (asterisk, question mark, brackets),
    variables (dollar sign), and command substitution (backticks). *)
let escape_env_value v =
  (* Check if value needs quoting *)
  let needs_quotes =
    String.length v > 0
    && (String.contains v ' ' || String.contains v '*' || String.contains v '?'
      || String.contains v '$' || String.contains v '`' || String.contains v '"'
      || String.contains v '\'' || String.contains v '\\'
      || String.contains v ';' || String.contains v '&' || String.contains v '|'
       )
  in
  if not needs_quotes then v
  else
    (* Escape backslashes and double quotes, then wrap in double quotes *)
    let escaped =
      v |> String.split_on_char '\\' |> String.concat "\\\\"
      |> String.split_on_char '"' |> String.concat "\\\""
      |> String.split_on_char '$' |> String.concat "\\$"
      |> String.split_on_char '`' |> String.concat "\\`"
    in
    "\"" ^ escaped ^ "\""

let write_pairs ?(with_comments = false) ~inst pairs =
  let base = Common.env_instances_base_dir () in
  let path = Filename.concat (Filename.concat base inst) "node.env" in
  let env_lines =
    pairs
    |> List.filter (fun (_, v) -> String.trim v <> "")
    |> List.map (fun (k, v) ->
        let escaped_value = escape_env_value v in
        if with_comments then
          match get_var_doc k with
          | Some doc -> Format.sprintf "\n# %s\n%s=%s\n" doc k escaped_value
          | None -> Format.sprintf "%s=%s\n" k escaped_value
        else Format.sprintf "%s=%s\n" k escaped_value)
    |> String.concat ""
  in
  let header =
    if with_comments then
      "# Octez Service Environment Configuration\n\
       # This file is sourced by the systemd service unit.\n\
       # Edit carefully - incorrect values may prevent the service from \
       starting.\n\
       #\n\
       # After editing, reload with: systemctl daemon-reload && systemctl \
       restart SERVICE\n\n\
       VERSION=v1\n"
    else "VERSION=v1\n"
  in
  let body = header ^ env_lines in
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let* _ =
    Common.ensure_dir_path ~owner ~group ~mode:0o755 (Filename.dirname path)
  in
  Common.write_file ~mode:0o644 ~owner ~group path body

let read ~inst =
  let base = Common.env_instances_base_dir () in
  let path = Filename.concat (Filename.concat base inst) "node.env" in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let ic = open_in path in
      let rec loop acc =
        try
          let line = input_line ic in
          let trimmed = String.trim line in
          if trimmed = "" || String.starts_with ~prefix:"#" trimmed then
            loop acc
          else
            match String.split_on_char '=' trimmed with
            | key :: rest ->
                let value = String.concat "=" rest in
                loop ((key, value) :: acc)
            | [] -> loop acc
        with End_of_file ->
          close_in ic ;
          List.rev acc
      in
      Ok (loop [])
    with Sys_error msg -> Error (`Msg msg)

let write ~inst ~data_dir ~run_args ~extra_env ?(with_comments = false) () =
  let env_pairs =
    ("OCTEZ_DATA_DIR", data_dir) :: ("OCTEZ_NODE_ARGS", run_args) :: extra_env
  in
  write_pairs ~with_comments ~inst env_pairs
