(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let ( let* ) = Rresult.R.bind

let write_pairs ~inst pairs =
  let base = Common.env_instances_base_dir () in
  let path = Filename.concat (Filename.concat base inst) "node.env" in
  let env_lines =
    pairs
    |> List.filter (fun (_, v) -> String.trim v <> "")
    |> List.map (fun (k, v) -> Format.sprintf "%s=%s\n" k v)
    |> String.concat ""
  in
  let body = "VERSION=v1\n" ^ env_lines in
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

let write ~inst ~data_dir ~run_args ~extra_env =
  let env_pairs =
    ("OCTEZ_DATA_DIR", data_dir) :: ("OCTEZ_NODE_ARGS", run_args) :: extra_env
  in
  write_pairs ~inst env_pairs
