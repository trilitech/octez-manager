(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let default_user_exists name =
  try
    let _ = Unix.getpwnam name in
    true
  with Not_found -> false

let default_group_exists name =
  try
    let _ = Unix.getgrnam name in
    true
  with Not_found -> false

module Hooks = struct
  let is_root = ref Common.is_root

  let run = ref Common.run

  let user_exists = ref default_user_exists

  let group_exists = ref default_group_exists
end

let is_root () = !Hooks.is_root ()

let run_cmd ?quiet argv = !Hooks.run ?quiet argv

let user_exists name = !Hooks.user_exists name

let group_exists name = !Hooks.group_exists name

let ensure_group ?(quiet = false) ~name () =
  if group_exists name then Ok ()
  else
    match run_cmd ~quiet ["groupadd"; "--system"; name] with
    | Ok () -> Ok ()
    | Error (`Msg e) -> R.error_msgf "Failed to create group %s: %s" name e

let ensure_service_account ?(quiet = false) ~name () =
  if not (is_root ()) then Ok ()
  else if user_exists name then Ok ()
  else
    let* () = ensure_group ~quiet ~name () in
    match
      run_cmd
        ~quiet
        [
          "useradd";
          "--system";
          "--home-dir";
          Printf.sprintf "/var/lib/%s" name;
          "--shell";
          "/usr/sbin/nologin";
          "--comment";
          "Octez Manager service user";
          "--gid";
          name;
          "--no-create-home";
          name;
        ]
    with
    | Ok () -> Ok ()
    | Error (`Msg e) -> R.error_msgf "Failed to create user %s: %s" name e

let ensure_system_directories ~user ~group () =
  if not (is_root ()) then Ok ()
  else
    let dirs =
      [
        ("/var/lib/octez", 0o755);
        ("/var/log/octez", 0o755);
        ("/etc/octez/instances", 0o755);
      ]
    in
    List.fold_left
      (fun acc (path, mode) ->
        match acc with
        | Error _ as e -> e
        | Ok () -> (
            match Common.ensure_dir_path ~owner:user ~group ~mode path with
            | Ok () -> Ok ()
            | Error (`Msg e) ->
                R.error_msgf "Failed to create/chown %s: %s" path e))
      (Ok ())
      dirs

let validate_user_for_service ~user =
  if user_exists user then Ok ()
  else
    R.error_msgf
      "Service user '%s' does not exist. Run 'octez-manager bootstrap-user' as \
       root."
      user

let remove_service_account ?(quiet = false) ~name () =
  let trimmed = String.trim name in
  if trimmed = "" then Ok ()
  else if not (is_root ()) then Ok ()
  else
    let* () =
      if user_exists trimmed then
        match run_cmd ~quiet ["userdel"; "--remove"; trimmed] with
        | Ok () -> Ok ()
        | Error (`Msg e) ->
            R.error_msgf "Failed to delete user %s: %s" trimmed e
      else Ok ()
    in
    if group_exists trimmed then
      match run_cmd ~quiet ["groupdel"; trimmed] with
      | Ok () -> Ok ()
      | Error (`Msg e) -> R.error_msgf "Failed to delete group %s: %s" trimmed e
    else Ok ()

module For_tests = struct
  let reset () =
    Hooks.is_root := Common.is_root ;
    Hooks.run := Common.run ;
    Hooks.user_exists := default_user_exists ;
    Hooks.group_exists := default_group_exists

  let with_overrides ?is_root ?run ?user_exists ?group_exists f =
    let original_is_root = !Hooks.is_root in
    let original_run = !Hooks.run in
    let original_user_exists = !Hooks.user_exists in
    let original_group_exists = !Hooks.group_exists in
    (match is_root with Some fn -> Hooks.is_root := fn | None -> ()) ;
    (match run with Some fn -> Hooks.run := fn | None -> ()) ;
    (match user_exists with Some fn -> Hooks.user_exists := fn | None -> ()) ;
    (match group_exists with Some fn -> Hooks.group_exists := fn | None -> ()) ;
    Fun.protect
      ~finally:(fun () ->
        Hooks.is_root := original_is_root ;
        Hooks.run := original_run ;
        Hooks.user_exists := original_user_exists ;
        Hooks.group_exists := original_group_exists)
      f
end
