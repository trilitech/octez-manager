(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for Group type and Group_registry *)

open Octez_manager_lib

(* ========================================================================= *)
(* Test Helpers *)
(* ========================================================================= *)

let is_ci () = match Sys.getenv_opt "CI" with Some "true" -> true | _ -> false

let with_temp_dir f =
  let base = Filename.temp_file "octez_manager" "test" in
  Sys.remove base ;
  Unix.mkdir base 0o755 ;
  Fun.protect
    ~finally:(fun () ->
      let (_ : (unit, [> Rresult.R.msg]) result) = File_ops.remove_tree base in
      ())
    (fun () -> f base)

let with_fake_xdg f =
  with_temp_dir (fun base ->
      let mk_dir name =
        let dir = Filename.concat base name in
        Unix.mkdir dir 0o755 ;
        dir
      in
      let config = mk_dir "cfg" in
      let data = mk_dir "data" in
      let set_var k v = Unix.putenv k v in
      let saved_config = Sys.getenv_opt "XDG_CONFIG_HOME" in
      let saved_data = Sys.getenv_opt "XDG_DATA_HOME" in
      set_var "XDG_CONFIG_HOME" config ;
      set_var "XDG_DATA_HOME" data ;
      Fun.protect
        ~finally:(fun () ->
          (match saved_config with
          | Some v -> set_var "XDG_CONFIG_HOME" v
          | None -> set_var "XDG_CONFIG_HOME" "") ;
          match saved_data with
          | Some v -> set_var "XDG_DATA_HOME" v
          | None -> set_var "XDG_DATA_HOME" "")
        (fun () -> f ()))

let sample_group ?(name = "mainnet-prod") ?(network = "mainnet")
    ?(service_user = "tezos") ?(app_bin_dir = "/opt/octez") () : Group.t =
  {
    Group.name;
    network;
    bin_source = Binary_registry.Managed_octez_version "24.0";
    service_user;
    app_bin_dir;
    created_at = "2026-01-01 00:00:00";
  }

let group_equal (a : Group.t) (b : Group.t) =
  String.equal a.name b.name
  && String.equal a.network b.network
  && a.bin_source = b.bin_source
  && String.equal a.service_user b.service_user
  && String.equal a.app_bin_dir b.app_bin_dir
  && String.equal a.created_at b.created_at

let check_group expected actual =
  Alcotest.(check bool) "group equality" true (group_equal expected actual)

(* ========================================================================= *)
(* Group type tests *)
(* ========================================================================= *)

let test_group_roundtrip_json () =
  let original = sample_group () in
  let json = Group.to_yojson original in
  match Group.of_yojson json with
  | Ok decoded -> check_group original decoded
  | Error (`Msg msg) -> Alcotest.failf "roundtrip failed: %s" msg

let test_group_roundtrip_raw_path () =
  let original =
    {(sample_group ()) with bin_source = Binary_registry.Raw_path "/usr/bin"}
  in
  let json = Group.to_yojson original in
  match Group.of_yojson json with
  | Ok decoded -> check_group original decoded
  | Error (`Msg msg) -> Alcotest.failf "roundtrip failed: %s" msg

let test_group_roundtrip_registered_alias () =
  let original =
    {
      (sample_group ()) with
      bin_source = Binary_registry.Registered_alias "dev-build";
    }
  in
  let json = Group.to_yojson original in
  match Group.of_yojson json with
  | Ok decoded -> check_group original decoded
  | Error (`Msg msg) -> Alcotest.failf "roundtrip failed: %s" msg

let test_group_make () =
  let g =
    Group.make
      ~name:"test"
      ~network:"ghostnet"
      ~bin_source:(Binary_registry.Managed_octez_version "24.1")
      ~service_user:"tezos"
      ~app_bin_dir:"/opt/octez"
      ()
  in
  Alcotest.(check string) "name" "test" g.name ;
  Alcotest.(check string) "network" "ghostnet" g.network ;
  Alcotest.(check string) "service_user" "tezos" g.service_user ;
  Alcotest.(check string) "app_bin_dir" "/opt/octez" g.app_bin_dir ;
  Alcotest.(check bool)
    "created_at non-empty"
    true
    (String.length g.created_at > 0)

let test_group_invalid_json () =
  let json = `Assoc [("name", `String "test")] in
  match Group.of_yojson json with
  | Ok _ -> Alcotest.fail "Should fail on incomplete JSON"
  | Error _ -> ()

(* ========================================================================= *)
(* Group_registry tests *)
(* ========================================================================= *)

let test_registry_write_and_find () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      let group = sample_group () in
      let result = Group_registry.write group in
      Alcotest.(check bool) "write succeeds" true (Result.is_ok result) ;
      match Group_registry.find ~name:"mainnet-prod" with
      | Ok (Some found) -> check_group group found
      | Ok None -> Alcotest.fail "Group not found after write"
      | Error (`Msg msg) -> Alcotest.failf "find failed: %s" msg)

let test_registry_list_empty () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      match Group_registry.list () with
      | Ok groups -> Alcotest.(check int) "empty list" 0 (List.length groups)
      | Error (`Msg msg) -> Alcotest.failf "list failed: %s" msg)

let test_registry_list_multiple () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      let g1 = sample_group ~name:"group-a" () in
      let g2 = sample_group ~name:"group-b" ~network:"ghostnet" () in
      let _ = Group_registry.write g1 in
      let _ = Group_registry.write g2 in
      match Group_registry.list () with
      | Ok groups -> Alcotest.(check int) "two groups" 2 (List.length groups)
      | Error (`Msg msg) -> Alcotest.failf "list failed: %s" msg)

let test_registry_find_missing () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      match Group_registry.find ~name:"nonexistent" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "Should not find nonexistent group"
      | Error (`Msg msg) -> Alcotest.failf "find failed: %s" msg)

let test_registry_remove () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      let group = sample_group () in
      let _ = Group_registry.write group in
      let result = Group_registry.remove ~name:"mainnet-prod" in
      Alcotest.(check bool) "remove succeeds" true (Result.is_ok result) ;
      match Group_registry.find ~name:"mainnet-prod" with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "Group still exists after remove"
      | Error (`Msg msg) -> Alcotest.failf "find failed: %s" msg)

let test_registry_remove_nonexistent () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      let result = Group_registry.remove ~name:"nonexistent" in
      Alcotest.(check bool) "remove nonexistent ok" true (Result.is_ok result))

let test_registry_overwrite () =
  if is_ci () then Alcotest.skip () ;
  with_fake_xdg (fun () ->
      let g1 = sample_group ~network:"mainnet" () in
      let _ = Group_registry.write g1 in
      let g2 = {g1 with network = "ghostnet"} in
      let _ = Group_registry.write g2 in
      match Group_registry.find ~name:"mainnet-prod" with
      | Ok (Some found) ->
          Alcotest.(check string)
            "network updated"
            "ghostnet"
            found.Group.network
      | Ok None -> Alcotest.fail "Group not found after overwrite"
      | Error (`Msg msg) -> Alcotest.failf "find failed: %s" msg)

(* ========================================================================= *)
(* Service.t group field tests *)
(* ========================================================================= *)

let test_service_group_field_none_by_default () =
  let svc =
    Service.make
      ~instance:"test"
      ~role:"node"
      ~network:"mainnet"
      ~history_mode:History_mode.Rolling
      ~data_dir:"/tmp"
      ~rpc_addr:(Rpc_addr.of_string "127.0.0.1:8732")
      ~net_addr:"0.0.0.0:9732"
      ~service_user:"tezos"
      ~app_bin_dir:"/opt/octez"
      ~logging_mode:Logging_mode.Journald
      ()
  in
  Alcotest.(check (option string)) "group default None" None svc.Service.group

let test_service_group_field_roundtrip () =
  let svc =
    Service.make
      ~instance:"test"
      ~role:"node"
      ~network:"mainnet"
      ~history_mode:History_mode.Rolling
      ~data_dir:"/tmp"
      ~rpc_addr:(Rpc_addr.of_string "127.0.0.1:8732")
      ~net_addr:"0.0.0.0:9732"
      ~service_user:"tezos"
      ~app_bin_dir:"/opt/octez"
      ~logging_mode:Logging_mode.Journald
      ~group:(Some "my-group")
      ()
  in
  let json = Service.to_yojson svc in
  match Service.of_yojson json with
  | Ok decoded ->
      Alcotest.(check (option string))
        "group preserved"
        (Some "my-group")
        decoded.Service.group
  | Error (`Msg msg) -> Alcotest.failf "roundtrip failed: %s" msg

let test_service_group_field_backward_compat () =
  (* Simulate a legacy JSON without the group field *)
  let svc =
    Service.make
      ~instance:"legacy"
      ~role:"node"
      ~network:"mainnet"
      ~history_mode:History_mode.Full
      ~data_dir:"/tmp"
      ~rpc_addr:(Rpc_addr.of_string "127.0.0.1:8732")
      ~net_addr:"0.0.0.0:9732"
      ~service_user:"tezos"
      ~app_bin_dir:"/opt/octez"
      ~logging_mode:Logging_mode.Journald
      ()
  in
  let json = Service.to_yojson svc in
  (* Remove the group field to simulate legacy JSON *)
  let json_without_group =
    match json with
    | `Assoc fields -> `Assoc (List.filter (fun (k, _) -> k <> "group") fields)
    | other -> other
  in
  match Service.of_yojson json_without_group with
  | Ok decoded ->
      Alcotest.(check (option string))
        "group defaults to None"
        None
        decoded.Service.group
  | Error (`Msg msg) -> Alcotest.failf "backward compat failed: %s" msg

(* ========================================================================= *)
(* Test Suite Registration *)
(* ========================================================================= *)

let () =
  Alcotest.run
    "Group Tests"
    [
      ( "group.type",
        [
          Alcotest.test_case "roundtrip JSON" `Quick test_group_roundtrip_json;
          Alcotest.test_case
            "roundtrip raw_path"
            `Quick
            test_group_roundtrip_raw_path;
          Alcotest.test_case
            "roundtrip registered_alias"
            `Quick
            test_group_roundtrip_registered_alias;
          Alcotest.test_case "make" `Quick test_group_make;
          Alcotest.test_case "invalid JSON" `Quick test_group_invalid_json;
        ] );
      ( "group_registry",
        [
          Alcotest.test_case "write and find" `Quick test_registry_write_and_find;
          Alcotest.test_case "list empty" `Quick test_registry_list_empty;
          Alcotest.test_case "list multiple" `Quick test_registry_list_multiple;
          Alcotest.test_case "find missing" `Quick test_registry_find_missing;
          Alcotest.test_case "remove" `Quick test_registry_remove;
          Alcotest.test_case
            "remove nonexistent"
            `Quick
            test_registry_remove_nonexistent;
          Alcotest.test_case "overwrite" `Quick test_registry_overwrite;
        ] );
      ( "service.group_field",
        [
          Alcotest.test_case
            "default None"
            `Quick
            test_service_group_field_none_by_default;
          Alcotest.test_case
            "roundtrip"
            `Quick
            test_service_group_field_roundtrip;
          Alcotest.test_case
            "backward compat"
            `Quick
            test_service_group_field_backward_compat;
        ] );
    ]
