(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for registry modules: directory_registry and keys_reader *)

open Octez_manager_lib

(* ========================================================================= *)
(* Test Helpers *)
(* ========================================================================= *)

let with_temp_dir f =
  let base = Filename.temp_file "octez_manager" "test" in
  Sys.remove base ;
  Unix.mkdir base 0o755 ;
  Fun.protect
    ~finally:(fun () ->
      let (_ : (unit, [> Rresult.R.msg]) result) = Common.remove_tree base in
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

let r_msg =
  let pp fmt (`Msg msg) = Format.fprintf fmt "Error: %s" msg in
  let eq (`Msg a) (`Msg b) = String.equal a b in
  Alcotest.testable pp eq

(* ========================================================================= *)
(* Directory Registry Tests *)
(* ========================================================================= *)

let test_add_and_find () =
  with_fake_xdg (fun () ->
      let open Directory_registry in
      (* Add an entry *)
      let result =
        add
          ~path:"/data/test-node"
          ~dir_type:Node_data_dir
          ~linked_services:["node-main"]
      in
      Alcotest.(check (result unit r_msg)) "Add succeeds" (Ok ()) result ;

      (* Find it *)
      let found = find_by_path "/data/test-node" in
      Alcotest.(check bool) "Entry found" true (Result.is_ok found) ;

      match Result.get_ok found with
      | Some entry ->
          Alcotest.(check string) "Path matches" "/data/test-node" entry.path ;
          Alcotest.(check bool)
            "Type is Node_data_dir"
            true
            (entry.dir_type = Node_data_dir) ;
          Alcotest.(check (list string))
            "Services match"
            ["node-main"]
            entry.linked_services
      | None -> Alcotest.fail "Entry not found after add")

let test_json_roundtrip () =
  let open Directory_registry in
  let entry =
    {
      path = "/data/node1";
      dir_type = Node_data_dir;
      created_at = "2026-01-01T00:00:00Z";
      last_used_at = "2026-01-01T00:00:00Z";
      linked_services = ["node-main"; "baker-main"];
    }
  in
  let json = For_test.directory_entry_to_yojson entry in
  let result = For_test.directory_entry_of_yojson json in
  match result with
  | Ok decoded ->
      Alcotest.(check string) "Path" entry.path decoded.path ;
      Alcotest.(check bool) "Dir type" true (entry.dir_type = decoded.dir_type) ;
      Alcotest.(check (list string))
        "Services"
        entry.linked_services
        decoded.linked_services
  | Error (`Msg msg) -> Alcotest.fail ("JSON roundtrip failed: " ^ msg)

let test_entry_limiting () =
  with_fake_xdg (fun () ->
      let open Directory_registry in
      (* Add 15 entries of same type *)
      for i = 1 to 15 do
        let path = Printf.sprintf "/data/node%d" i in
        let _ = add ~path ~dir_type:Node_data_dir ~linked_services:[] in
        ()
      done ;

      (* List should be limited to 10 *)
      match list ~dir_type:Node_data_dir () with
      | Ok entries ->
          Alcotest.(check int) "Limited to 10 entries" 10 (List.length entries)
      | Error _ -> Alcotest.fail "List failed")

let test_remove () =
  with_fake_xdg (fun () ->
      let open Directory_registry in
      let path = "/data/removeme" in

      (* Add entry *)
      let _ = add ~path ~dir_type:Client_base_dir ~linked_services:[] in

      (* Verify it exists *)
      (match find_by_path path with
      | Ok (Some _) -> ()
      | _ -> Alcotest.fail "Entry should exist before removal") ;

      (* Remove it *)
      let result = remove path in
      Alcotest.(check (result unit r_msg)) "Remove succeeds" (Ok ()) result ;

      (* Verify it's gone *)
      match find_by_path path with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "Entry still exists after removal"
      | Error _ -> Alcotest.fail "Error checking after removal")

let test_update_linked_services () =
  with_fake_xdg (fun () ->
      let open Directory_registry in
      let path = "/data/node" in

      (* Add with initial services *)
      let _ =
        add ~path ~dir_type:Node_data_dir ~linked_services:["service-1"]
      in

      (* Update services *)
      let result =
        update_linked_services ~path ~linked_services:["service-1"; "service-2"]
      in
      Alcotest.(check (result unit r_msg)) "Update succeeds" (Ok ()) result ;

      (* Verify updated *)
      match find_by_path path with
      | Ok (Some entry) ->
          Alcotest.(check int)
            "Two services"
            2
            (List.length entry.linked_services)
      | _ -> Alcotest.fail "Entry not found after update")

let test_list_by_type () =
  with_fake_xdg (fun () ->
      let open Directory_registry in
      (* Add different types *)
      let _ =
        add ~path:"/data/node1" ~dir_type:Node_data_dir ~linked_services:[]
      in
      let _ =
        add ~path:"/data/client1" ~dir_type:Client_base_dir ~linked_services:[]
      in
      let _ =
        add ~path:"/data/node2" ~dir_type:Node_data_dir ~linked_services:[]
      in

      (* List only nodes *)
      match list ~dir_type:Node_data_dir () with
      | Ok entries ->
          Alcotest.(check int) "Two node entries" 2 (List.length entries)
      | Error _ -> Alcotest.fail "List failed")

(* ========================================================================= *)
(* Keys Reader Tests *)
(* ========================================================================= *)

let test_keys_valid_file () =
  with_temp_dir (fun base ->
      let keys_file = Filename.concat base "public_key_hashs" in
      let content =
        {|[
  {"name": "alice", "value": "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"},
  {"name": "bob", "value": "tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6"}
]|}
      in
      let oc = open_out keys_file in
      output_string oc content ;
      close_out oc ;

      match Keys_reader.read_public_key_hashes ~base_dir:base with
      | Ok keys ->
          Alcotest.(check int) "Two keys found" 2 (List.length keys) ;
          let alice = List.find (fun k -> k.Keys_reader.name = "alice") keys in
          Alcotest.(check string)
            "Alice key"
            "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"
            alice.value
      | Error (`Msg msg) -> Alcotest.fail ("Read failed: " ^ msg))

let test_keys_missing_file () =
  with_temp_dir (fun base ->
      match Keys_reader.read_public_key_hashes ~base_dir:base with
      | Ok keys ->
          Alcotest.(check int) "Returns empty list" 0 (List.length keys)
      | Error _ -> Alcotest.fail "Should return empty list for missing file")

let test_keys_malformed_json () =
  with_temp_dir (fun base ->
      let keys_file = Filename.concat base "public_key_hashs" in
      let oc = open_out keys_file in
      output_string oc "{invalid json}" ;
      close_out oc ;

      match Keys_reader.read_public_key_hashes ~base_dir:base with
      | Ok _ -> Alcotest.fail "Should fail on malformed JSON"
      | Error _ -> ())

let test_keys_empty_array () =
  with_temp_dir (fun base ->
      let keys_file = Filename.concat base "public_key_hashs" in
      let oc = open_out keys_file in
      output_string oc "[]" ;
      close_out oc ;

      match Keys_reader.read_public_key_hashes ~base_dir:base with
      | Ok keys -> Alcotest.(check int) "Empty array" 0 (List.length keys)
      | Error _ -> Alcotest.fail "Should handle empty array")

let test_keys_invalid_format () =
  with_temp_dir (fun base ->
      let keys_file = Filename.concat base "public_key_hashs" in
      let content = {|[{"name": "alice"}]|} in
      (* Missing "value" field *)
      let oc = open_out keys_file in
      output_string oc content ;
      close_out oc ;

      match Keys_reader.read_public_key_hashes ~base_dir:base with
      | Ok _ -> Alcotest.fail "Should fail on missing fields"
      | Error _ -> ())

(* ========================================================================= *)
(* Test Suite Registration *)
(* ========================================================================= *)

let () =
  Alcotest.run
    "Registry Tests"
    [
      ( "directory_registry",
        [
          Alcotest.test_case "add_and_find" `Quick test_add_and_find;
          Alcotest.test_case "json_roundtrip" `Quick test_json_roundtrip;
          Alcotest.test_case "entry_limiting" `Quick test_entry_limiting;
          Alcotest.test_case "remove" `Quick test_remove;
          Alcotest.test_case
            "update_linked_services"
            `Quick
            test_update_linked_services;
          Alcotest.test_case "list_by_type" `Quick test_list_by_type;
        ] );
      ( "keys_reader",
        [
          Alcotest.test_case "valid_file" `Quick test_keys_valid_file;
          Alcotest.test_case "missing_file" `Quick test_keys_missing_file;
          Alcotest.test_case "malformed_json" `Quick test_keys_malformed_json;
          Alcotest.test_case "empty_array" `Quick test_keys_empty_array;
          Alcotest.test_case "invalid_format" `Quick test_keys_invalid_format;
        ] );
    ]
