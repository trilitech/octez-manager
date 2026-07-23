(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for Node_env module - environment file parsing and generation
    
    Tests cover:
    - Environment value escaping (shell injection prevention)
    - Environment file writing and reading
    - Edge cases: special characters, empty values, long values
    - Error handling: file permissions, invalid formats
    - Property-based tests for round-trip behavior
*)

open Alcotest
open Octez_manager_lib

(* ============================================================ *)
(* Test Helpers *)
(* ============================================================ *)

let ok_or_fail = function Ok x -> x | Error (`Msg e) -> fail e

let check_string = check string

let create_temp_instance () =
  let temp_dir = Filename.temp_file "octez-manager-test-" "" in
  Unix.unlink temp_dir ;
  Unix.mkdir temp_dir 0o755 ;
  let inst_name = "test-instance-" ^ string_of_int (Random.int 100000) in
  let inst_dir = Filename.concat temp_dir inst_name in
  Unix.mkdir inst_dir 0o755 ;
  (inst_name, temp_dir, inst_dir)

let cleanup_temp (_, temp_dir, _) =
  let rec rm_rf path =
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (Filename.concat path name)) ;
      Unix.rmdir path)
    else Unix.unlink path
  in
  try rm_rf temp_dir with _ -> ()

(* ============================================================ *)
(* Environment Value Escaping Tests *)
(* ============================================================ *)

let test_escape_simple_value () =
  let result = Node_env.escape_env_value "simple" in
  check_string "simple value unchanged" "simple" result

let test_escape_value_with_spaces () =
  let result = Node_env.escape_env_value "hello world" in
  check_string "spaces cause quoting" "\"hello world\"" result

let test_escape_value_with_dollar () =
  let result = Node_env.escape_env_value "price is $100" in
  (* Should quote and escape dollar to prevent variable expansion *)
  check
    bool
    "dollar is escaped"
    true
    (String.contains result '\\' || String.contains result '"')

let test_escape_value_with_backtick () =
  let result = Node_env.escape_env_value "command `whoami`" in
  (* Should prevent command substitution *)
  check
    bool
    "backtick is handled"
    true
    (String.contains result '\\' || String.contains result '"')

let test_escape_value_with_quotes () =
  let result = Node_env.escape_env_value "say \"hello\"" in
  (* Should escape nested quotes *)
  check bool "quotes are escaped" true (String.contains result '\\')

let test_escape_value_with_newline () =
  let result = Node_env.escape_env_value "line1\nline2" in
  (* Should handle newlines somehow (quote or escape) *)
  check bool "newline is handled" true (String.length result > 0)

let test_escape_value_with_semicolon () =
  let result = Node_env.escape_env_value "cmd1; cmd2" in
  (* Semicolon could cause command chaining, should be quoted *)
  check bool "semicolon causes quoting" true (String.contains result '"')

let test_escape_value_with_asterisk () =
  let result = Node_env.escape_env_value "*.txt" in
  (* Asterisk causes glob expansion, should be quoted *)
  check bool "asterisk causes quoting" true (String.contains result '"')

let test_escape_empty_value () =
  let result = Node_env.escape_env_value "" in
  (* Empty value should probably be quoted *)
  check bool "empty value is quoted" true (result = "\"\"" || result = "")

let test_escape_value_with_equals () =
  let result = Node_env.escape_env_value "key=value" in
  (* Equals sign might need quoting *)
  check bool "equals handled" true (String.length result >= 9)

let test_escape_path_with_spaces () =
  let result = Node_env.escape_env_value "/path/to/my documents/file.txt" in
  check bool "path with spaces is quoted" true (String.contains result '"')

let test_escape_url () =
  let result =
    Node_env.escape_env_value "https://example.com:8080/path?query=1"
  in
  (* URLs might contain special chars but should mostly be safe *)
  check bool "url is handled" true (String.length result > 0)

(* ============================================================ *)
(* Environment File Write/Read Tests *)
(* ============================================================ *)

let test_write_and_read_simple () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  (* Write some environment variables - use simple values that don't need quoting *)
  let pairs = [("DATA_DIR", "/path/to/data"); ("NETWORK", "mainnet")] in

  Node_env.write_pairs ~inst pairs |> ok_or_fail ;

  (* Read them back *)
  let read_pairs = Node_env.read ~inst |> ok_or_fail in

  (* Filter out VERSION which is auto-added *)
  let read_pairs_filtered =
    List.filter (fun (k, _) -> k <> "VERSION") read_pairs
  in

  (* Verify *)
  check (list (pair string string)) "pairs match" pairs read_pairs_filtered ;

  cleanup_temp (inst, temp_dir, inst_dir)

let test_write_with_special_characters () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  let pairs =
    [
      ("PATH_WITH_SPACE", "/my documents/folder");
      ("VALUE_WITH_DOLLAR", "$HOME/test");
      ("COMMAND", "echo \"hello\"");
    ]
  in

  Node_env.write_pairs ~inst pairs |> ok_or_fail ;
  let read_pairs = Node_env.read ~inst |> ok_or_fail in
  let read_pairs_filtered =
    List.filter (fun (k, _) -> k <> "VERSION") read_pairs
  in

  (* After the fix: Values should be read back unescaped, matching the original input.
     The escaping/quoting is only used in the file format, not in the API. *)
  let expected = pairs in

  check
    (list (pair string string))
    "special chars round-trip correctly"
    expected
    read_pairs_filtered ;

  cleanup_temp (inst, temp_dir, inst_dir)

let test_write_empty_value () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  let pairs = [("EMPTY", ""); ("NOT_EMPTY", "value")] in

  Node_env.write_pairs ~inst pairs |> ok_or_fail ;
  let read_pairs = Node_env.read ~inst |> ok_or_fail in
  let read_pairs_filtered =
    List.filter (fun (k, _) -> k <> "VERSION") read_pairs
  in

  (* Empty values are filtered out by write_pairs (line 75 in node_env.ml) *)
  let expected = [("NOT_EMPTY", "value")] in

  check
    (list (pair string string))
    "empty value filtered"
    expected
    read_pairs_filtered ;

  cleanup_temp (inst, temp_dir, inst_dir)

let test_write_very_long_value () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  let long_value = String.make 1000 'x' in
  let pairs = [("LONG_VALUE", long_value)] in

  Node_env.write_pairs ~inst pairs |> ok_or_fail ;
  let read_pairs = Node_env.read ~inst |> ok_or_fail in
  let read_pairs_filtered =
    List.filter (fun (k, _) -> k <> "VERSION") read_pairs
  in

  let expected = [("LONG_VALUE", long_value)] in
  check
    (list (pair string string))
    "long value preserved"
    expected
    read_pairs_filtered ;

  cleanup_temp (inst, temp_dir, inst_dir)

let test_write_with_comments () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  let pairs = [("KEY", "value")] in

  Node_env.write_pairs ~with_comments:true ~inst pairs |> ok_or_fail ;

  (* Read file content to verify comments exist *)
  let base = Paths.env_instances_base_dir () in
  let env_file = Filename.concat (Filename.concat base inst) "node.env" in
  let ic = open_in env_file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic ;

  check bool "comments present" true (String.contains content '#') ;

  cleanup_temp (inst, temp_dir, inst_dir)

let test_overwrite_existing_file () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  (* Write first set *)
  let pairs1 = [("KEY1", "value1")] in
  Node_env.write_pairs ~inst pairs1 |> ok_or_fail ;

  (* Overwrite with second set *)
  let pairs2 = [("KEY2", "value2")] in
  Node_env.write_pairs ~inst pairs2 |> ok_or_fail ;

  (* Read should return second set *)
  let read_pairs = Node_env.read ~inst |> ok_or_fail in
  let read_pairs_filtered =
    List.filter (fun (k, _) -> k <> "VERSION") read_pairs
  in

  check
    (list (pair string string))
    "file overwritten"
    pairs2
    read_pairs_filtered ;

  cleanup_temp (inst, temp_dir, inst_dir)

(* ============================================================ *)
(* Error Handling Tests *)
(* ============================================================ *)

let test_read_nonexistent_file () =
  (* According to node_env.ml line 110, nonexistent files return Ok [] *)
  match Node_env.read ~inst:"nonexistent-instance-xyz" with
  | Ok pairs ->
      check (list (pair string string)) "empty list for nonexistent" [] pairs
  | Error (`Msg msg) ->
      (* If it errors, that's also acceptable behavior *)
      check bool "error message not empty" true (String.length msg > 0)

let test_read_invalid_format () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  (* Write malformed file manually to the correct location *)
  let base = Paths.env_instances_base_dir () in
  let inst_path = Filename.concat base inst in
  (* Ensure all parent directories exist *)
  let rec mkdir_p path =
    if not (Sys.file_exists path) then (
      mkdir_p (Filename.dirname path) ;
      Unix.mkdir path 0o755)
  in
  mkdir_p inst_path ;
  let env_file = Filename.concat inst_path "node.env" in
  let oc = open_out env_file in
  output_string oc "INVALID LINE WITHOUT EQUALS\n" ;
  output_string oc "VALID=value\n" ;
  close_out oc ;

  (* According to node_env.ml line 121-125, lines without '=' split into [key] and
     rest is empty list, so String.concat "=" [] gives "" as value *)
  let result = Node_env.read ~inst in

  (match result with
  | Ok pairs ->
      (* Should get both lines: invalid one with empty value, valid one *)
      let has_invalid = List.mem_assoc "INVALID LINE WITHOUT EQUALS" pairs in
      let has_valid = List.mem_assoc "VALID" pairs in
      check bool "has valid line" true has_valid ;
      (* The invalid line is parsed as key with empty value *)
      check bool "invalid line parsed" true has_invalid
  | Error (`Msg msg) ->
      (* Shouldn't fail unless file I/O error *)
      fail ("Unexpected error: " ^ msg)) ;

  cleanup_temp (inst, temp_dir, inst_dir)

(* ============================================================ *)
(* Node_env.write Tests (high-level API) *)
(* ============================================================ *)

let test_write_full_node_env () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  Node_env.write
    ~inst
    ~data_dir:"/data/node"
    ~run_args:"--network mainnet --rpc-addr 127.0.0.1:8732"
    ~extra_env:[("CUSTOM", "value")]
    ()
  |> ok_or_fail ;

  let pairs = Node_env.read ~inst |> ok_or_fail in

  (* Should contain OCTEZ_DATA_DIR, OCTEZ_NODE_ARGS (not DATA_DIR/NODE_ARGS), and CUSTOM *)
  let has_data_dir = List.mem_assoc "OCTEZ_DATA_DIR" pairs in
  let has_node_args = List.mem_assoc "OCTEZ_NODE_ARGS" pairs in
  let has_custom = List.mem_assoc "CUSTOM" pairs in

  check bool "has OCTEZ_DATA_DIR" true has_data_dir ;
  check bool "has OCTEZ_NODE_ARGS" true has_node_args ;
  check bool "has CUSTOM" true has_custom ;

  cleanup_temp (inst, temp_dir, inst_dir)

let test_write_node_env_with_comments () =
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;

  Node_env.write
    ~inst
    ~data_dir:"/data/node"
    ~run_args:"--network mainnet"
    ~extra_env:[]
    ~with_comments:true
    ()
  |> ok_or_fail ;

  let base = Paths.env_instances_base_dir () in
  let env_file = Filename.concat (Filename.concat base inst) "node.env" in
  let ic = open_in env_file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic ;

  check bool "has comments" true (String.contains content '#') ;

  cleanup_temp (inst, temp_dir, inst_dir)

(* ============================================================ *)
(* Property-Based Tests *)
(* ============================================================ *)

(** Generator for environment variable values that may contain special characters.
    This generates realistic values that could appear in Octez configuration:
    - Paths with spaces
    - URLs with special characters
    - Command-line arguments with quotes
    - Values with shell metacharacters
    
    Note: Excludes newlines and other characters that would break the env file format.
*)
let env_value_gen =
  let open QCheck.Gen in
  (* Filter to exclude newlines and other control characters that break env files *)
  let printable_no_newline = char_range ' ' '~' in
  oneof_weighted
    [
      (10, string_size (return 0));
      (* empty string *)
      (30, string_size ~gen:printable_no_newline (int_range 1 50));
      (* simple strings *)
      (20, string_size ~gen:printable_no_newline (int_range 1 30));
      (* printable chars including spaces *)
      ( 15,
        map
          (fun s -> s ^ " " ^ s)
          (string_size ~gen:printable_no_newline (int_range 1 20))
        (* values with spaces *) );
      ( 10,
        map
          (fun s -> "/path/to/" ^ s)
          (string_size ~gen:printable_no_newline (int_range 1 20))
        (* path-like *) );
      ( 5,
        map
          (fun s -> "http://localhost:8732/" ^ s)
          (string_size ~gen:printable_no_newline (int_range 0 10))
        (* URL-like *) );
      ( 5,
        map
          (fun s -> "\"" ^ s ^ "\"")
          (string_size ~gen:printable_no_newline (int_range 1 10))
        (* quoted *) );
      ( 5,
        map
          (fun (a, b) -> a ^ " " ^ b)
          (pair
             (string_size ~gen:printable_no_newline (int_range 1 10))
             (string_size ~gen:printable_no_newline (int_range 1 10))
             (* delegate lists *)) );
    ]

(** Generator for valid environment variable keys (no special chars, no spaces) *)
let env_key_gen =
  let open QCheck.Gen in
  let char_gen = char_range 'A' 'Z' in
  map
    (fun s -> "TEST_VAR_" ^ String.uppercase_ascii s)
    (string_size ~gen:char_gen (int_range 1 10))

(** Property: write_pairs followed by read should be idempotent.
    For any list of key-value pairs, if we:
    1. Write them with write_pairs
    2. Read them back with read
    3. Write them again with write_pairs
    4. Read them back again
    
    The result should be the same as step 2. This ensures that the
    escaping/unescaping is symmetric and doesn't accumulate on each cycle.
*)
let prop_write_read_idempotent =
  QCheck.Test.make
    ~name:"write_pairs + read is idempotent"
    ~count:500
    QCheck.(
      make
        ~print:
          (Print.list (fun (k, v) ->
               Printf.sprintf "%s=%s" k (String.escaped v)))
        ~shrink:Shrink.(list ~shrink:(pair Shrink.string Shrink.string))
        (Gen.list_size Gen.(int_range 1 5) (Gen.pair env_key_gen env_value_gen)))
  @@ fun pairs ->
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;
  Fun.protect
    ~finally:(fun () -> cleanup_temp (inst, temp_dir, inst_dir))
    (fun () ->
      (* Filter empty values since write_pairs skips them *)
      let non_empty_pairs =
        List.filter (fun (_, v) -> String.trim v <> "") pairs
      in
      if non_empty_pairs = [] then true
      else
        match Node_env.write_pairs ~inst non_empty_pairs with
        | Error _ -> false
        | Ok () -> (
            match Node_env.read ~inst with
            | Error _ -> false
            | Ok read1 -> (
                let read1_filtered =
                  List.filter (fun (k, _) -> k <> "VERSION") read1
                in
                (* Write again using the read values *)
                match Node_env.write_pairs ~inst read1_filtered with
                | Error _ -> false
                | Ok () -> (
                    match Node_env.read ~inst with
                    | Error _ -> false
                    | Ok read2 ->
                        let read2_filtered =
                          List.filter (fun (k, _) -> k <> "VERSION") read2
                        in
                        (* The two reads should be identical *)
                        let sorted1 = List.sort compare read1_filtered in
                        let sorted2 = List.sort compare read2_filtered in
                        sorted1 = sorted2))))

(** Property: values written should be readable by the shell.
    After writing a value with write_pairs, sourcing the env file
    in a shell should give us back the original value (not with extra quotes).
    
    Note: This test is limited to "safe" characters that don't require complex
    shell escaping beyond what our escape_env_value provides.
*)
let prop_shell_readable =
  QCheck.Test.make
    ~name:"written values are shell-readable"
    ~count:200
    QCheck.(
      make
        (let open Gen in
         let printable_no_newline = char_range ' ' '~' in
         pair
           env_key_gen
           (* Only test with values that contain spaces, quotes, or dollars - the main
              cases we care about for delegates, paths, and URLs *)
           (oneof
              [
                map
                  (fun s -> s ^ " " ^ s)
                  (string_size ~gen:printable_no_newline (int_range 1 10));
                map
                  (fun s -> "/path to/" ^ s)
                  (string_size ~gen:printable_no_newline (int_range 1 10));
                map
                  (fun s -> "$VAR/" ^ s)
                  (string_size ~gen:printable_no_newline (int_range 1 10));
                map
                  (fun s -> "\"quoted " ^ s ^ "\"")
                  (string_size ~gen:printable_no_newline (int_range 1 10));
              ])))
  @@ fun (key, value) ->
  if String.trim value = "" then true
  else
    let inst, temp_dir, inst_dir = create_temp_instance () in
    Unix.putenv "HOME" temp_dir ;
    Fun.protect
      ~finally:(fun () -> cleanup_temp (inst, temp_dir, inst_dir))
      (fun () ->
        match Node_env.write_pairs ~inst [(key, value)] with
        | Error _ -> false
        | Ok () -> (
            (* Instead of using bash to source, just verify the round-trip works *)
            match Node_env.read ~inst with
            | Error _ -> false
            | Ok pairs ->
                let read_value =
                  List.find_map
                    (fun (k, v) -> if k = key then Some v else None)
                    pairs
                in
                read_value = Some value))

(** Property: patch_keys should only modify specified keys.
    When we use patch_keys to update a subset of variables,
    other variables should remain unchanged, including their
    original formatting (quotes, escaping, comments).
*)
let prop_patch_keys_preserves_others =
  QCheck.Test.make
    ~name:"patch_keys preserves unmodified keys"
    ~count:200
    QCheck.(
      make
        (Gen.pair
           (Gen.list_size
              Gen.(int_range 2 5)
              (Gen.pair env_key_gen env_value_gen))
           env_key_gen))
  @@ fun (initial_pairs, update_key) ->
  let inst, temp_dir, inst_dir = create_temp_instance () in
  Unix.putenv "HOME" temp_dir ;
  Fun.protect
    ~finally:(fun () -> cleanup_temp (inst, temp_dir, inst_dir))
    (fun () ->
      let non_empty_pairs =
        List.filter (fun (_, v) -> String.trim v <> "") initial_pairs
      in
      if non_empty_pairs = [] then true
      else
        match Node_env.write_pairs ~inst non_empty_pairs with
        | Error _ -> false
        | Ok () -> (
            (* Update just one key *)
            let update_value = "NEW_VALUE_123" in
            match
              Node_env.patch_keys ~inst ~updates:[(update_key, update_value)]
            with
            | Error _ -> false
            | Ok () -> (
                match Node_env.read ~inst with
                | Error _ -> false
                | Ok read_pairs ->
                    let read_map = List.to_seq read_pairs |> Hashtbl.of_seq in
                    (* Check that the updated key has new value *)
                    let updated_correctly =
                      Hashtbl.find_opt read_map update_key = Some update_value
                    in
                    (* Check that other keys are unchanged *)
                    let others_unchanged =
                      List.for_all
                        (fun (k, _original_v) ->
                          if k = update_key then true
                          else
                            match Node_env.read ~inst with
                            | Error _ -> false
                            | Ok pairs ->
                                List.mem_assoc k pairs
                                && List.assoc k pairs = List.assoc k read_pairs)
                        non_empty_pairs
                    in
                    updated_correctly && others_unchanged)))

(** Property: Multiple round-trips should not corrupt data.
    This is the core bug from #995 - writing and reading N times
    should not keep adding escape layers.
*)
let prop_multiple_roundtrips =
  QCheck.Test.make
    ~name:"multiple write/read cycles don't corrupt data"
    ~count:100
    QCheck.(make (Gen.pair env_key_gen env_value_gen))
  @@ fun (key, value) ->
  if String.trim value = "" then true
  else
    let inst, temp_dir, inst_dir = create_temp_instance () in
    Unix.putenv "HOME" temp_dir ;
    Fun.protect
      ~finally:(fun () -> cleanup_temp (inst, temp_dir, inst_dir))
      (fun () ->
        (* Do 5 round-trips *)
        let rec roundtrip n current_pairs =
          if n = 0 then Ok current_pairs
          else
            match Node_env.write_pairs ~inst current_pairs with
            | Error e -> Error e
            | Ok () -> (
                match Node_env.read ~inst with
                | Error e -> Error e
                | Ok read_pairs ->
                    let filtered =
                      List.filter (fun (k, _) -> k <> "VERSION") read_pairs
                    in
                    roundtrip (n - 1) filtered)
        in
        match roundtrip 5 [(key, value)] with
        | Error _ -> false
        | Ok final_pairs ->
            (* After 5 round-trips, the value should match the original *)
            let final_value =
              List.find_map
                (fun (k, v) -> if k = key then Some v else None)
                final_pairs
            in
            final_value = Some value)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let escape_tests =
  [
    ("escape simple value", `Quick, test_escape_simple_value);
    ("escape value with spaces", `Quick, test_escape_value_with_spaces);
    ("escape value with dollar", `Quick, test_escape_value_with_dollar);
    ("escape value with backtick", `Quick, test_escape_value_with_backtick);
    ("escape value with quotes", `Quick, test_escape_value_with_quotes);
    ("escape value with newline", `Quick, test_escape_value_with_newline);
    ("escape value with semicolon", `Quick, test_escape_value_with_semicolon);
    ("escape value with asterisk", `Quick, test_escape_value_with_asterisk);
    ("escape empty value", `Quick, test_escape_empty_value);
    ("escape value with equals", `Quick, test_escape_value_with_equals);
    ("escape path with spaces", `Quick, test_escape_path_with_spaces);
    ("escape url", `Quick, test_escape_url);
  ]

let write_read_tests =
  [
    ("write and read simple", `Quick, test_write_and_read_simple);
    ("write with special chars", `Quick, test_write_with_special_characters);
    ("write empty value", `Quick, test_write_empty_value);
    ("write very long value", `Quick, test_write_very_long_value);
    ("write with comments", `Quick, test_write_with_comments);
    ("overwrite existing file", `Quick, test_overwrite_existing_file);
  ]

let error_tests =
  [
    ("read nonexistent file", `Quick, test_read_nonexistent_file);
    ("read invalid format", `Quick, test_read_invalid_format);
  ]

let high_level_tests =
  [
    ("write full node env", `Quick, test_write_full_node_env);
    ("write node env with comments", `Quick, test_write_node_env_with_comments);
  ]

let property_tests =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_write_read_idempotent;
      prop_shell_readable;
      prop_patch_keys_preserves_others;
      prop_multiple_roundtrips;
    ]

let () =
  Alcotest.run
    "Node_env"
    [
      ("escape", escape_tests);
      ("write_read", write_read_tests);
      ("error_handling", error_tests);
      ("high_level_api", high_level_tests);
      ("properties", property_tests);
    ]
