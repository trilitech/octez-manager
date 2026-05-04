(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for pure logic in form_builder.ml:
    parse_host_port, field validation, field display, with_hint.

    Uses the For_tests accessor module to reach through the abstract field type. *)

open Alcotest
module FB = Octez_manager_ui.Form_builder
module FT = Octez_manager_ui.Form_builder.For_tests

(* ================================================================== *)
(* Test model                                                          *)
(* ================================================================== *)

type model = {
  name : string;
  endpoint : string;
  enabled : bool;
  items : string list;
}

let empty () = {name = ""; endpoint = ""; enabled = false; items = []}

(* ================================================================== *)
(* parse_host_port unit tests                                          *)
(* ================================================================== *)

let test_php_valid_standard () =
  check
    (result unit string)
    "standard"
    (Ok ())
    (FT.parse_host_port "127.0.0.1:8732")

let test_php_valid_localhost () =
  check
    (result unit string)
    "localhost"
    (Ok ())
    (FT.parse_host_port "localhost:8732")

let test_php_valid_port_1 () =
  check (result unit string) "port 1" (Ok ()) (FT.parse_host_port "host:1")

let test_php_valid_port_65535 () =
  check
    (result unit string)
    "port 65535"
    (Ok ())
    (FT.parse_host_port "host:65535")

let test_php_invalid_port_0 () =
  match FT.parse_host_port "host:0" with
  | Error _ -> ()
  | Ok () -> fail "port 0 should fail"

let test_php_invalid_port_65536 () =
  match FT.parse_host_port "host:65536" with
  | Error _ -> ()
  | Ok () -> fail "port 65536 should fail"

let test_php_invalid_port_negative () =
  match FT.parse_host_port "host:-1" with
  | Error _ -> ()
  | Ok () -> fail "negative port should fail"

let test_php_invalid_port_alpha () =
  match FT.parse_host_port "host:abc" with
  | Error msg -> check bool "mentions port" true (String.length msg > 0)
  | Ok () -> fail "alpha port should fail"

let test_php_invalid_empty_host () =
  match FT.parse_host_port ":8732" with
  | Error _ -> ()
  | Ok () -> fail "empty host should fail"

let test_php_invalid_no_colon () =
  match FT.parse_host_port "localhost" with
  | Error msg ->
      check
        bool
        "mentions host:port"
        true
        (let needle = "host:port" in
         let nlen = String.length needle in
         let hlen = String.length msg in
         nlen <= hlen
         &&
         let found = ref false in
         for i = 0 to hlen - nlen do
           if (not !found) && String.sub msg i nlen = needle then found := true
         done ;
         !found)
  | Ok () -> fail "no colon should fail"

let test_php_invalid_multiple_colons () =
  match FT.parse_host_port "host:1:2" with
  | Error _ -> ()
  | Ok () -> fail "multiple colons should fail"

let test_php_invalid_empty_string () =
  match FT.parse_host_port "" with
  | Error _ -> ()
  | Ok () -> fail "empty string should fail"

let test_php_port_with_spaces () =
  (* int_of_string (String.trim " 8732") works *)
  check
    (result unit string)
    "trimmed port"
    (Ok ())
    (FT.parse_host_port "host: 8732")

(* ================================================================== *)
(* QCheck: parse_host_port properties                                  *)
(* ================================================================== *)

let test_php_qcheck_no_crash =
  let prop s =
    let _ = FT.parse_host_port s in
    true
  in
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"parse_host_port never crashes"
       QCheck.string
       prop)

let test_php_qcheck_valid_format =
  let gen =
    QCheck.Gen.(
      let* host =
        oneof
          [
            return "127.0.0.1";
            return "localhost";
            return "192.168.1.1";
            return "myhost";
          ]
      in
      let* port = int_range 1 65535 in
      return (Printf.sprintf "%s:%d" host port))
  in
  let prop s =
    match FT.parse_host_port s with Ok () -> true | Error _ -> false
  in
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:200
       ~name:"valid host:port always accepted"
       (QCheck.make gen)
       prop)

let test_php_qcheck_error_messages_nonempty =
  let prop s =
    match FT.parse_host_port s with
    | Ok () -> true
    | Error msg -> String.length msg > 0
  in
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:500
       ~name:"error messages are non-empty"
       QCheck.string
       prop)

(* ================================================================== *)
(* endpoint field validation tests                                     *)
(* ================================================================== *)

let mk_endpoint_field () =
  FB.endpoint
    ~label:"RPC"
    ~get:(fun m -> m.endpoint)
    ~set:(fun v m -> {m with endpoint = v})
    ()

let test_endpoint_empty_valid () =
  let f = mk_endpoint_field () in
  check bool "empty is valid" true (FT.field_validate f (empty ()))

let test_endpoint_whitespace_valid () =
  let f = mk_endpoint_field () in
  check
    bool
    "spaces valid"
    true
    (FT.field_validate f {(empty ()) with endpoint = "   "})

let test_endpoint_good_value () =
  let f = mk_endpoint_field () in
  check
    bool
    "good value"
    true
    (FT.field_validate f {(empty ()) with endpoint = "127.0.0.1:8732"})

let test_endpoint_bad_value () =
  let f = mk_endpoint_field () in
  check
    bool
    "bad value"
    false
    (FT.field_validate f {(empty ()) with endpoint = "not-valid"})

let test_endpoint_bad_msg () =
  let f = mk_endpoint_field () in
  let msg = FT.field_validate_msg f {(empty ()) with endpoint = "not-valid"} in
  check bool "has error msg" true (msg <> None)

let test_endpoint_good_msg () =
  let f = mk_endpoint_field () in
  let msg =
    FT.field_validate_msg f {(empty ()) with endpoint = "127.0.0.1:8732"}
  in
  check (option string) "no error msg" None msg

let test_endpoint_empty_msg () =
  let f = mk_endpoint_field () in
  let msg = FT.field_validate_msg f (empty ()) in
  check (option string) "no error on empty" None msg

(* ================================================================== *)
(* validated_text field tests                                          *)
(* ================================================================== *)

let mk_validated_name () =
  FB.validated_text
    ~label:"Name"
    ~get:(fun m -> m.name)
    ~set:(fun v m -> {m with name = v})
    ~validate:(fun m ->
      if String.length m.name > 0 then Ok () else Error "Name is required")

let test_validated_empty_fails () =
  let f = mk_validated_name () in
  check bool "empty fails" false (FT.field_validate f (empty ()))

let test_validated_empty_msg () =
  let f = mk_validated_name () in
  check
    (option string)
    "error message"
    (Some "Name is required")
    (FT.field_validate_msg f (empty ()))

let test_validated_filled_passes () =
  let f = mk_validated_name () in
  check
    bool
    "filled passes"
    true
    (FT.field_validate f {(empty ()) with name = "Alice"})

let test_validated_filled_msg () =
  let f = mk_validated_name () in
  check
    (option string)
    "no msg"
    None
    (FT.field_validate_msg f {(empty ()) with name = "Alice"})

(* ================================================================== *)
(* text, toggle, readonly: always valid                                *)
(* ================================================================== *)

let test_text_always_valid () =
  let f =
    FB.text
      ~label:"Name"
      ~get:(fun m -> m.name)
      ~set:(fun v m -> {m with name = v})
  in
  check bool "empty valid" true (FT.field_validate f (empty ())) ;
  check
    bool
    "filled valid"
    true
    (FT.field_validate f {(empty ()) with name = "x"})

let test_toggle_always_valid () =
  let f =
    FB.toggle
      ~label:"Enable"
      ~get:(fun m -> m.enabled)
      ~set:(fun v m -> {m with enabled = v})
  in
  check bool "false valid" true (FT.field_validate f (empty ())) ;
  check
    bool
    "true valid"
    true
    (FT.field_validate f {(empty ()) with enabled = true})

let test_readonly_always_valid () =
  let f = FB.readonly ~label:"Status" ~get:(fun m -> m.name) in
  check bool "readonly valid" true (FT.field_validate f (empty ()))

(* ================================================================== *)
(* field_get_string tests                                              *)
(* ================================================================== *)

let test_text_get_string () =
  let f =
    FB.text
      ~label:"Name"
      ~get:(fun m -> m.name)
      ~set:(fun v m -> {m with name = v})
  in
  check string "empty" "" (FT.field_get_string f (empty ())) ;
  check
    string
    "filled"
    "Alice"
    (FT.field_get_string f {(empty ()) with name = "Alice"})

let test_toggle_get_string () =
  let f =
    FB.toggle
      ~label:"Enabled"
      ~get:(fun m -> m.enabled)
      ~set:(fun v m -> {m with enabled = v})
  in
  check string "false" "false" (FT.field_get_string f (empty ())) ;
  check
    string
    "true"
    "true"
    (FT.field_get_string f {(empty ()) with enabled = true})

let test_readonly_get_string () =
  let f = FB.readonly ~label:"Computed" ~get:(fun m -> "hi " ^ m.name) in
  check string "computed" "hi " (FT.field_get_string f (empty ())) ;
  check
    string
    "computed filled"
    "hi Bob"
    (FT.field_get_string f {(empty ()) with name = "Bob"})

(* ================================================================== *)
(* field_label tests                                                   *)
(* ================================================================== *)

let test_field_label () =
  let f1 =
    FB.text
      ~label:"First"
      ~get:(fun m -> m.name)
      ~set:(fun v m -> {m with name = v})
  in
  let f2 =
    FB.toggle
      ~label:"Second"
      ~get:(fun m -> m.enabled)
      ~set:(fun v m -> {m with enabled = v})
  in
  let f3 = FB.readonly ~label:"Third" ~get:(fun m -> m.name) in
  check string "text label" "First" (FT.field_label f1) ;
  check string "toggle label" "Second" (FT.field_label f2) ;
  check string "readonly label" "Third" (FT.field_label f3)

(* ================================================================== *)
(* with_hint tests                                                     *)
(* ================================================================== *)

let test_no_hint_by_default () =
  let f =
    FB.text
      ~label:"Name"
      ~get:(fun m -> m.name)
      ~set:(fun v m -> {m with name = v})
  in
  check (option string) "no hint" None (FT.field_hint f)

let test_with_hint_sets_hint () =
  let f =
    FB.text
      ~label:"Name"
      ~get:(fun m -> m.name)
      ~set:(fun v m -> {m with name = v})
    |> FB.with_hint "Enter your name"
  in
  check (option string) "has hint" (Some "Enter your name") (FT.field_hint f)

let test_with_hint_overwrite () =
  let f =
    FB.text
      ~label:"Name"
      ~get:(fun m -> m.name)
      ~set:(fun v m -> {m with name = v})
    |> FB.with_hint "first" |> FB.with_hint "second"
  in
  check (option string) "last hint wins" (Some "second") (FT.field_hint f)

(* ================================================================== *)
(* service_or_endpoint validation tests                                *)
(* ================================================================== *)

let mk_soe_field () =
  FB.service_or_endpoint
    ~label:"Node"
    ~role:"node"
    ~get:(fun _m -> `None)
    ~set:(fun _v m -> m)
    ()

let test_soe_none_valid () =
  let f = mk_soe_field () in
  check bool "`None valid" true (FT.field_validate f (empty ()))

(* For service_or_endpoint, we need to test the validate logic with different
   get functions. Let's build fields that return different variants. *)

let test_soe_service_valid () =
  let f =
    FB.service_or_endpoint
      ~label:"Node"
      ~role:"node"
      ~get:(fun _m -> `Service "my-node")
      ~set:(fun _v m -> m)
      ()
  in
  check bool "`Service valid" true (FT.field_validate f (empty ()))

let test_soe_endpoint_valid () =
  let f =
    FB.service_or_endpoint
      ~label:"Node"
      ~role:"node"
      ~get:(fun _m -> `Endpoint "127.0.0.1:8732")
      ~set:(fun _v m -> m)
      ()
  in
  check bool "`Endpoint valid" true (FT.field_validate f (empty ()))

let test_soe_endpoint_invalid () =
  let f =
    FB.service_or_endpoint
      ~label:"Node"
      ~role:"node"
      ~get:(fun _m -> `Endpoint "bad")
      ~set:(fun _v m -> m)
      ()
  in
  check bool "`Endpoint invalid" false (FT.field_validate f (empty ()))

let test_soe_endpoint_invalid_msg () =
  let f =
    FB.service_or_endpoint
      ~label:"Node"
      ~role:"node"
      ~get:(fun _m -> `Endpoint "bad")
      ~set:(fun _v m -> m)
      ()
  in
  let msg = FT.field_validate_msg f (empty ()) in
  check bool "has error" true (msg <> None)

let test_soe_custom_validator () =
  let f =
    FB.service_or_endpoint
      ~label:"Node"
      ~role:"node"
      ~get:(fun _m -> `Endpoint "custom:999")
      ~set:(fun _v m -> m)
      ~endpoint_validator:(fun _s -> Error "custom error")
      ()
  in
  check bool "custom rejects" false (FT.field_validate f (empty ())) ;
  check
    (option string)
    "custom msg"
    (Some "custom error")
    (FT.field_validate_msg f (empty ()))

(* ================================================================== *)
(* string_list to_string tests                                         *)
(* ================================================================== *)

let test_string_list_none () =
  let f =
    FB.string_list
      ~label:"Items"
      ~get:(fun m -> m.items)
      ~set:(fun v m -> {m with items = v})
      ()
  in
  check string "empty → (none)" "(none)" (FT.field_get_string f (empty ()))

let test_string_list_one () =
  let f =
    FB.string_list
      ~label:"Items"
      ~get:(fun m -> m.items)
      ~set:(fun v m -> {m with items = v})
      ()
  in
  check
    string
    "one item"
    "alpha"
    (FT.field_get_string f {(empty ()) with items = ["alpha"]})

let test_string_list_two () =
  let f =
    FB.string_list
      ~label:"Items"
      ~get:(fun m -> m.items)
      ~set:(fun v m -> {m with items = v})
      ()
  in
  check
    string
    "two items"
    "a, b"
    (FT.field_get_string f {(empty ()) with items = ["a"; "b"]})

let test_string_list_many () =
  let f =
    FB.string_list
      ~label:"Items"
      ~get:(fun m -> m.items)
      ~set:(fun v m -> {m with items = v})
      ()
  in
  check
    string
    "three items"
    "3 selected"
    (FT.field_get_string f {(empty ()) with items = ["a"; "b"; "c"]})

(* ================================================================== *)
(* TEST SUITE                                                          *)
(* ================================================================== *)

let () =
  run
    "Form Builder Pure"
    [
      ( "parse_host_port",
        [
          test_case "valid standard" `Quick test_php_valid_standard;
          test_case "valid localhost" `Quick test_php_valid_localhost;
          test_case "valid port 1" `Quick test_php_valid_port_1;
          test_case "valid port 65535" `Quick test_php_valid_port_65535;
          test_case "invalid port 0" `Quick test_php_invalid_port_0;
          test_case "invalid port 65536" `Quick test_php_invalid_port_65536;
          test_case
            "invalid port negative"
            `Quick
            test_php_invalid_port_negative;
          test_case "invalid port alpha" `Quick test_php_invalid_port_alpha;
          test_case "invalid empty host" `Quick test_php_invalid_empty_host;
          test_case "invalid no colon" `Quick test_php_invalid_no_colon;
          test_case
            "invalid multiple colons"
            `Quick
            test_php_invalid_multiple_colons;
          test_case "invalid empty string" `Quick test_php_invalid_empty_string;
          test_case "port with spaces" `Quick test_php_port_with_spaces;
        ] );
      ( "parse_host_port QCheck",
        [
          test_php_qcheck_no_crash;
          test_php_qcheck_valid_format;
          test_php_qcheck_error_messages_nonempty;
        ] );
      ( "endpoint field",
        [
          test_case "empty valid" `Quick test_endpoint_empty_valid;
          test_case "whitespace valid" `Quick test_endpoint_whitespace_valid;
          test_case "good value" `Quick test_endpoint_good_value;
          test_case "bad value" `Quick test_endpoint_bad_value;
          test_case "bad msg" `Quick test_endpoint_bad_msg;
          test_case "good msg" `Quick test_endpoint_good_msg;
          test_case "empty msg" `Quick test_endpoint_empty_msg;
        ] );
      ( "validated_text",
        [
          test_case "empty fails" `Quick test_validated_empty_fails;
          test_case "empty msg" `Quick test_validated_empty_msg;
          test_case "filled passes" `Quick test_validated_filled_passes;
          test_case "filled msg" `Quick test_validated_filled_msg;
        ] );
      ( "always-valid fields",
        [
          test_case "text" `Quick test_text_always_valid;
          test_case "toggle" `Quick test_toggle_always_valid;
          test_case "readonly" `Quick test_readonly_always_valid;
        ] );
      ( "field_get_string",
        [
          test_case "text" `Quick test_text_get_string;
          test_case "toggle" `Quick test_toggle_get_string;
          test_case "readonly" `Quick test_readonly_get_string;
        ] );
      ("field_label", [test_case "labels match" `Quick test_field_label]);
      ( "with_hint",
        [
          test_case "no hint by default" `Quick test_no_hint_by_default;
          test_case "sets hint" `Quick test_with_hint_sets_hint;
          test_case "overwrite" `Quick test_with_hint_overwrite;
        ] );
      ( "service_or_endpoint",
        [
          test_case "None valid" `Quick test_soe_none_valid;
          test_case "Service valid" `Quick test_soe_service_valid;
          test_case "Endpoint valid" `Quick test_soe_endpoint_valid;
          test_case "Endpoint invalid" `Quick test_soe_endpoint_invalid;
          test_case "Endpoint invalid msg" `Quick test_soe_endpoint_invalid_msg;
          test_case "custom validator" `Quick test_soe_custom_validator;
        ] );
      ( "string_list display",
        [
          test_case "none" `Quick test_string_list_none;
          test_case "one" `Quick test_string_list_one;
          test_case "two" `Quick test_string_list_two;
          test_case "many" `Quick test_string_list_many;
        ] );
    ]
