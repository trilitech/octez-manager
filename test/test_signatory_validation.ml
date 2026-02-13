(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

let test_valid_public_key_hashes () =
  let valid_keys =
    [
      "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb";
      "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m";
      "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN";
      "tz4HQ91B7jVojQDZKvApqMfsfPnkqzfG3zPp";
    ]
  in
  List.iter
    (fun key ->
      match Signatory_validation.validate_public_key_hash key with
      | Ok () -> ()
      | Error (`Msg msg) ->
          Alcotest.failf "Expected %s to be valid but got error: %s" key msg)
    valid_keys

let test_invalid_public_key_hashes () =
  let invalid_keys =
    [
      ("", "Empty string");
      ("tz1", "Too short");
      ("tz5VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb", "Invalid prefix");
      ("tx1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb", "Wrong prefix");
      ("tz1short", "Too short hash");
    ]
  in
  List.iter
    (fun (key, description) ->
      match Signatory_validation.validate_public_key_hash key with
      | Ok () -> Alcotest.failf "%s: Expected %s to be invalid" description key
      | Error (`Msg _) -> ())
    invalid_keys

let test_valid_authorized_keys () =
  let keys =
    [
      "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb";
      "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m";
    ]
  in
  match Signatory_validation.validate_authorized_keys keys with
  | Ok () -> ()
  | Error (`Msg msg) ->
      Alcotest.failf "Expected valid authorized keys but got error: %s" msg

let test_empty_authorized_keys () =
  match Signatory_validation.validate_authorized_keys [] with
  | Ok () -> Alcotest.fail "Expected error for empty authorized keys"
  | Error (`Msg msg) ->
      Alcotest.(check string)
        "Error message"
        "At least one authorized key is required"
        msg

let test_authorized_keys_with_invalid () =
  let keys = ["tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"; "invalid_key"] in
  match Signatory_validation.validate_authorized_keys keys with
  | Ok () -> Alcotest.fail "Expected error for invalid key in list"
  | Error (`Msg _) -> ()

let test_valid_http_addresses () =
  let addresses =
    [
      ("127.0.0.1:6732", "Localhost IPv4");
      ("0.0.0.0:8080", "Any address");
      ("192.168.1.100:9000", "Private network");
      ("signatory.example.com:6732", "Domain name");
    ]
  in
  List.iter
    (fun (addr, description) ->
      match
        Signatory_validation.validate_http_address ~addr ~name:"test address"
      with
      | Ok () -> ()
      | Error (`Msg msg) ->
          Alcotest.failf
            "%s: Expected %s to be valid but got error: %s"
            description
            addr
            msg)
    addresses

let test_invalid_http_addresses () =
  let addresses =
    [
      ("localhost", "Missing port");
      (":6732", "Missing host");
      ("127.0.0.1:0", "Port too low");
      ("127.0.0.1:65536", "Port too high");
      ("127.0.0.1:abc", "Invalid port");
      ("127.0.0.1:6732:extra", "Too many colons");
    ]
  in
  List.iter
    (fun (addr, description) ->
      match
        Signatory_validation.validate_http_address ~addr ~name:"test address"
      with
      | Ok () -> Alcotest.failf "%s: Expected %s to be invalid" description addr
      | Error (`Msg _) -> ())
    addresses

let test_valid_backends () =
  let backends =
    [
      Installer_types.File "/path/to/keys";
      Installer_types.YubiHSM {connector_url = "http://localhost:12345"};
      Installer_types.Azure_KMS {vault_name = "my-vault"; tenant_id = "tenant"};
      Installer_types.AWS_KMS {region = "us-east-1"};
      Installer_types.GCP_KMS {project_id = "my-project"; location = "global"};
      Installer_types.Vault {address = "http://vault:8200"; role = "signer"};
    ]
  in
  List.iter
    (fun backend ->
      match Signatory_validation.validate_backend backend with
      | Ok () -> ()
      | Error (`Msg msg) ->
          Alcotest.failf "Expected backend to be valid but got error: %s" msg)
    backends

let test_invalid_backends () =
  let backends =
    [
      (Installer_types.File "", "Empty file path");
      (Installer_types.YubiHSM {connector_url = ""}, "Empty YubiHSM URL");
      ( Installer_types.Azure_KMS {vault_name = ""; tenant_id = "tenant"},
        "Empty vault name" );
      ( Installer_types.Azure_KMS {vault_name = "vault"; tenant_id = ""},
        "Empty tenant ID" );
      (Installer_types.AWS_KMS {region = ""}, "Empty AWS region");
      ( Installer_types.GCP_KMS {project_id = ""; location = "global"},
        "Empty project ID" );
      ( Installer_types.GCP_KMS {project_id = "project"; location = ""},
        "Empty location" );
      ( Installer_types.Vault {address = ""; role = "signer"},
        "Empty Vault address" );
      (Installer_types.Vault {address = "http://vault"; role = ""}, "Empty role");
    ]
  in
  List.iter
    (fun (backend, description) ->
      match Signatory_validation.validate_backend backend with
      | Ok () -> Alcotest.failf "%s: Expected backend to be invalid" description
      | Error (`Msg _) -> ())
    backends

let test_valid_watermarks () =
  let watermarks =
    [
      Installer_types.Memory;
      Installer_types.File_watermark "/path/to/watermark.json";
      Installer_types.AWS_DynamoDB
        {table_name = "watermarks"; region = "us-east-1"};
      Installer_types.GCP_Firestore
        {project_id = "my-project"; collection = "watermarks"};
    ]
  in
  List.iter
    (fun watermark ->
      match Signatory_validation.validate_watermark watermark with
      | Ok () -> ()
      | Error (`Msg msg) ->
          Alcotest.failf "Expected watermark to be valid but got error: %s" msg)
    watermarks

let test_invalid_watermarks () =
  let watermarks =
    [
      (Installer_types.File_watermark "", "Empty file path");
      ( Installer_types.AWS_DynamoDB {table_name = ""; region = "us-east-1"},
        "Empty table name" );
      ( Installer_types.AWS_DynamoDB {table_name = "table"; region = ""},
        "Empty region" );
      ( Installer_types.GCP_Firestore {project_id = ""; collection = "coll"},
        "Empty project ID" );
      ( Installer_types.GCP_Firestore {project_id = "proj"; collection = ""},
        "Empty collection" );
    ]
  in
  List.iter
    (fun (watermark, description) ->
      match Signatory_validation.validate_watermark watermark with
      | Ok () ->
          Alcotest.failf "%s: Expected watermark to be invalid" description
      | Error (`Msg _) -> ())
    watermarks

let test_valid_request () =
  let req : Installer_types.signatory_request =
    {
      instance = "test-signatory";
      backend = Installer_types.File "/keys";
      authorized_keys = ["tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"];
      address = "127.0.0.1:6732";
      metrics_address = "";
      (* Metrics are optional *)
      watermark = Installer_types.Memory;
      service_user = "tezos";
      app_bin_dir = "/usr/local/bin";
      bin_source = None;
      logging_mode = Logging_mode.Journald;
      auto_enable = true;
      preserve_data = false;
    }
  in
  match Signatory_validation.validate_request req with
  | Ok () -> ()
  | Error (`Msg msg) ->
      Alcotest.failf "Expected request to be valid but got error: %s" msg

let () =
  let open Alcotest in
  run
    "Signatory Validation"
    [
      ( "Public Key Hashes",
        [
          test_case "valid key hashes" `Quick test_valid_public_key_hashes;
          test_case "invalid key hashes" `Quick test_invalid_public_key_hashes;
        ] );
      ( "Authorized Keys",
        [
          test_case "valid authorized keys" `Quick test_valid_authorized_keys;
          test_case "empty authorized keys" `Quick test_empty_authorized_keys;
          test_case
            "authorized keys with invalid"
            `Quick
            test_authorized_keys_with_invalid;
        ] );
      ( "HTTP Addresses",
        [
          test_case "valid HTTP addresses" `Quick test_valid_http_addresses;
          test_case "invalid HTTP addresses" `Quick test_invalid_http_addresses;
        ] );
      ( "Backends",
        [
          test_case "valid backends" `Quick test_valid_backends;
          test_case "invalid backends" `Quick test_invalid_backends;
        ] );
      ( "Watermarks",
        [
          test_case "valid watermarks" `Quick test_valid_watermarks;
          test_case "invalid watermarks" `Quick test_invalid_watermarks;
        ] );
      ("Requests", [test_case "valid request" `Quick test_valid_request]);
    ]
