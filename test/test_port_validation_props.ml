(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Property-based tests for Port_validation module.

    Properties tested:
    - No-crash: parse_host_port/parse_port never raise on random input
    - Port range: parsed port is always in [1, 65535]
    - Valid format roundtrip: "host:port" → (host, port) preserves values
*)

open Octez_manager_lib

(* ============================================================ *)
(* No-crash properties *)
(* ============================================================ *)

let prop_parse_host_port_no_crash =
  QCheck.Test.make
    ~name:"parse_host_port never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Port_validation.parse_host_port s in
      true)

let prop_parse_port_no_crash =
  QCheck.Test.make
    ~name:"parse_port never crashes on random input"
    ~count:300
    QCheck.string
    (fun s ->
      let _result = Port_validation.parse_port s in
      true)

(* ============================================================ *)
(* Invariant properties *)
(* ============================================================ *)

let prop_parsed_port_in_range =
  QCheck.Test.make
    ~name:"parsed port is always in [1, 65535]"
    ~count:300
    QCheck.string
    (fun s ->
      match Port_validation.parse_host_port s with
      | Some (_host, port) -> port >= 1 && port <= 65535
      | None -> true)

let prop_parse_port_in_range =
  QCheck.Test.make
    ~name:"parse_port result is always in [1, 65535]"
    ~count:300
    QCheck.string
    (fun s ->
      match Port_validation.parse_port s with
      | Some port -> port >= 1 && port <= 65535
      | None -> true)

let prop_valid_host_port_parsed =
  QCheck.Test.make
    ~name:"valid host:port strings are always parsed"
    ~count:300
    QCheck.(pair (int_range 0 255) (int_range 1 65535))
    (fun (octet, port) ->
      let addr = Printf.sprintf "127.0.0.%d:%d" octet port in
      match Port_validation.parse_host_port addr with
      | Some (host, p) ->
          String.equal host (Printf.sprintf "127.0.0.%d" octet) && p = port
      | None -> false)

let prop_pp_error_no_crash =
  QCheck.Test.make
    ~name:"pp_error never crashes"
    ~count:100
    QCheck.(
      oneof
        [
          always (Port_validation.Invalid_format "test");
          always Port_validation.Port_out_of_range;
          always (Port_validation.Used_by_other_instance (8732, "node1"));
          always (Port_validation.Port_in_use (8732, Some "process"));
          always (Port_validation.Port_in_use (8732, None));
        ])
    (fun err ->
      let _s = Port_validation.pp_error err in
      true)

(* ============================================================ *)
(* Test Suite *)
(* ============================================================ *)

let props =
  List.map
    QCheck_alcotest.to_alcotest
    [
      prop_parse_host_port_no_crash;
      prop_parse_port_no_crash;
      prop_parsed_port_in_range;
      prop_parse_port_in_range;
      prop_valid_host_port_parsed;
      prop_pp_error_no_crash;
    ]

let () = Alcotest.run "Port_validation_props" [("properties", props)]
