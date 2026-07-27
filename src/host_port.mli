(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Bracket-aware [host:port] address splitting.

    Every place in the codebase that parses a [host:port] address (RPC
    addresses, P2P addresses, Signatory HTTP addresses, ...) used to split
    naively on [':'], which rejects IPv6 literals such as [::1] because
    they contain colons themselves. This module centralizes the splitting
    logic so all call sites agree on the same policy:

    - A single-colon [host:port] is split as before (IPv4, hostnames, and
      the empty/wildcard host used for "bind all interfaces", e.g. the
      port-only form with an empty host part before the colon).
    - A bracketed IPv6 literal followed by a port (optionally with a zone
      id, e.g. bracketed [fe80::1%eth0] followed by [:9732]) is accepted.
      The returned host string includes its brackets, since that is
      exactly the form octez accepts back on the command line — callers
      must not strip them when round-tripping the value.
    - A bare IPv6 literal combined with a port (e.g. [fe80::1:9732], with
      no brackets) is ambiguous: there is no way to tell where the
      address ends and the port begins. Such inputs are rejected with
      {!Bare_ipv6} rather than silently mis-parsed; callers must bracket
      the address instead. *)

(** Reasons {!split} can fail to recognize a [host:port] string. *)
type error =
  | Invalid_format
      (** Does not look like [host:port] (no colon, or malformed
          brackets). *)
  | Bare_ipv6
      (** Multiple colons outside of brackets — most likely an
          unbracketed IPv6 literal combined with a port. *)

(** Actionable message recommended for {!Bare_ipv6}, shared so every
    caller reports the same text: IPv6 addresses must be bracketed, e.g.
    bracketed [::1] followed by [:8732]. *)
val bare_ipv6_message : string

(** Split a [host:port] string into its raw host and port substrings.
    The host substring includes brackets for a bracketed IPv6 literal.
    The port substring is returned verbatim (not parsed as an int) so
    callers can apply their own numeric range validation. *)
val split : string -> (string * string, error) result
