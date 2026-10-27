(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** UI regression testing: compare rendered screens with committed baselines.

    Verification contract:
    - Verification is read-only. A missing baseline fails the check; it is
      never created implicitly.
    - Baselines live in [ui_regressions/] under the current directory (the
      [dune runtest] sandbox, where they are declared dependencies), else in
      [test/ui_regressions/] (repository root), else in
      [$OM_UI_REGRESSION_DIR]-overridden location. No directory is a setup
      error.
    - Update mode ([UPDATE_REGRESSIONS=1|true] or [--update-regressions])
      rewrites baselines. Run it with [dune exec] from the repository root so
      that the source tree is written. It is refused under [CI=true] or
      [GITHUB_ACTIONS=true].
    - [ui_regressions/QUARANTINE] lists known-stale baselines, one per line:
      [name issue-url reason...]. A quarantined check passes only while the
      screen still differs from its baseline; a match fails so that the entry
      gets removed. Entries need a [trilitech/octez-manager] issue URL and an
      existing baseline file.

    Exit contract for golden suites: assertion failures exit 1 (Alcotest).
    Suites call {!check_setup} before [Alcotest.run] and exit 2 on [Error]. *)

(** Raised when the verification environment is unusable (no baseline
    directory, unreadable baseline, malformed quarantine, update mode in CI).
    It is distinct from an assertion failure. *)
exception Setup_error of string

(** Outcome of comparing one screen with its baseline. *)
type verdict =
  | Match
  | Mismatch of (int * string * string) list
      (** [(line, expected, actual)] differences *)
  | Missing of string  (** Path of the absent baseline *)
  | Known_drift of string  (** Quarantined and still different; issue URL *)
  | Unexpected_pass of string  (** Quarantined but now matching; issue URL *)

(** [check_setup ()] validates the baseline directory, the quarantine file and
    the update-mode policy. *)
val check_setup : unit -> (unit, string) result

(** [load_quarantine ~dir] parses [dir/QUARANTINE] into [(name, issue)] pairs.
    An absent file is an empty quarantine. *)
val load_quarantine : dir:string -> ((string * string) list, string) result

(** [verify ~dir ~quarantine ~name ~current] compares [current] with the
    baseline [name] in [dir]. Never writes.
    @raise Setup_error if the baseline exists but cannot be read. *)
val verify :
  dir:string ->
  quarantine:(string * string) list ->
  name:string ->
  current:string ->
  verdict

(** Capture the current headless screen without ANSI escapes. *)
val capture_screen : unit -> string

(** [check_regression name] verifies the current screen against baseline
    [name] (or rewrites it in update mode) and reports the result.
    @return [true] on match or known quarantined drift.
    @raise Setup_error on setup errors. *)
val check_regression : string -> bool

(** Like {!check_regression} but fails the test on [false]. *)
val assert_ui_regression : string -> unit

(** Save the current screen to [$OM_UI_DEBUG_DIR] (default
    [<tmp>/om-ui-debug]) for diagnosis. Never writes into the baseline
    directory. *)
val capture_debug_screenshot : string -> unit

(** Run each step's action then assert a regression named
    [<name>_step<i>_<step>]. *)
val test_scenario : string -> (string * (unit -> unit)) list -> unit

(** Run [setup_fn] then assert regression [name]. *)
val test_ui_state : string -> (unit -> unit) -> unit
