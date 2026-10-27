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
      [name issue-url drift-digest reason...]. A quarantined check passes only
      while the screen is exactly the recorded drift (MD5 of the rendered
      screen, printed by a failing check): a match with the baseline fails so
      that the entry gets removed, and any other screen fails as a new
      regression. Entries need a [trilitech/octez-manager] issue URL and an
      existing baseline file.

    Exit contract for golden suites: assertion failures exit 1 (Alcotest).
    Suites call {!check_setup} before [Alcotest.run] and exit 2 on [Error]. *)

(** Raised when the verification environment is unusable (no baseline
    directory, unreadable baseline, malformed quarantine, update mode in CI).
    It is distinct from an assertion failure. *)
exception Setup_error of string

(** [check_setup ()] validates the baseline directory, the quarantine file and
    the update-mode policy. *)
val check_setup : unit -> (unit, string) result

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

(**/**)

module Internal_for_tests : sig
  (** Outcome of comparing one screen with its baseline. *)
  type verdict =
    | Match
    | Mismatch of (int * string * string) list
        (** [(line, expected, actual)] differences *)
    | Missing of string  (** Path of the absent baseline *)
    | Known_drift of string  (** Quarantined, screen is the recorded drift *)
    | Unknown_drift of string * string * (int * string * string) list
        (** Quarantined but a different screen: issue URL, digest, diffs *)
    | Unexpected_pass of string  (** Quarantined but now matching; issue URL *)

  (** A quarantine line: tracking issue URL and digest of the accepted drift. *)
  type quarantine_entry = {issue : string; drift : string}

  (** MD5 hex of a rendered screen, after ephemeral-URL normalization. *)
  val screen_digest : string -> string

  (** [load_quarantine ~dir] parses [dir/QUARANTINE] into [(name, entry)]
      pairs. An absent file is an empty quarantine. *)
  val load_quarantine :
    dir:string -> ((string * quarantine_entry) list, string) result

  (** [verify ~dir ~quarantine ~name ~current] compares [current] with the
      baseline [name] in [dir]. Never writes.
      @raise Setup_error if the baseline exists but cannot be read. *)
  val verify :
    dir:string ->
    quarantine:(string * quarantine_entry) list ->
    name:string ->
    current:string ->
    verdict
end
