(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure-state regression tests for the node installation form
    ([install_node_form_v3.ml]) that exercise model-transition helpers
    directly, without the headless TUI driver. *)

open Alcotest
module Install_node_form = Octez_manager_ui.Install_node_form_v3

(* ============================================================ *)
(* Regression test for issue #1000: switching to archive history *)
(* mode must clear a manually-selected snapshot                  *)
(* ============================================================ *)

(** https://github.com/trilitech/octez-manager/issues/1000

    Switching History Mode to "archive" hides the Snapshot field (see the
    field-list guard in [install_node_form_v3.ml]), but
    [set_node_with_autoname] -- the [~set_node] callback the History Mode
    field actually invokes -- only resets the snapshot selection when it
    is an *auto* snapshot ([is_auto_snapshot]). A manually-picked Tzinit
    entry survives untouched in the model. Because the snapshot field is
    no longer rendered, its [history_snapshot_conflict] validation never
    runs again, so [on_submit] would build an archive install that still
    carries a snapshot -- contradicting the wizard's own invariant that
    archive installs never import a snapshot
    ([create_default_snapshot] returns [`None] for archive). *)
let test_archive_clears_manual_snapshot_issue_1000 () =
  let manual_snapshot =
    `Tzinit
      Install_node_form.
        {
          network_slug = "shadownet";
          kind_slug = "rolling";
          label = "Tzinit \xc2\xb7 2026-07-20";
          (* Does not start with "Auto (" -> a manual, user-chosen entry. *)
        }
  in
  let before =
    Install_node_form.For_tests.(with_snapshot manual_snapshot (fresh_model ()))
  in
  (* Same code path the History Mode field uses when the user switches the
     mode to "archive": build the new node config and hand it to the
     field's own [~set_node] setter. *)
  let archive_node =
    {
      (Install_node_form.For_tests.node_of before) with
      Octez_manager_ui.Form_builder_common.history_mode = "archive";
    }
  in
  let after =
    Install_node_form.For_tests.set_node_with_autoname archive_node before
  in
  let describe = function
    | `None -> "`None"
    | `Url u -> Printf.sprintf "`Url %S" u
    | `Tzinit (t : Install_node_form.tzinit_snapshot) ->
        Printf.sprintf "`Tzinit {label = %S; ...}" t.label
  in
  check
    string
    "archive history mode must clear manual snapshot selection"
    "`None"
    (describe (Install_node_form.For_tests.snapshot_of after))

let () =
  Alcotest.run
    "Install Node Form (pure)"
    [
      ( "archive_snapshot_conflict",
        [
          Alcotest.test_case
            "archive clears manual snapshot (#1000)"
            `Quick
            test_archive_clears_manual_snapshot_issue_1000;
        ] );
    ]
