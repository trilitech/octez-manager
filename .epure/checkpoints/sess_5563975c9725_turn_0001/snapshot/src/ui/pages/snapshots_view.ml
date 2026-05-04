(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the snapshots page. No Eio calls. *)

module Widgets = Miaou_widgets_display.Widgets
module Lib_snapshots = Octez_manager_lib.Snapshots

type state = {
  network : string;
  entries : Lib_snapshots.entry list;
  selected : int;
  error : string option;
}

let header s =
  [
    Widgets.themed_primary (" Snapshots · " ^ s.network);
    Widgets.themed_muted "n: select network";
  ]

let view s ~focus:_ ~size =
  let body =
    if s.entries = [] then ["No snapshots found or error loading."]
    else
      s.entries
      |> List.mapi (fun i (entry : Lib_snapshots.entry) ->
          let marker =
            if i = s.selected then Widgets.themed_emphasis ">" else " "
          in
          Printf.sprintf
            "%s %-20s %s"
            marker
            (Widgets.themed_emphasis entry.label)
            (Widgets.themed_muted (Option.value ~default:"" entry.download_url)))
  in
  Themed_page.render_layout ~size ~header:(header s) ~footer:[] ~child:(fun _ ->
      String.concat "\n" body)
