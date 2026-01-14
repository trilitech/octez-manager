(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tui_page

module Page : PAGE_SIG = struct
  type state = string

  type msg = unit

  type pstate = state Navigation.t

  type key_binding = state Tui_page.key_binding_desc

  let handle_modal_key ps _ ~size:_ = ps

  let handle_key ps _ ~size:_ = ps

  let update ps _ = ps

  let move ps _ = ps

  let refresh ps = ps

  let service_select ps _ = ps

  let service_cycle ps _ = ps

  let back ps = ps

  let has_modal _ = false

  let init () =
    Navigation.make
      "Your terminal is narrow (< 80 cols). For best experience, widen it. \
       Press any key to dismiss."

  let view ps ~focus:_ ~size:_ = ps.Navigation.s

  let keymap (_ : pstate) = []

  let handled_keys () = []
end
