(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Wrapper functor that adds automatic metrics tracking to any page *)

module Make (P : Miaou.Core.Tui_page.PAGE_SIG) (Config : sig
  val page_name : string
end) : Miaou.Core.Tui_page.PAGE_SIG with type state = P.state and type msg = P.msg = struct
  type state = P.state

  type msg = P.msg

  let init = P.init

  let update = P.update

  (* Wrap view with metrics tracking *)
  let view s ~focus ~size =
    Metrics.record_render ~page:Config.page_name (fun () ->
      P.view s ~focus ~size)

  let move = P.move

  let refresh = P.refresh

  let enter = P.enter

  let service_select = P.service_select

  let service_cycle = P.service_cycle

  let back = P.back

  let handle_modal_key = P.handle_modal_key

  let handle_key = P.handle_key

  let next_page = P.next_page

  let keymap = P.keymap

  let has_modal = P.has_modal
end
