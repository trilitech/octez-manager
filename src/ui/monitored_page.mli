(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Wrapper functor that adds automatic metrics tracking to any page.
    
    Usage:
    {[
      module Page = struct
        (* ... normal page implementation ... *)
      end

      module Monitored = Monitored_page.Make(Page)(struct
        let page_name = "instances"
      end)

      let page () = (module Monitored : Miaou.Core.Tui_page.PAGE_SIG)
    ]}
*)

module Make : functor
  (P : Miaou.Core.Tui_page.PAGE_SIG)
  (_ : sig
     val page_name : string
   end)
  ->
  Miaou.Core.Tui_page.PAGE_SIG with type state = P.state and type msg = P.msg
