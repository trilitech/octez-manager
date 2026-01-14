(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Tui_page = Tui_page
module Registry = Registry
module Widgets = Miaou_widgets_display.Widgets

(* Re-export widget modules from the miaou.widgets library to avoid duplicating implementations.
	Consumers should access them as Miaou.Core.Widgets or via the top-level Miaou library. *)
module Table_widget = Miaou_widgets_display.Table_widget
module Textbox_widget = Miaou_widgets_input.Textbox_widget
module Select_widget = Miaou_widgets_input.Select_widget
module File_browser_widget = Miaou_widgets_layout.File_browser_widget
module File_browser_modal = File_browser_modal
module Pane_layout = Miaou_widgets_layout.Pane_layout
module Pager_widget = Miaou_widgets_display.Pager_widget
module Tree_widget = Miaou_widgets_display.Tree_widget
module Tui_logger = Tui_logger

(* The following modules are application-specific and are intentionally
	not re-exported by the core library. Applications that need these
	helpers should reference their own `App_specific` (or similarly
	named) modules directly. *)
module Keys = Keys
module Palette = Miaou_widgets_display.Palette
module Modal_manager = Modal_manager
module Quit_flag = Quit_flag
module Help_hint = Help_hint
