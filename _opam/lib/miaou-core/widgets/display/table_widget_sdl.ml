(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* SDL-specific table render helpers, kept separate for backend tweaks. *)

let render_table_80_with_opts ?wrap ~cols ~header ~rows ~cursor ~sel_col ~opts
    () =
  Table_widget.render_table_80_with_opts
    ~backend:`Sdl
    ?wrap
    ~cols
    ~header
    ~rows
    ~cursor
    ~sel_col
    ~opts
    ()

let render_table_80 ~cols ~header ~rows ~cursor ~sel_col =
  Table_widget.render_table_80 ~cols ~header ~rows ~cursor ~sel_col
