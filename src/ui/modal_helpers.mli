(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val open_text_modal : title:string -> lines:string list -> unit

val open_choice_modal :
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  ?on_tick:(unit -> unit) ->
  on_select:('a -> unit) ->
  unit ->
  unit

val open_choice_modal_with_hint :
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  hint:('a -> unit) ->
  describe:('a -> string list) ->
  on_select:('a -> unit) ->
  unit ->
  unit

val open_multiselect_modal :
  title:string ->
  items:(unit -> 'a list) ->
  to_string:('a -> string) ->
  on_select:('a -> [< `KeepOpen | `Close]) ->
  unit

val prompt_text_modal :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  on_submit:(string -> unit) ->
  unit ->
  unit

val prompt_validated_text_modal :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  validator:(string -> (unit, string) result) ->
  on_submit:(string -> unit) ->
  unit ->
  unit

val show_success : title:string -> string -> unit

val show_error : title:string -> string -> unit

val confirm_modal :
  ?title:string -> message:string -> on_result:(bool -> unit) -> unit -> unit

val show_help_modal : unit -> unit

val show_menu_modal : unit -> unit

val open_file_browser_modal :
  ?initial_path:string ->
  dirs_only:bool ->
  require_writable:bool ->
  on_select:(string -> unit) ->
  unit ->
  unit

val select_directory_modal :
  title:string ->
  dir_type:Octez_manager_lib.Directory_registry.dir_type ->
  on_select:(string -> unit) ->
  unit ->
  unit

val select_node_data_dir_modal : on_select:(string -> unit) -> unit -> unit

val select_client_base_dir_modal : on_select:(string -> unit) -> unit -> unit

val select_app_bin_dir_modal : on_select:(string -> unit) -> unit -> unit
