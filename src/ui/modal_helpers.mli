(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Open a read-only text modal showing [lines] with the given [title]. *)
val open_text_modal : title:string -> lines:string list -> unit

(** Open a single-select list modal.
    @param items Choices to display.
    @param to_string Render each item as a string.
    @param on_tick Optional callback invoked on each render tick (e.g. to refresh items).
    @param on_select Called when the user picks an item. *)
val open_choice_modal :
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  ?on_tick:(unit -> unit) ->
  on_select:('a -> unit) ->
  unit ->
  unit

(** Like {!open_choice_modal} but with a hint panel and description lines
    for the currently highlighted item. *)
val open_choice_modal_with_hint :
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  hint:('a -> unit) ->
  describe:('a -> string list) ->
  on_select:('a -> unit) ->
  unit ->
  unit

(** Open a multi-select list modal.
    [on_select] is called for each toggled item and returns
    [`KeepOpen] to continue selecting or [`Close] to dismiss.
    
    @param item_key Optional function to extract a stable key from items for
    cursor position tracking. When provided, the cursor will stay on the same
    logical item after the list is rebuilt (even if the item's display changes).
    Without this, cursor position is based on direct item equality. *)
val open_multiselect_modal :
  title:string ->
  items:(unit -> 'a list) ->
  to_string:('a -> string) ->
  ?item_key:('a -> 'key) ->
  on_select:('a -> [< `KeepOpen | `Close]) ->
  unit ->
  unit

(** Open a text input prompt modal.
    @param initial Pre-filled text.
    @param placeholder Ghost text shown when input is empty.
    @param on_submit Called with the entered text on Enter. *)
val prompt_text_modal :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  on_submit:(string -> unit) ->
  unit ->
  unit

(** Like {!prompt_text_modal} but with masked input (characters shown as ['*']).
    Suitable for password entry. No [initial] parameter since passwords should
    never be pre-filled.

    @param on_submit Called with the entered password on Enter. *)
val prompt_password_modal :
  ?title:string -> ?width:int -> on_submit:(string -> unit) -> unit -> unit

(** Like {!prompt_text_modal} but validates input before accepting.
    The [validator] returns [Ok ()] to allow submission or [Error msg]
    to show an inline error. *)
val prompt_validated_text_modal :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  validator:(string -> (unit, string) result) ->
  on_submit:(string -> unit) ->
  unit ->
  unit

(** Open a multi-line text input prompt modal.
    @param initial Pre-filled text.
    @param placeholder Ghost text shown when input is empty.
    @param height Height in rows for the textarea. *)
val prompt_textarea_modal :
  ?title:string ->
  ?width:int ->
  ?height:int ->
  ?initial:string ->
  ?placeholder:string ->
  on_submit:(string -> unit) ->
  unit ->
  unit

(** Show a success modal with a green-highlighted message. *)
val show_success : title:string -> string -> unit

(** Show an error modal with a red-highlighted message. *)
val show_error : title:string -> string -> unit

(** Show a yes/no confirmation dialog.
    [on_result] receives [true] for yes, [false] for no/cancel. *)
val confirm_modal :
  ?title:string -> message:string -> on_result:(bool -> unit) -> unit -> unit

(** Show the global help overlay listing all keybindings. *)
val show_help_modal : unit -> unit

(** Open a theme picker modal with live preview.
    As the user navigates through themes, [load_theme] is called to preview each one.
    On Enter, [on_select] is called with the chosen theme ID.
    On Esc, [on_cancel] is called to restore the previous theme. *)
val open_theme_picker_modal :
  title:string ->
  items:string list ->
  to_string:(string -> string) ->
  load_theme:(string -> unit) ->
  on_select:(string -> unit) ->
  on_cancel:(unit -> unit) ->
  unit ->
  unit

(** Open a file-system browser modal for selecting files or directories.
    @param dirs_only When [true], only directories are selectable.
    @param require_writable Only show directories the user can write to. *)
val open_file_browser_modal :
  ?initial_path:string ->
  dirs_only:bool ->
  require_writable:bool ->
  on_select:(string -> unit) ->
  unit ->
  unit

(** Open a directory picker restricted to registered directories of [dir_type]. *)
val select_directory_modal :
  title:string ->
  dir_type:Octez_manager_lib.Directory_registry.dir_type ->
  on_select:(string -> unit) ->
  unit ->
  unit

(** Shortcut to open a directory picker for node data directories. *)
val select_node_data_dir_modal : on_select:(string -> unit) -> unit -> unit

(** Shortcut to open a directory picker for client base directories. *)
val select_client_base_dir_modal : on_select:(string -> unit) -> unit -> unit

(** Open a directory picker for application binary directories,
    returning both the path and the corresponding {!Binary_registry.bin_source}. *)
val select_app_bin_dir_modal :
  on_select:(string * Octez_manager_lib.Binary_registry.bin_source -> unit) ->
  unit ->
  unit

(** Signatory-specific app bin dir selection modal. Shows managed Signatory
    versions, registered directories, and download options. *)
val select_signatory_app_bin_dir_modal :
  on_select:(string * Octez_manager_lib.Binary_registry.bin_source -> unit) ->
  unit ->
  unit

(** Show a spinner modal while a background task runs.
    @param title Modal title
    @param label Text shown next to spinner
    @param work Background work function
    @param on_complete Called when work completes *)
val show_spinner_modal :
  title:string ->
  label:string ->
  work:(unit -> (unit, [`Msg of string]) result) ->
  on_complete:([`Succeeded | `Failed of string | `Cancelled] -> unit) ->
  unit ->
  unit

(** Wrap a string to the given width, breaking at word boundaries.
    Handles embedded newlines.

    @param width The maximum line width in characters. Must be [>= 1]. *)
val wrap_text : width:int -> string -> string list

module For_tests : sig
  (** Return the first non-empty line from a list, or [None]. *)
  val first_nonempty_line : string list -> string option

  (** @see wrap_text *)
  val wrap_text : width:int -> string -> string list

  (** Extract the major version number from a version string (e.g. ["v21.1" -> 21]). *)
  val extract_major : string -> int
end
