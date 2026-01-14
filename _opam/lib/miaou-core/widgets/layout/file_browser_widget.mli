(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** File browser widget for navigating and selecting files/directories.

    This widget provides an interactive file system browser with:
    - Directory navigation (Up/Down arrows, Enter to descend)
    - Path editing mode (press 'e' or start typing '/')
    - Tab completion
    - History navigation (Up/Down in edit mode)
    - Directory creation modal
    - Filters (dirs_only, writable check)

    {b Typical usage}:
    {[
      (* Create a file browser *)
      let browser = File_browser_widget.open_centered
        ~path:"/home/user"
        ~dirs_only:true
        ~require_writable:true
        ()

      (* Render in your view *)
      let view state ~size ~focus =
        File_browser_widget.render_with_size browser ~size ~focus

      (* Handle keyboard input *)
      let handle_key state ~key ~size =
        let browser' = File_browser_widget.handle_key browser ~key in
        ...

      (* Get selected path *)
      match File_browser_widget.get_selection browser with
      | Some path -> Printf.printf "Selected: %s\n" path
      | None -> ()

      (* Check if user cancelled *)
      if File_browser_widget.is_cancelled browser then
        (* User pressed Esc *)
    ]}
*)

(** File system entry *)
type entry = {
  name : string;  (** Filename or directory name *)
  is_dir : bool;  (** Whether this is a directory *)
}

(** Browser mode *)
type mode =
  | Browsing  (** Normal browsing mode *)
  | EditingPath  (** Direct path editing mode *)

(** File browser state *)
type t = {
  current_path : string;  (** Current directory path *)
  cursor : int;  (** Selected entry index *)
  cancelled : bool;  (** Whether user pressed Esc *)
  dirs_only : bool;  (** Show only directories *)
  require_writable : bool;  (** Filter to writable directories only *)
  select_dirs : bool;  (** Allow selecting directories (including "."). *)
  show_hidden : bool;  (** Show hidden files/directories (starting with '.') *)
  mode : mode;  (** Current interaction mode *)
  path_buffer : string;  (** Buffer for path editing *)
  path_error : string option;  (** Error message from invalid path *)
  pending_selection : string option;  (** Path pending selection *)
  create_dir_on_enter : bool;
      (** When [true], [Enter] in edit mode will create a new directory with the
          textbox contents instead of navigating/selecting. *)
  textbox : Miaou_widgets_input.Textbox_widget.t option;
      (** Textbox for path editing *)
  history : string list;  (** Path history (most recent first) *)
  history_idx : int option;  (** Current position in history *)
}

(** {1 Creation} *)

(** Create a file browser widget.

    @param path Initial directory path (default: "/")
    @param dirs_only Show only directories, hide files (default: true)
    @param require_writable Filter to writable directories only (default: true)
    @param select_dirs Allow selecting directories (default: true)
    @param show_hidden Show hidden files/directories starting with '.' (default: false)
*)
val open_centered :
  ?path:string ->
  ?dirs_only:bool ->
  ?require_writable:bool ->
  ?select_dirs:bool ->
  ?show_hidden:bool ->
  unit ->
  t

(** {1 State Queries} *)

(** Check if user cancelled (pressed Esc). *)
val is_cancelled : t -> bool

(** Clear the cancelled flag. *)
val reset_cancelled : t -> t

(** Get the current directory path. *)
val get_current_path : t -> string

(** Get the selected path.

    Returns [Some path] if a valid directory is selected and passes filters
    (writable check, etc.). Returns [None] if no valid selection.
*)
val get_selection : t -> string option

(** Get the entry currently under the cursor (including [".."] and ["."]). *)
val get_selected_entry : t -> entry option

(** Check if browser is in path editing mode. *)
val is_editing : t -> bool

(** Check if current selection can be committed.

    Returns true if the selected path exists, is a directory, and passes
    all filters (writable, etc.).
*)
val can_commit : t -> bool

(** Get the current input text (from path editing textbox). *)
val current_input : t -> string

(** {1 Internal Textbox Utilities} *)

(** Create a textbox for path editing (internal utility).

    @param width Textbox width (default: 60)
    @param initial Initial text (default: "")
*)
val textbox_create :
  ?width:int -> ?initial:string -> unit -> Miaou_widgets_input.Textbox_widget.t

(** Get text from textbox (wrapper around Textbox_widget.get_text). *)
val textbox_get_text : Miaou_widgets_input.Textbox_widget.t -> string

(** Set text in textbox (wrapper around Textbox_widget.set_text). *)
val textbox_set_text :
  Miaou_widgets_input.Textbox_widget.t ->
  string ->
  Miaou_widgets_input.Textbox_widget.t

(** Handle key in textbox (wrapper around Textbox_widget.handle_key). *)
val textbox_handle_key :
  Miaou_widgets_input.Textbox_widget.t ->
  key:string ->
  Miaou_widgets_input.Textbox_widget.t

(** Key hints for footer/help display.
    Includes navigation, selection, edit path, hidden toggle, parent, cancel. *)
val key_hints : t -> (string * string) list

(** Render textbox (wrapper around Textbox_widget.render). *)
val textbox_render : Miaou_widgets_input.Textbox_widget.t -> string

(** {1 Rendering} *)

(** Render the file browser with size awareness.

    Displays:
    - Current path
    - List of entries (directories/files)
    - Cursor highlighting
    - Path editing textbox (when in EditingPath mode)
    - Error messages (when path is invalid)

    @param size Terminal/window size
    @param focus Whether the widget has focus
    @return Rendered string
*)
val render_with_size : t -> focus:bool -> size:LTerm_geom.size -> string

(** Render with default size assumptions.

    @param focus Whether the widget has focus
    @return Rendered string
*)
val render : t -> focus:bool -> string

(** {1 Input Handling} *)

(** Handle keyboard input.

    Browsing mode keys:
    - [Up]/[Down]: Move cursor
    - [Enter]: Enter selected directory; on files/[.] select instead
    - [Backspace]: Go to parent directory
    - [Tab]/[C-l]: Enter path editing mode
    - [/] or [~]: Start path editing with that character
    - [n]: Start inline mkdir by opening path editing prefilled with a new name
    - [h]: Toggle visibility of hidden files/directories
    - [Esc]: Cancel selection

    Path editing mode keys:
    - [Enter]: Navigate to entered path, or create directory when started via [n]
    - [Esc]: Exit editing mode
    - [Tab]/[Shift-Tab]: Path completion (forward/backward)
    - [Up]/[Down]: Navigate history
    - Other keys: Edit path text

    @param key Key string (e.g., "Up", "Enter", "e")
    @return Updated file browser state
*)
val handle_key : t -> key:string -> t

(** {1 Directory Operations} *)

(** Create a directory and navigate to it.

    Creates the directory and navigates to it if successful.

    @param dirname Name of directory to create
    @return Result containing (updated browser, success flag) or error message
*)
val mkdir_and_cd : t -> string -> (t * bool, string) result

(** {1 Async Updates} *)

(** Apply any pending path updates scheduled from async callbacks.

    Used internally for handling modal callbacks.

    @return Updated file browser with pending updates applied
*)
val apply_pending_updates : t -> t

(** Schedule a path update for later application.

    Used internally for modal callbacks to update the browser path.

    @param path Path to schedule
*)
val schedule_path_update : string -> unit

(** {1 Cache Management} *)

(** Invalidate the internal filesystem cache.

    Call this after external filesystem changes (e.g., directory creation)
    to ensure the browser shows fresh data. The cache is automatically
    invalidated when navigating to a different directory.
*)
val invalidate_cache : unit -> unit
