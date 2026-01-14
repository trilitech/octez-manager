(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Table widget for displaying tabular data with headers and selection.

    This widget provides formatted tables with:
    - Box-drawing borders (Unicode/ASCII)
    - Row/column/cell selection highlighting
    - Sortable columns
    - Wrapping support for long content
    - SDL and terminal rendering modes

    {b Typical usage}:
    {[
      (* Simple 3-column table *)
      let output = Table_widget.render_table_80
        ~cols:80
        ~header:("Name", "Age", "City")
        ~rows:[
          ("Alice", "30", "Paris");
          ("Bob", "25", "London");
        ]
        ~cursor:0
        ~sel_col:0
      in
      print_endline output
    ]}
*)

(** Selection mode for table highlighting *)
type selection_mode =
  | Row  (** Highlight entire row *)
  | Col  (** Highlight entire column *)
  | Cell  (** Highlight single cell *)
  | None_mode  (** No highlighting *)

(** Column rendering options *)
type column_opts = {
  max_width : int option;  (** Maximum width for column (truncated with …) *)
  pad_left : int;  (** Left padding spaces *)
  pad_right : int;  (** Right padding spaces *)
}

(** Table rendering options *)
type render_opts = {
  selection_mode : selection_mode;  (** How to highlight selection *)
  highlight_header : bool;  (** Bold/style the header row *)
  sort : (int * bool) option;  (** Sort indicator: (column_index, ascending) *)
}

(** Default rendering options: Row selection, no header highlight, no sort indicator *)
val default_opts : render_opts

(** {1 Rendering} *)

(** Render a 3-column table for 80-column terminals.

    Simple interface for the common case of 3 columns.

    @param cols Terminal width in columns
    @param header Tuple of (header1, header2, header3)
    @param rows List of (col1, col2, col3) tuples
    @param cursor Index of selected row (0-indexed)
    @param sel_col Index of selected column (0-indexed)
    @return Rendered table string
*)
val render_table_80 :
  cols:int option ->
  header:string * string * string ->
  rows:(string * string * string) list ->
  cursor:int ->
  sel_col:int ->
  string

(** Render 3-column table with full rendering options.

    Like {!render_table_80} but with customizable styling.

    @param cols Terminal width in columns
    @param header Tuple of (header1, header2, header3)
    @param rows List of (col1, col2, col3) tuples
    @param cursor Index of selected row (0-indexed)
    @param sel_col Index of selected column (0-indexed)
    @param opts Rendering options
    @return Rendered table string
*)
val render_table_80_with_opts :
  ?backend:Widgets.backend ->
  ?wrap:bool ->
  cols:int option ->
  header:string * string * string ->
  rows:(string * string * string) list ->
  cursor:int ->
  sel_col:int ->
  opts:render_opts ->
  unit ->
  string

(** Render table for SDL backend with modern styling.

    Uses solid backgrounds and badge-style rendering optimized for SDL.

    @param cols Terminal width in columns (optional)
    @param header Tuple of (header1, header2, header3)
    @param rows List of (col1, col2, col3) tuples
    @param cursor Index of selected row (0-indexed)
    @param sel_col Index of selected column (0-indexed)
    @param opts Rendering options
    @return Rendered table string
*)
val render_table_sdl :
  cols:int option ->
  header:string * string * string ->
  rows:(string * string * string) list ->
  cursor:int ->
  sel_col:int ->
  opts:render_opts ->
  string

(** Render table with full customization (variable columns).

    Most flexible table rendering function supporting any number of columns.

    @param backend Rendering backend (Terminal or SDL, default: current)
    @param wrap Enable text wrapping in cells (default: false)
    @param cols Terminal width in columns (optional)
    @param header_list List of header strings (determines column count)
    @param rows_list List of row data, where each row is a list of cell strings
    @param col_opts Optional list of column options (one per column)
    @param cursor Index of selected row (0-indexed)
    @param sel_col Index of selected column (0-indexed)
    @param opts Rendering options
    @return Rendered table string
*)
val render_table_generic_with_opts :
  ?backend:Widgets.backend ->
  ?wrap:bool ->
  cols:int option ->
  header_list:string list ->
  rows_list:string list list ->
  cursor:int ->
  sel_col:int ->
  opts:render_opts ->
  ?col_opts:column_opts list ->
  unit ->
  string

(** {1 Polymorphic Table API} *)

(** Polymorphic table module for type-safe tabular data. *)
module Table : sig
  (** Column definition mapping data to strings *)
  type 'a column = {
    header : string;  (** Column header text *)
    to_string : 'a -> string;  (** Function to convert data to display string *)
  }

  (** Column layout options *)
  type column_layout = {
    min_width : int option;  (** Minimum column width *)
    max_width : int option;  (** Maximum column width *)
    weight : int option;  (** Proportional width weight for distribution *)
    pad_left : int option;  (** Left padding *)
    pad_right : int option;  (** Right padding *)
  }

  (** Polymorphic table state *)
  type 'a t = {
    cols : int option;
    columns : 'a column list;
    opts : render_opts;
    rows : 'a list;
    cursor : int;
    layout : column_layout list option;
  }

  (** Create a new table.

      @param cols Terminal width (optional)
      @param opts Rendering options (default: default_opts)
      @param layout Column layout specifications (optional)
      @param columns List of column definitions
      @param rows List of data rows
  *)
  val create :
    ?cols:int ->
    ?opts:render_opts ->
    ?layout:column_layout list ->
    columns:'a column list ->
    rows:'a list ->
    unit ->
    'a t

  (** Update table rows and adjust cursor if needed. *)
  val set_rows : 'a t -> 'a list -> 'a t

  (** Move cursor by delta (clamped to valid range). *)
  val move_cursor : 'a t -> int -> 'a t

  (** Get current cursor position. *)
  val cursor : 'a t -> int

  (** Get number of rows. *)
  val rows : 'a t -> int

  (** Set column layout. *)
  val set_layout : 'a t -> column_layout list -> 'a t

  (** Render the table. *)
  val render : 'a t -> string

  (** Get the currently selected row. *)
  val get_selected : 'a t -> 'a option
end
