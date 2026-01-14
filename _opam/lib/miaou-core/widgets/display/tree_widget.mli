(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Tree widget for displaying hierarchical data structures.

    This widget provides a simple tree view for nested data, with indentation
    to show hierarchy. Commonly used for displaying JSON structures, file
    trees, or other hierarchical data.

    {b Typical usage}:
    {[
      (* Create a tree from JSON *)
      let json = Yojson.Safe.from_string {|
        {
          "name": "root",
          "children": [
            {"type": "leaf", "value": 42}
          ]
        }
      |}
      in
      let tree = Tree_widget.of_json json in
      let widget = Tree_widget.open_root tree in

      (* Render *)
      let output = Tree_widget.render widget ~focus:false in
      (* Output:
         obj
           name
             "root"
           children
             list
               obj
                 type
                   "leaf"
                 value
                   42
      *)

      (* Or create a tree manually *)
      let tree = {
        Tree_widget.label = "Root";
        children = [
          {label = "Child 1"; children = []};
          {label = "Child 2"; children = [
            {label = "Grandchild"; children = []}
          ]}
        ]
      }
      in
      let widget = Tree_widget.open_root tree in
    ]}
*)

(** Tree node with label and children *)
type node = {
  label : string;  (** Display text for this node *)
  children : node list;  (** Child nodes *)
}

(** Tree widget state with cursor position *)
type t = {
  root : node;  (** Root node of the tree *)
  cursor_path : int list;
      (** Path to current cursor position (unused currently) *)
}

(** {1 Construction} *)

(** Convert JSON value to tree node.

    Creates a tree structure from Yojson data:
    - Objects become nodes with label "obj" and children for each key
    - Lists become nodes with label "list" and children for each element
    - Primitives become leaf nodes with their string representation

    @param json Yojson value to convert
    @return Root node of the tree
*)
val of_json : Yojson.Safe.t -> node

(** Create tree widget from root node.

    @param node Root node of the tree
    @return Tree widget with cursor at position [0]
*)
val open_root : node -> t

(** {1 Rendering} *)

(** Render the tree with indentation.

    Returns a multi-line string with indentation showing hierarchy:
    - Each level is indented by 2 spaces
    - One line per node

    @param focus Whether the widget has focus (currently unused)
    @return Rendered tree string
*)
val render : t -> focus:bool -> string

(** {1 Input Handling} *)

(** Handle keyboard input.

    Currently a no-op - tree widget doesn't support navigation yet.

    @param key Key string
    @return Unchanged tree widget
*)
val handle_key : t -> key:string -> t

(** {1 Internal Rendering} *)

(** Render a node with given indentation level.

    @param indent Number of spaces to indent
    @param node Node to render
    @return Rendered string
*)
val render_node : int -> node -> string
