(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation

let name = "keys"

(* ================================================================ *)
(* Types (aliased from Keys_page_view)                               *)
(* ================================================================ *)

type dir_group = Keys_page_view.dir_group = {
  base_dir : string;
  keys : Keys_reader.key_info list;
  error : string option;
}

type state = Keys_page_view.state = {
  groups : dir_group list;
  selected : int;
  total_keys : int;
}

type msg = unit

type pstate = state Navigation.t

(* ================================================================ *)
(* Data loading (I/O — called in init, not in view)                  *)
(* ================================================================ *)

(** Get default Octez client base directory *)
let default_client_base_dir () =
  Filename.concat (Paths.home_dir ()) ".tezos-client"

(** Count total number of keys across all groups *)
let count_keys groups =
  List.fold_left (fun acc g -> acc + List.length g.keys) 0 groups

(** Load keys from a base directory *)
let load_keys_from_dir base_dir =
  match Keys_reader.read_public_key_hashes ~base_dir with
  | Ok keys -> {base_dir; keys; error = None}
  | Error (`Msg msg) ->
      (* Only show error if directory exists but has read issues *)
      if Sys.file_exists base_dir then {base_dir; keys = []; error = Some msg}
      else {base_dir; keys = []; error = None}

(** Remove trailing slash from path for normalization *)
let normalize_path path =
  let len = String.length path in
  if len > 1 && String.get path (len - 1) = '/' then String.sub path 0 (len - 1)
  else path

(** Get all base directories to scan for keys *)
let get_all_base_dirs () =
  let default_dir = default_client_base_dir () in
  let managed_dirs =
    match Directory_registry.list ~dir_type:Client_base_dir () with
    | Ok entries ->
        List.map
          (fun (e : Directory_registry.directory_entry) -> e.path)
          entries
    | Error _ -> []
  in
  (* Put default first, then managed dirs. Deduplicate in case default is also registered. *)
  (* Normalize paths to handle trailing slash differences *)
  let all_dirs = default_dir :: managed_dirs in
  let normalized = List.map normalize_path all_dirs in
  List.sort_uniq String.compare normalized |> List.sort String.compare

(** Get all keys from all base directories.
    Returns (key_hash, alias, base_dir) tuples. *)
let get_all_keys () =
  let all_dirs = get_all_base_dirs () in
  all_dirs
  |> List.map (fun base_dir ->
      match Keys_reader.read_public_key_hashes ~base_dir with
      | Ok keys ->
          List.map
            (fun (k : Keys_reader.key_info) -> (k.value, k.name, base_dir))
            keys
      | Error _ -> [])
  |> List.flatten

(** Initialize page state by scanning all base directories *)
let init () =
  let all_dirs = get_all_base_dirs () in
  let groups =
    all_dirs
    |> List.map load_keys_from_dir
    (* Only keep groups that have keys or errors *)
    |> List.filter (fun g -> g.keys <> [] || g.error <> None)
  in
  let total_keys = count_keys groups in
  Navigation.make {groups; selected = 0; total_keys}

let update ps _ = ps

let refresh ps = ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "?" "Help"]

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  Keys_page_view.view s ~focus ~size

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

(** Move cursor by delta positions *)
let move_selection ps delta =
  Navigation.update
    (fun s ->
      if s.total_keys = 0 then s
      else
        let selected = max 0 (min (s.total_keys - 1) (s.selected + delta)) in
        {s with selected})
    ps

(** Jump to first key *)
let jump_to_top ps = Navigation.update (fun s -> {s with selected = 0}) ps

(** Jump to last key *)
let jump_to_bottom ps =
  Navigation.update (fun s -> {s with selected = max 0 (s.total_keys - 1)}) ps

(** Handle keyboard input *)
let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape | Some (Keys.Char "q") -> Navigation.back ps
    | Some Keys.Up | Some (Keys.Char "k") -> move_selection ps (-1)
    | Some Keys.Down | Some (Keys.Char "j") -> move_selection ps 1
    | Some (Keys.Char "g") -> jump_to_top ps
    | Some (Keys.Char "G") -> jump_to_bottom ps
    | Some (Keys.Char "?") ->
        Modal_helpers.show_error
          ~title:"Keys Page Help"
          "j/Down      Move down\n\
           k/Up        Move up\n\
           g           Jump to top\n\
           G           Jump to bottom\n\
           Esc/q       Back\n\
           ?           This help" ;
        ps
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handled_keys = handled_keys

  let keymap = keymap

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [{key = "Esc"; help = "Back"}; {key = "?"; help = "Help"}]

  let has_modal = has_modal
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = "keys"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

(** Expose internals for testing *)
module Internal_for_tests = struct
  let default_client_base_dir = default_client_base_dir

  let get_all_base_dirs = get_all_base_dirs
end

(** Register the page in the global registry *)
let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
