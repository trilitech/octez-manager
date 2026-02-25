(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Theme = Miaou_style.Theme
module Theme_loader = Miaou_style.Theme_loader
module Builtin_themes = Miaou_style.Builtin_themes
module Style_context = Miaou_style.Style_context

module Theme_assets = struct
  let dark_json = [%blob "themes/dark.json"]

  let light_json = [%blob "themes/light.json"]
end

type theme_info = {
  id : string;
  name : string;
  description : string;
  dark_mode : bool;
  source : [`Builtin | `Miaou | `User];
}

let themes_dir () = Filename.concat (Paths.registry_root ()) "themes"

(** Path to saved theme preference *)
let preference_path () = Filename.concat (Paths.registry_root ()) "theme"

(** Load saved theme preference from disk *)
let load_preference () =
  let path = preference_path () in
  if Sys.file_exists path then (
    let ic = open_in path in
    let name = input_line ic in
    close_in ic ;
    Some (String.trim name))
  else None

(** Save theme preference to disk *)
let save_preference name =
  let path = preference_path () in
  let oc = open_out path in
  output_string oc name ;
  close_out oc

(** Current theme reference - updated when theme is switched *)
let current_theme : Theme.t ref = ref Theme.default

(** Get current theme *)
let get_current () = !current_theme

(** Set current theme and export to MIAOU_THEME env var.
    The Miaou Matrix driver reads theme from MIAOU_THEME, so we write
    our theme JSON to a persistent file and set the env var to point to it. *)
let set_current theme =
  current_theme := theme ;
  (* Write theme to persistent file for Miaou driver to pick up *)
  let json = Theme.to_yojson theme in
  let json_str = Yojson.Safe.pretty_to_string json in
  let theme_path =
    Filename.concat (Paths.registry_root ()) "octez-manager-theme.json"
  in
  (* Ensure the registry directory exists *)
  let registry_dir = Paths.registry_root () in
  if not (Sys.file_exists registry_dir) then Unix.mkdir registry_dir 0o755 ;
  let oc = open_out theme_path in
  output_string oc json_str ;
  close_out oc ;
  Unix.putenv "MIAOU_THEME" theme_path

(** List available themes: built-ins + Miaou built-ins + files in themes dir *)
let list_available () =
  let om_builtins = ["dark"; "light"] in
  let miaou_builtins = Builtin_themes.list_builtin_ids () in
  let dir_themes =
    let dir = themes_dir () in
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".json")
      |> List.map Filename.remove_extension
    else []
  in
  om_builtins @ miaou_builtins @ dir_themes |> List.sort_uniq String.compare

(** List all themes with full info for the picker *)
let list_all () =
  (* octez-manager built-in themes *)
  let om_builtins =
    [
      {
        id = "dark";
        name = "Dark";
        description = "Default dark theme";
        dark_mode = true;
        source = `Builtin;
      };
      {
        id = "light";
        name = "Light";
        description = "Default light theme";
        dark_mode = false;
        source = `Builtin;
      };
    ]
  in
  (* Miaou built-in themes *)
  let miaou_builtins =
    Builtin_themes.list_builtin ()
    |> List.map (fun t ->
        {
          id = t.Builtin_themes.id;
          name = t.Builtin_themes.name;
          description = t.Builtin_themes.description;
          dark_mode = t.Builtin_themes.dark_mode;
          source = `Miaou;
        })
  in
  (* User themes from themes directory *)
  let user_themes =
    let dir = themes_dir () in
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".json")
      |> List.map Filename.remove_extension
      |> List.map (fun id ->
          {
            id;
            name = String.capitalize_ascii id;
            description = "User theme";
            dark_mode = true;
            (* assume dark by default *)
            source = `User;
          })
    else []
  in
  om_builtins @ miaou_builtins @ user_themes

(** Try to load an octez-manager built-in theme *)
let om_builtin_theme name =
  let name_lower = String.lowercase_ascii (String.trim name) in
  match name_lower with
  | "dark" | "default" -> Theme_loader.of_json_string Theme_assets.dark_json
  | "light" -> Theme_loader.of_json_string Theme_assets.light_json
  | _ -> Error "unknown built-in theme"

let load_from_path path =
  if Sys.file_exists path then Theme_loader.load_file path
  else Error (Printf.sprintf "Theme file not found: %s" path)

let load_from_dir name =
  let path = Filename.concat (themes_dir ()) (name ^ ".json") in
  load_from_path path

(** Try all sources in order: om built-in -> Miaou built-in -> path -> user dir *)
let try_load_theme raw =
  (* First: octez-manager built-in *)
  match om_builtin_theme raw with
  | Ok t -> Ok (Theme.merge ~base:Theme.default ~overlay:t)
  | Error _ -> (
      (* Second: Miaou built-in *)
      match Builtin_themes.get_builtin raw with
      | Some t -> Ok t
      | None -> (
          (* Third: direct file path *)
          match load_from_path raw with
          | Ok t -> Ok (Theme.merge ~base:Theme.default ~overlay:t)
          | Error _ -> (
              (* Fourth: user themes directory *)
              match load_from_dir raw with
              | Ok t -> Ok (Theme.merge ~base:Theme.default ~overlay:t)
              | Error msg -> Error msg)))

let load ?name () =
  let requested =
    match name with
    | Some v -> Some v
    | None -> (
        (* Check env var first, then saved preference *)
        match Sys.getenv_opt "OCTEZ_MANAGER_THEME" with
        | Some _ as v -> v
        | None -> load_preference ())
  in
  let fallback =
    match om_builtin_theme "dark" with Ok t -> t | Error _ -> Theme.default
  in
  let theme, warning =
    match requested with
    | None -> (fallback, None)
    | Some raw -> (
        match try_load_theme raw with
        | Ok t -> (t, None)
        | Error msg -> (fallback, Some msg))
  in
  let warning =
    match warning with
    | Some _ as w -> w
    | None -> (
        match Theme.validate theme with
        | [] -> None
        | w :: _ -> Some ("Theme warning: " ^ w))
  in
  (theme, warning)
