(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type config = {
  font_path : string option;
  font_size : int;
  window_title : string;
  fg : Sdl_colors.color;
  bg : Sdl_colors.color;
  gradient : bool;
  scale : float;
  transition : [`None | `Slide | `Fade | `Explode | `Random];
}

let detect_display_scale () =
  match Sys.getenv_opt "MIAOU_SDL_SCALE" with
  | Some v -> (
      try float_of_string v
      with _ ->
        Printf.eprintf "Warning: invalid MIAOU_SDL_SCALE=%s, using 1.0\n%!" v ;
        1.0)
  | None -> 1.0

let default_config =
  let fg = {Sdl_colors.r = 200; g = 200; b = 200; a = 255} in
  let bg = {Sdl_colors.r = 20; g = 20; b = 30; a = 255} in
  let transition =
    match Sys.getenv_opt "MIAOU_SDL_TRANSITION" with
    | Some "slide" -> `Slide
    | Some "fade" -> `Fade
    | Some "explode" -> `Explode
    | Some "random" -> `Random
    | Some "none" | None -> `None
    | Some other ->
        Printf.eprintf
          "Warning: unknown MIAOU_SDL_TRANSITION=%s, using 'none'\n%!"
          other ;
        `None
  in
  {
    font_path = None;
    font_size = 14;
    window_title = "Miaou TUI (SDL)";
    fg;
    bg;
    gradient = true;
    scale = detect_display_scale ();
    transition;
  }

let font_candidates =
  [
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf";
    "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf";
    "/Library/Fonts/Menlo-Regular.ttf";
    "/System/Library/Fonts/Menlo.ttc";
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf";
  ]

let pick_font_path (cfg : config) =
  match cfg.font_path with
  | Some p when Sys.file_exists p -> Ok p
  | Some p ->
      Error
        (Printf.sprintf
           "Configured MIAOU_SDL_FONT does not exist: %s (current working \
            directory: %s)"
           p
           (Sys.getcwd ()))
  | None -> (
      match Sys.getenv_opt "MIAOU_SDL_FONT" with
      | Some env when Sys.file_exists env -> Ok env
      | Some env ->
          Error
            (Printf.sprintf
               "Configured MIAOU_SDL_FONT does not exist: %s (cwd: %s)"
               env
               (Sys.getcwd ()))
      | None -> (
          let available = List.filter Sys.file_exists font_candidates in
          match available with
          | p :: _ -> Ok p
          | [] ->
              Error
                (Printf.sprintf
                   "Could not find any monospaced font. Provide \
                    MIAOU_SDL_FONT=<path> to a .ttf file. Probed: %s"
                   (String.concat ", " font_candidates))))
