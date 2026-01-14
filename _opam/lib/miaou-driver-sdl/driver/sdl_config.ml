(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl
module Colors = Sdl_colors

type color = Colors.color

type config = {
  font_path : string option;
  font_size : int;
  window_title : string;
  fg : color;
  bg : color;
  gradient : bool;
  scale : float;
  transition : [`None | `Slide | `Fade | `Explode | `Random];
}

let detect_display_scale () =
  match Sdl.get_display_dpi 0 with
  | Ok (_ddpi, hdpi, _vdpi) ->
      let scale = hdpi /. 96.0 in
      let clamped = max 1.0 (min 3.0 scale) in
      Some clamped
  | Error _ -> None

let default_config =
  {
    font_path = None;
    font_size =
      (match Sys.getenv_opt "MIAOU_SDL_FONT_SIZE" with
      | Some s -> ( try int_of_string s with _ -> 16)
      | None -> 16);
    window_title =
      Sys.getenv_opt "MIAOU_SDL_WINDOW_TITLE" |> Option.value ~default:"Miaou";
    fg = Colors.{r = 235; g = 235; b = 235; a = 255};
    bg = Colors.{r = 20; g = 20; b = 20; a = 255};
    gradient =
      (match Sys.getenv_opt "MIAOU_SDL_GRADIENT" with
      | Some v ->
          let v = String.lowercase_ascii (String.trim v) in
          not (v = "0" || v = "false" || v = "off")
      | None -> true);
    scale =
      (match Sys.getenv_opt "MIAOU_SDL_SCALE" with
      | Some v -> ( try float_of_string v with _ -> 2.0)
      | None -> detect_display_scale () |> Option.value ~default:2.0);
    transition =
      (match Sys.getenv_opt "MIAOU_SDL_TRANSITION" with
      | Some v ->
          let v = String.lowercase_ascii (String.trim v) in
          if v = "slide" then `Slide
          else if v = "fade" then `Fade
          else if v = "explode" then `Explode
          else if v = "random" then `Random
          else `None
      | None -> `Slide);
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
              let buf = Buffer.create 256 in
              List.iteri
                (fun i p ->
                  if i > 0 then Buffer.add_string buf ", " ;
                  Buffer.add_string buf p)
                font_candidates ;
              Error
                (Printf.sprintf
                   "Could not find any monospaced font. Provide \
                    MIAOU_SDL_FONT=<path> to a .ttf file. Probed: %s"
                   (Buffer.contents buf))))
