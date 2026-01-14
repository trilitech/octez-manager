(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

module Logger_capability = Miaou_interfaces.Logger_capability

let debug_enabled =
  lazy
    (let get_env var =
       match Miaou_interfaces.System.get () with
       | Some sys -> sys.get_env_var var
       | None -> Sys.getenv_opt var
     in
     match get_env "MIAOU_TUI_DEBUG_MODAL" with
     | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
     | _ -> false)

let dprintf fmt =
  if Lazy.force debug_enabled then Printf.eprintf fmt
  else Printf.ifprintf stdout fmt

(* Append diagnostics using Logger capability if available *)
let append_log line =
  match Logger_capability.get () with
  | Some logger -> logger.logf Debug line
  | None -> ()

let trim_preview (s : string) ~max_lines ~max_chars =
  let lines = String.split_on_char '\n' s in
  let lines =
    let rec take n xs acc =
      match (n, xs) with
      | 0, _ | _, [] -> List.rev acc
      | n, x :: xt -> take (n - 1) xt (x :: acc)
    in
    take max_lines lines []
  in
  let joined = String.concat "\n" lines in
  if String.length joined <= max_chars then joined
  else String.sub joined 0 (max 0 (max_chars - 1)) ^ "…"

let render_overlay ~(cols : int option) ~base ?rows () =
  let frames = Modal_snapshot.get_stack_snapshot () in
  dprintf
    "[DEBUG] Modal_renderer.render_overlay: stack_size=%d\n%!"
    (List.length frames) ;
  if Lazy.force debug_enabled then
    append_log (Printf.sprintf "RENDERER: stack_size=%d" (List.length frames)) ;
  if frames = [] then None
  else
    (* Deduplicate by title keeping last occurrence *)
    let with_idx = List.mapi (fun i fr -> (i, fr)) frames in
    let last_idx =
      List.fold_left
        (fun acc (i, (t, _l, _m, _d, _v)) ->
          let acc = List.remove_assoc t acc in
          (t, i) :: acc)
        []
        with_idx
    in
    let frames =
      List.filter
        (fun (i, (t, _l, _m, _d, _v)) ->
          match List.assoc_opt t last_idx with Some j -> i = j | None -> true)
        with_idx
      |> List.map snd
    in
    dprintf
      "[DEBUG] Modal_renderer.render_overlay: after dedup, rendering %d frames\n\
       %!"
      (List.length frames) ;
    if Lazy.force debug_enabled then
      append_log
        (Printf.sprintf "RENDERER: after_dedup=%d" (List.length frames)) ;
    (* Use the shared modal wrapping helper from miaou_internals to keep
       wrapping logic centralized and testable. *)
    let wrap_content_to_width = Modal_utils.wrap_content_to_width in

    let rendered =
      List.fold_left
        (fun acc
             (title, left_opt, max_width_spec_opt, dim_background, view_thunk)
           ->
          let cols_val = match cols with Some c -> c | None -> 80 in
          let max_width_opt =
            match max_width_spec_opt with
            | None -> None
            | Some spec -> Modal_snapshot.resolve_max_width spec ~cols:cols_val
          in
          (try
             append_log
               (Printf.sprintf
                  "RENDERER_FRAME: title='%s' left=%s max_w=%s dim_bg=%b"
                  title
                  (match left_opt with
                  | Some l -> string_of_int l
                  | None -> "-")
                  (match max_width_opt with
                  | Some w -> string_of_int w
                  | None -> "-")
                  dim_background)
           with _ -> ()) ;
          (* Determine effective left and max_width based on available cols. *)
          let rows_val =
            match rows with
            | Some r -> r
            | None -> List.length (String.split_on_char '\n' base)
          in
          let geom =
            Modal_utils.compute_modal_geometry
              ~cols:cols_val
              ~rows:rows_val
              ~left_opt
              ~max_width_opt
          in
          let raw_content =
            view_thunk
              {LTerm_geom.rows = geom.max_content_h; cols = geom.content_width}
          in
          (try
             let preview =
               trim_preview raw_content ~max_lines:8 ~max_chars:400
             in
             append_log
               (Printf.sprintf
                  "RENDERER_FRAME_PREVIEW: title='%s'\n%s"
                  title
                  preview)
           with _ -> ()) ;
          let content = wrap_content_to_width raw_content geom.content_width in
          let dim_background = dim_background || true in
          (* Derive an adaptive max_height from the current base output lines.
         Keep a higher floor so content like Select lists isn't clipped by header lines. *)
          let max_height = geom.max_height in
          let out =
            Miaou_widgets_display.Widgets.center_modal
              ~cols:(Some cols_val)
              ~rows:rows_val
              ~title
              ~dim_background
              ~left:geom.left
              ~max_width:geom.max_width
              ~max_height
              ~content
              ~base:acc
              ()
          in
          (try
             append_log
               (Printf.sprintf
                  "RENDERER_COMPOSED: title='%s' out_len=%d"
                  title
                  (String.length out))
           with _ -> ()) ;
          out)
        base
        frames
    in
    Some rendered
