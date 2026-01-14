(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let bounds values =
  if values = [] then (0., 0.)
  else
    let min_val = List.fold_left min Float.infinity values in
    let max_val = List.fold_left max Float.neg_infinity values in
    (min_val, max_val)

let scale ~value ~min_val ~max_val ~display_max =
  let range = max_val -. min_val in
  if range = 0. then 0
  else
    let ratio = (value -. min_val) /. range in
    min display_max (max 0 (int_of_float (ratio *. float_of_int display_max)))

let format_value value =
  let abs_val = abs_float value in
  if abs_val >= 1000. then Printf.sprintf "%.0f" value
  else if abs_val >= 10. then Printf.sprintf "%.1f" value
  else Printf.sprintf "%.2f" value

let format_label ~value ~unit_ =
  Printf.sprintf "%s%s" (format_value value) unit_

let tick_positions ~count ~max =
  if count <= 0 then []
  else if count = 1 then [max / 2]
  else
    let step = float_of_int max /. float_of_int (count - 1) in
    List.init count (fun i -> int_of_float (float_of_int i *. step))

let nice_number value ~round_up =
  if value = 0. then 0.
  else
    let exp = floor (log10 (abs_float value)) in
    let frac = value /. (10. ** exp) in
    let nice_frac =
      if round_up then
        if frac <= 1. then 1.
        else if frac <= 2. then 2.
        else if frac <= 5. then 5.
        else 10.
      else if frac < 1.5 then 1.
      else if frac < 3.5 then 2.
      else if frac < 7.5 then 5.
      else 10.
    in
    nice_frac *. (10. ** exp)

(** Render braille canvas with color styling from a 2D style array.
    This is the common pattern used by sparkline, line_chart, and bar_chart. *)
let render_braille_with_colors canvas styles =
  Braille_canvas.render_with canvas ~f:(fun ~x ~y ch ->
      match styles.(y).(x) with
      | Some color -> Widgets.ansi color ch
      | None -> ch)
