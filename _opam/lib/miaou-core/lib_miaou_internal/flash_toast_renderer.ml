(* SPDX-License-Identifier: MIT *)
(* Render flash bus snapshots as toast notifications. *)

[@@@warning "-32-34-37-69"]

module Toast = Miaou_widgets_layout.Toast_widget
module Flash_bus = Flash_bus

let severity_of = function
  | Flash_bus.Info -> Toast.Info
  | Success -> Toast.Success
  | Warn -> Toast.Warn
  | Error -> Toast.Error

let to_toast ?position entries =
  let t = Toast.empty ?position () in
  let now = Unix.gettimeofday () in
  List.fold_left
    (fun acc (lvl, msg) -> Toast.enqueue ~now acc (severity_of lvl) msg)
    t
    entries

let render_snapshot ?position ~cols entries =
  let toast = to_toast ?position entries in
  Toast.render toast ~cols
