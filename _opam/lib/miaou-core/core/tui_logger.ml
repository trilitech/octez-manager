(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

open Miaou_interfaces.Logger_capability

let enabled = ref false

let ch : out_channel option ref = ref None

let level_to_string = function
  | Debug -> "DBG"
  | Info -> "INF"
  | Warning -> "WRN"
  | Error -> "ERR"

let timestamp () =
  let open Unix in
  let tm = gmtime (time ()) in
  Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let set_enabled b = enabled := b

let set_logfile path_opt =
  try
    (match !ch with Some oc -> close_out_noerr oc | None -> ()) ;
    (ch :=
       match path_opt with
       | None -> None
       | Some p ->
           Some
             (open_out_gen
                [Open_creat; Open_text; Open_wronly; Open_append]
                0o644
                p)) ;
    Ok ()
  with e -> Error (Printexc.to_string e)

let logf level msg =
  if not !enabled then ()
  else
    let formatted_msg =
      Printf.sprintf "[%s] %s: %s" (timestamp ()) (level_to_string level) msg
    in
    match !ch with
    | Some oc ->
        output_string oc formatted_msg ;
        output_char oc '\n' ;
        flush oc
    | None ->
        output_string stderr formatted_msg ;
        output_char stderr '\n' ;
        flush stderr
