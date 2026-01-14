(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@coverage off]

module Logger_capability = Miaou_interfaces.Logger_capability

type event =
  [ `Quit
  | `Refresh
  | `Enter
  | `NextPage
  | `PrevPage
  | `Up
  | `Down
  | `Left
  | `Right
  | `Other of string ]

let read_key_blocking ~fd ~pending ~resize_pending ~pager_last_notify
    ~pager_notify_debounce_s =
  let refill timeout =
    try
      let r, _, _ = Unix.select [fd] [] [] timeout in
      if r = [] then 0
      else
        let b = Bytes.create 256 in
        try
          let n = Unix.read fd b 0 256 in
          if n <= 0 then 0
          else (
            pending := !pending ^ Bytes.sub_string b 0 n ;
            n)
        with Unix.Unix_error (Unix.EINTR, _, _) -> 0
    with Unix.Unix_error (Unix.EINTR, _, _) -> 0
  in
  try
    if Atomic.get resize_pending then (
      Atomic.set resize_pending false ;
      `Refresh)
    else
      let last = Atomic.get pager_last_notify in
      let now = Unix.gettimeofday () in
      if last > 0. && now -. last >= pager_notify_debounce_s then (
        Atomic.set pager_last_notify 0.0 ;
        `Refresh)
      else (
        if String.length !pending = 0 then ignore (refill 0.15) ;
        if String.length !pending = 0 then pending := "\000" ^ !pending ;
        if String.length !pending = 0 then raise End_of_file ;
        let first = String.get !pending 0 in
        if Char.code first <> 27 then (
          pending :=
            if String.length !pending > 1 then
              String.sub !pending 1 (String.length !pending - 1)
            else "" ;
          if first = '\000' then `Refresh
          else if first = '\n' || first = '\r' then `Enter
          else if Char.code first = 9 then `NextPage
          else if Char.code first = 127 then `Other "Backspace"
          else
            let code = Char.code first in
            if code >= 1 && code <= 26 then
              let letter = Char.chr (code + 96) in
              `Other ("C-" ^ String.make 1 letter)
            else `Other (String.make 1 first))
        else (
          for _ = 1 to 5 do
            if String.length !pending >= 3 then () else ignore (refill 0.02)
          done ;
          let len = String.length !pending in
          if len = 1 then (
            pending := "" ;
            `Other "Esc")
          else if len >= 3 && String.get !pending 1 = '[' then (
            if len >= 6 && String.get !pending 2 = '<' then (
              let rec ensure_full_mouse timeout =
                if timeout <= 0 then ()
                else
                  let l = String.length !pending in
                  if l > 0 then (
                    let last = String.get !pending (l - 1) in
                    if last = 'M' || last = 'm' then ()
                    else ignore (refill 0.02) ;
                    ensure_full_mouse (timeout - 1))
                  else (
                    ignore (refill 0.02) ;
                    ensure_full_mouse (timeout - 1))
              in
              ensure_full_mouse 20 ;
              let seq = !pending in
              let l = String.length seq in
              let term_idx =
                let rec find i =
                  if i >= l then l - 1
                  else
                    let c = String.get seq i in
                    if c = 'M' || c = 'm' then i else find (i + 1)
                in
                find 0
              in
              let chunk = String.sub seq 0 (min (term_idx + 1) l) in
              pending :=
                if String.length seq > String.length chunk then
                  String.sub
                    seq
                    (String.length chunk)
                    (String.length seq - String.length chunk)
                else "" ;
              let parsed =
                try
                  if String.length chunk < 6 then None
                  else if String.get chunk 0 <> '\027' then None
                  else if String.get chunk 1 <> '[' then None
                  else if String.get chunk 2 <> '<' then None
                  else
                    let body = String.sub chunk 3 (String.length chunk - 4) in
                    let lastc = String.get chunk (String.length chunk - 1) in
                    let parts = String.split_on_char ';' body in
                    match parts with
                    | [b; c; r] -> (
                        let btn = int_of_string_opt b in
                        let col = int_of_string_opt c in
                        let row =
                          let r' = String.sub r 0 (String.length r - 0) in
                          int_of_string_opt (String.trim r')
                        in
                        match (btn, col, row) with
                        | Some _btn, Some col, Some row -> Some (row, col, lastc)
                        | _ -> None)
                    | _ -> None
                with _ -> None
              in
              match parsed with
              | Some (row, col, lastc) ->
                  (match Logger_capability.get () with
                  | Some logger ->
                      logger.logf
                        Debug
                        (Printf.sprintf "MOUSE: row=%d col=%d" row col)
                  | None -> ()) ;
                  if lastc = 'm' then
                    `Other (Printf.sprintf "Mouse:%d:%d" row col)
                  else `Other "MouseMove"
              | None -> `Other "Esc")
            else
              let code = String.get !pending 2 in
              pending := if len > 3 then String.sub !pending 3 (len - 3) else "" ;
              match code with
              | 'M' ->
                  let rec ensure_bytes n timeout =
                    if timeout <= 0 then false
                    else if String.length !pending >= n then true
                    else (
                      ignore (refill 0.02) ;
                      ensure_bytes n (timeout - 1))
                  in
                  if ensure_bytes 3 20 then (
                    let _b = String.get !pending 0 in
                    let x = String.get !pending 1 in
                    let y = String.get !pending 2 in
                    pending :=
                      if String.length !pending > 3 then
                        String.sub !pending 3 (String.length !pending - 3)
                      else "" ;
                    let col = max 1 (Char.code x - 32) in
                    let row = max 1 (Char.code y - 32) in
                    (match Logger_capability.get () with
                    | Some logger ->
                        logger.logf
                          Debug
                          (Printf.sprintf "MOUSE_X10: row=%d col=%d" row col)
                    | None -> ()) ;
                    `Other (Printf.sprintf "Mouse:%d:%d" row col))
                  else `Other "Esc"
              | 'A' -> `Up
              | 'B' -> `Down
              | 'C' -> `Right
              | 'D' -> `Left
              | '3' ->
                  if String.length !pending > 0 && String.get !pending 0 = '~'
                  then (
                    pending :=
                      if String.length !pending > 1 then
                        String.sub !pending 1 (String.length !pending - 1)
                      else "" ;
                    `Other "Delete")
                  else `Other "3"
              | c -> `Other (String.make 1 c))
          else if len >= 2 && String.get !pending 1 = 'O' then (
            let code = if len >= 3 then String.get !pending 2 else '\000' in
            pending := if len > 2 then String.sub !pending 2 (len - 2) else "" ;
            match code with
            | 'A' -> `Up
            | 'B' -> `Down
            | 'C' -> `Right
            | 'D' -> `Left
            | '\000' -> `Other "Esc"
            | c -> `Other (String.make 1 c))
          else if len >= 2 then (
            let second = String.get !pending 1 in
            pending := if len > 2 then String.sub !pending 2 (len - 2) else "" ;
            `Other (String.make 1 second))
          else (
            pending := "" ;
            `Other "Esc")))
  with
  | End_of_file -> `Quit
  | Unix.Unix_error _ -> `Quit
