(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

module Helpers = Miaou_helpers.Helpers

type modal_geometry = {
  left : int;
  max_width : int;
  content_width : int;
  max_height : int;
  max_content_h : int;
}

let compute_modal_geometry ~cols ~rows ~left_opt ~max_width_opt =
  let cols = max 0 cols in
  let rows = max 0 rows in
  let left =
    match left_opt with
    | Some v -> max 0 (min v (max 0 (cols - 10)))
    | None ->
        max 0 ((cols - match max_width_opt with Some w -> w | None -> 76) / 2)
  in
  let effective_requested =
    match max_width_opt with Some v -> v | None -> 76
  in
  let max_width = max 10 (min effective_requested (max 10 (cols - left - 4))) in
  let content_width =
    (* Match Modal_renderer logic: reserve 2 for borders and 2 for padding. *)
    max 0 (max_width - 4)
  in
  let max_height = min rows (max 14 (rows - 4)) in
  let max_content_h = max 0 (max_height - 2) in
  {left; max_width; content_width; max_height; max_content_h}

let tokenize_ansi_utf8 (s : string) : string list =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then List.rev acc
    else if Miaou_widgets_display.Widgets.is_esc_start s i then
      let j = Miaou_widgets_display.Widgets.skip_ansi_until_m s (i + 2) in
      let j = if j < 0 then len else j in
      let token = String.sub s i (j - i) in
      loop j (token :: acc)
    else
      let start = i in
      let i = ref (i + 1) in
      while !i < len && Char.code s.[!i] land 0xC0 = 0x80 do
        incr i
      done ;
      let token = String.sub s start (!i - start) in
      loop !i (token :: acc)
  in
  loop 0 []

let concat_rev parts =
  match parts with
  | [] -> ""
  | _ ->
      let buf =
        let est = List.fold_left (fun acc l -> acc + String.length l) 0 parts in
        Buffer.create est
      in
      List.iter (Buffer.add_string buf) parts ;
      Buffer.contents buf

let wrap_line_to_width (line : string) (width : int) : string list =
  if width <= 0 then [line]
  else
    let tokens = tokenize_ansi_utf8 line in
    let buf = Buffer.create (String.length line) in
    let cur_vis = ref 0 in
    let module W = Miaou_widgets_display.Widgets in
    let visible_byte_index_of_pos = W.visible_byte_index_of_pos in
    let rstrip_spaces s =
      let len = String.length s in
      let rec find idx =
        if idx < 0 then 0 else if s.[idx] = ' ' then find (idx - 1) else idx + 1
      in
      let trimmed_len = find (len - 1) in
      if trimmed_len = len then s else String.sub s 0 trimmed_len
    in
    let last_break =
      ref None
      (* Stored as (buf_len_before_whitespace, whitespace_bytes). *)
    in
    let flush_at_break (buf_len, ws_bytes) =
      let full = Buffer.contents buf in
      let total_len = String.length full in
      let line =
        if buf_len <= 0 then "" else String.sub full 0 buf_len |> rstrip_spaces
      in
      let rest_start = min total_len (buf_len + ws_bytes) in
      let rest_len = max 0 (total_len - rest_start) in
      let rest =
        if rest_len = 0 then "" else String.sub full rest_start rest_len
      in
      Buffer.clear buf ;
      if rest <> "" then Buffer.add_string buf rest ;
      cur_vis := if rest = "" then 0 else W.visible_chars_count rest ;
      last_break := None ;
      line
    in
    let rec drain_overflow () =
      if !cur_vis > width && Buffer.length buf > 0 then
        match !last_break with
        | Some (buf_len, ws_bytes) when buf_len > 0 ->
            let line = flush_at_break (buf_len, ws_bytes) in
            drain_overflow_line line
        | _ ->
            let remainder = emit_hard_chunks (Buffer.contents buf) in
            Buffer.clear buf ;
            if remainder <> "" then Buffer.add_string buf remainder ;
            cur_vis := W.visible_chars_count remainder ;
            last_break := None ;
            drain_overflow ()
    and push_output line = if line <> "" then out := line :: !out else ()
    and drain_overflow_line line =
      push_output line ;
      drain_overflow ()
    and emit_hard_chunks s =
      if width <= 0 || s = "" || W.visible_chars_count s <= width then s
      else
        let idx = visible_byte_index_of_pos s (max 0 width) in
        let idx = min idx (String.length s) in
        let chunk = String.sub s 0 idx in
        let rest =
          if idx >= String.length s then ""
          else String.sub s idx (String.length s - idx)
        in
        push_output chunk ;
        emit_hard_chunks rest
    and out = ref [] in
    List.iter
      (fun tk ->
        let len_before = Buffer.length buf in
        Buffer.add_string buf tk ;
        let len_after = Buffer.length buf in
        let v = W.visible_chars_count tk in
        cur_vis := !cur_vis + v ;
        if String.length tk > 0 && tk.[0] <> '\027' && (tk = " " || tk = "\t")
        then last_break := Some (len_before, len_after - len_before)
        else if String.length tk > 0 && tk.[0] = '\027' then
          (* ANSI sequences do not affect break points or width. *) ()
        else () ;
        drain_overflow ())
      tokens ;
    let last = Buffer.contents buf in
    let out = if last = "" then !out else last :: !out in
    List.rev out

let wrap_content_to_width content content_width =
  let lines = String.split_on_char '\n' content in
  let wrapped_lines =
    List.flatten (List.map (fun l -> wrap_line_to_width l content_width) lines)
  in
  Helpers.concat_lines wrapped_lines

let wrap_line_to_width_words (line : string) (width : int) : string list =
  if width <= 0 then [line]
  else
    (* Split into words but preserve ANSI tokens within words. *)
    let tokens = tokenize_ansi_utf8 line in
    let words =
      let rec fold acc cur = function
        | [] ->
            let w = String.trim (concat_rev (List.rev cur)) in
            let acc = if w = "" then acc else w :: acc in
            List.rev acc
        | tk :: tl ->
            if String.length tk > 0 && tk.[0] <> '\027' && tk = " " then
              let w = String.trim (concat_rev (List.rev cur)) in
              let acc = if w = "" then acc else w :: acc in
              fold acc [] tl
            else fold acc (tk :: cur) tl
      in
      fold [] [] tokens
    in
    let buf = Buffer.create (String.length line) in
    let cur_vis = ref 0 in
    let flush () =
      let s = Buffer.contents buf in
      Buffer.clear buf ;
      cur_vis := 0 ;
      s
    in
    let out = ref [] in
    List.iter
      (fun word ->
        (* Visible width of the word ignoring spaces *)
        let wv = Miaou_widgets_display.Widgets.visible_chars_count word in
        let sep = if !cur_vis = 0 then "" else " " in
        let add_word () = Buffer.add_string buf (sep ^ word) in
        if wv > width then (
          (* Fallback to char wrapping for very long words *)
          if !cur_vis > 0 then out := flush () :: !out ;
          List.iter
            (fun chunk ->
              if chunk <> "" then (
                Buffer.add_string buf chunk ;
                out := flush () :: !out))
            (wrap_line_to_width word width))
        else if !cur_vis + (if sep = "" then 0 else 1) + wv > width then (
          out := flush () :: !out ;
          Buffer.add_string buf word ;
          cur_vis := wv)
        else (
          add_word () ;
          cur_vis := !cur_vis + (if sep = "" then 0 else 1) + wv))
      words ;
    let last = Buffer.contents buf in
    let out = if last = "" then !out else last :: !out in
    List.rev out

let wrap_content_to_width_words content content_width =
  let lines = String.split_on_char '\n' content in
  let wrapped_lines =
    List.flatten
      (List.map (fun l -> wrap_line_to_width_words l content_width) lines)
  in
  Helpers.concat_lines wrapped_lines

let markdown_to_ansi (s : string) : string =
  let open Miaou_widgets_display.Widgets in
  let colorize_urls (l : string) : string =
    let len = String.length l in
    let buf = Buffer.create (len + 16) in
    let has_prefix i p =
      let plen = String.length p in
      i + plen <= len && String.sub l i plen = p
    in
    let rec loop i =
      if i >= len then ()
      else if is_esc_start l i then (
        let j = skip_ansi_until_m l (i + 2) in
        let j = if j < 0 then len else j in
        Buffer.add_string buf (String.sub l i (j - i)) ;
        loop j)
      else if has_prefix i "http://" || has_prefix i "https://" then (
        let j = ref i in
        while
          !j < len
          && not
               (List.mem
                  l.[!j]
                  [' '; '\t'; '\n'; ')'; ']'; '"'; '\''; ','; ';'])
        do
          incr j
        done ;
        let url = String.sub l i (!j - i) in
        Buffer.add_string buf (fg 75 url) ;
        loop !j)
      else (
        Buffer.add_char buf l.[i] ;
        loop (i + 1))
    in
    loop 0 ;
    Buffer.contents buf
  in
  let emphasize_label (l : string) : string =
    match String.index_opt l ':' with
    | Some idx when idx > 0 ->
        let left = String.sub l 0 idx |> String.trim in
        let right =
          if idx + 1 < String.length l && l.[idx + 1] = ' ' then
            String.sub l (idx + 2) (String.length l - idx - 2)
          else String.sub l (idx + 1) (String.length l - idx - 1)
        in
        if left = "" then l else bold left ^ fg 244 ":" ^ " " ^ right
    | _ -> l
  in
  let inline_style (l : string) : string =
    let len = String.length l in
    let buf = Buffer.create (len + 32) in
    let apply ~in_bold ~in_italic ~in_code chunk =
      if chunk = "" then ""
      else if in_code then fg 114 chunk
      else if in_bold && in_italic then bold (fg 228 chunk)
      else if in_bold then bold chunk
      else if in_italic then dim chunk
      else chunk
    in
    let rec loop i last ~in_bold ~in_italic ~in_code =
      if i >= len then
        let chunk = String.sub l last (len - last) in
        Buffer.add_string buf (apply ~in_bold ~in_italic ~in_code chunk)
      else if (not in_code) && i + 1 < len && l.[i] = '*' && l.[i + 1] = '*'
      then (
        let chunk = String.sub l last (i - last) in
        Buffer.add_string buf (apply ~in_bold ~in_italic ~in_code chunk) ;
        loop (i + 2) (i + 2) ~in_bold:(not in_bold) ~in_italic ~in_code)
      else if (not in_code) && (l.[i] = '*' || l.[i] = '_') then (
        let chunk = String.sub l last (i - last) in
        Buffer.add_string buf (apply ~in_bold ~in_italic ~in_code chunk) ;
        loop (i + 1) (i + 1) ~in_bold ~in_italic:(not in_italic) ~in_code)
      else if l.[i] = '`' then (
        let chunk = String.sub l last (i - last) in
        Buffer.add_string buf (apply ~in_bold ~in_italic ~in_code chunk) ;
        loop (i + 1) (i + 1) ~in_bold ~in_italic ~in_code:(not in_code))
      else if (not in_code) && l.[i] = '[' then
        match String.index_from_opt l (i + 1) ']' with
        | Some j when j + 1 < len && l.[j + 1] = '(' -> (
            match String.index_from_opt l (j + 2) ')' with
            | Some k ->
                let chunk = String.sub l last (i - last) in
                Buffer.add_string buf (apply ~in_bold ~in_italic ~in_code chunk) ;
                let text = String.sub l (i + 1) (j - i - 1) in
                let url = String.sub l (j + 2) (k - j - 2) in
                Buffer.add_string buf (fg 75 text) ;
                Buffer.add_string buf (dim (" (" ^ url ^ ")")) ;
                loop (k + 1) (k + 1) ~in_bold ~in_italic ~in_code
            | None -> loop (i + 1) last ~in_bold ~in_italic ~in_code)
        | _ -> loop (i + 1) last ~in_bold ~in_italic ~in_code
      else loop (i + 1) last ~in_bold ~in_italic ~in_code
    in
    loop 0 0 ~in_bold:false ~in_italic:false ~in_code:false ;
    Buffer.contents buf
  in
  let is_hr l =
    let t = String.trim l in
    let len = String.length t in
    if len < 3 then false
    else
      let c = t.[0] in
      (c = '-' || c = '*' || c = '_') && String.for_all (fun ch -> ch = c) t
  in
  let lines = String.split_on_char '\n' s in
  let rec loop acc in_code = function
    | [] -> List.rev acc
    | l :: tl ->
        if String.length l >= 3 && String.sub l 0 3 = "```" then
          (* Toggle fenced code blocks but omit the fence markers themselves. *)
          loop acc (not in_code) tl
        else if in_code then loop (fg 114 l :: acc) in_code tl
        else if is_hr l then
          loop
            (dim "────────────────────────────────────────" :: acc)
            in_code
            tl
        else if String.length l > 0 && l.[0] = '>' then
          let content =
            String.sub l 1 (String.length l - 1)
            |> String.trim |> inline_style |> emphasize_label |> colorize_urls
          in
          loop (fg 244 ("▎ " ^ content) :: acc) in_code tl
        else if String.length l > 1 && l.[0] = '#' then
          let rec count i =
            if i < String.length l && l.[i] = '#' then count (i + 1) else i
          in
          let n = count 0 in
          let rest =
            let start = min (n + 1) (String.length l) in
            String.sub l start (String.length l - start) |> String.trim
          in
          if n = 1 then
            let title = bold (fg 81 rest) in
            let underline_len =
              Miaou_widgets_display.Widgets.visible_chars_count rest
            in
            let underline =
              if underline_len = 0 then None
              else Some (fg 75 (String.make underline_len '~'))
            in
            match underline with
            | Some u -> loop (u :: title :: acc) in_code tl
            | None -> loop (title :: acc) in_code tl
          else
            let styled =
              match n with 2 -> bold (fg 81 rest) | _ -> bold (fg 75 rest)
            in
            loop (styled :: acc) in_code tl
        else if String.length l > 2 && String.sub l 0 2 = "- " then
          let body =
            inline_style (String.sub l 2 (String.length l - 2))
            |> emphasize_label |> colorize_urls
          in
          loop (("  • " ^ body) :: acc) in_code tl
        else if String.length l > 3 then
          let rec scan_digits i =
            if
              i < String.length l
              && Char.code l.[i] >= 48
              && Char.code l.[i] <= 57
            then scan_digits (i + 1)
            else i
          in
          let j = scan_digits 0 in
          if
            j > 0
            && j + 1 < String.length l
            && l.[j] = '.'
            && (l.[j + 1] = ' ' || l.[j + 1] = '\t')
          then
            let num = String.sub l 0 j in
            let body =
              inline_style (String.sub l (j + 2) (String.length l - j - 2))
              |> emphasize_label |> colorize_urls
            in
            loop (("  " ^ fg 81 (num ^ ".") ^ " " ^ body) :: acc) in_code tl
          else loop (inline_style l :: acc) in_code tl
        else
          let styled = inline_style l |> emphasize_label |> colorize_urls in
          loop (styled :: acc) in_code tl
  in
  String.concat "\n" (loop [] false lines)

let center_content_to_width (content : string) (width : int) : string =
  let open Miaou_widgets_display.Widgets in
  if width <= 0 then content
  else
    let lines = String.split_on_char '\n' content in
    let center_line l =
      let v = visible_chars_count l in
      if v >= width then l
      else
        let pad = (width - v) / 2 in
        String.make pad ' ' ^ l
    in
    String.concat "\n" (List.map center_line lines)
