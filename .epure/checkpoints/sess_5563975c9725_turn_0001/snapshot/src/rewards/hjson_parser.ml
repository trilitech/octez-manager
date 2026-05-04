(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Minimal HJSON parser for importing external configuration files. *)

(* ── Comment stripping ───────────────────────────────────── *)

let strip_comments input =
  let len = String.length input in
  let buf = Buffer.create len in
  let i = ref 0 in
  let in_string = ref false in
  let in_triple = ref false in
  while !i < len do
    if
      (not !in_string) && (not !in_triple)
      && !i + 2 < len
      && input.[!i] = '\''
      && input.[!i + 1] = '\''
      && input.[!i + 2] = '\''
    then (
      Buffer.add_string buf "'''" ;
      i := !i + 3 ;
      in_triple := true)
    else if !in_triple then
      if
        !i + 2 < len
        && input.[!i] = '\''
        && input.[!i + 1] = '\''
        && input.[!i + 2] = '\''
      then (
        Buffer.add_string buf "'''" ;
        i := !i + 3 ;
        in_triple := false)
      else (
        Buffer.add_char buf input.[!i] ;
        incr i)
    else if (not !in_string) && input.[!i] = '"' then (
      in_string := true ;
      Buffer.add_char buf '"' ;
      incr i)
    else if !in_string then
      if input.[!i] = '\\' && !i + 1 < len then (
        Buffer.add_char buf input.[!i] ;
        Buffer.add_char buf input.[!i + 1] ;
        i := !i + 2)
      else if input.[!i] = '"' then (
        in_string := false ;
        Buffer.add_char buf '"' ;
        incr i)
      else (
        Buffer.add_char buf input.[!i] ;
        incr i)
    else if !i + 1 < len && input.[!i] = '/' && input.[!i + 1] = '/' then (
      while !i < len && input.[!i] <> '\n' do
        incr i
      done ;
      if !i < len then (
        Buffer.add_char buf '\n' ;
        incr i))
    else if !i + 1 < len && input.[!i] = '/' && input.[!i + 1] = '*' then (
      i := !i + 2 ;
      while !i + 1 < len && not (input.[!i] = '*' && input.[!i + 1] = '/') do
        if input.[!i] = '\n' then Buffer.add_char buf '\n' ;
        incr i
      done ;
      if !i + 1 < len then i := !i + 2)
    else if input.[!i] = '#' then (
      while !i < len && input.[!i] <> '\n' do
        incr i
      done ;
      if !i < len then (
        Buffer.add_char buf '\n' ;
        incr i))
    else (
      Buffer.add_char buf input.[!i] ;
      incr i)
  done ;
  Buffer.contents buf

(* ── Remove trailing commas ──────────────────────────────── *)

let remove_trailing_commas input =
  let len = String.length input in
  let buf = Buffer.create len in
  let i = ref 0 in
  let in_string = ref false in
  while !i < len do
    if !in_string then (
      Buffer.add_char buf input.[!i] ;
      if input.[!i] = '\\' && !i + 1 < len then (
        incr i ;
        Buffer.add_char buf input.[!i])
      else if input.[!i] = '"' then in_string := false ;
      incr i)
    else if input.[!i] = '"' then (
      in_string := true ;
      Buffer.add_char buf '"' ;
      incr i)
    else if input.[!i] = ',' then (
      (* Look ahead past whitespace for } or ] *)
      let j = ref (!i + 1) in
      while
        !j < len
        && (input.[!j] = ' '
           || input.[!j] = '\t'
           || input.[!j] = '\n'
           || input.[!j] = '\r')
      do
        incr j
      done ;
      if !j < len && (input.[!j] = '}' || input.[!j] = ']') then (
        (* Skip the comma, copy the whitespace *)
        incr i ;
        while !i < !j do
          Buffer.add_char buf input.[!i] ;
          incr i
        done)
      else (
        Buffer.add_char buf ',' ;
        incr i))
    else (
      Buffer.add_char buf input.[!i] ;
      incr i)
  done ;
  Buffer.contents buf

(* ── JSON normalization ──────────────────────────────────── *)

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'

let is_ident_char c =
  is_ident_start c || (c >= '0' && c <= '9') || c = '-' || c = '.'

let normalize_to_json input =
  let stripped = strip_comments input in
  let no_trailing = remove_trailing_commas stripped in
  let len = String.length no_trailing in
  let buf = Buffer.create len in
  let i = ref 0 in
  let skip_ws () =
    while
      !i < len
      && (no_trailing.[!i] = ' '
         || no_trailing.[!i] = '\t'
         || no_trailing.[!i] = '\n'
         || no_trailing.[!i] = '\r')
    do
      incr i
    done
  in
  let rec process () =
    if !i >= len then ()
    else
      let c = no_trailing.[!i] in
      match c with
      | ' ' | '\t' | '\n' | '\r' ->
          Buffer.add_char buf c ;
          incr i ;
          process ()
      | '"' ->
          Buffer.add_char buf '"' ;
          incr i ;
          copy_string () ;
          process ()
      | '\''
        when !i + 2 < len
             && no_trailing.[!i + 1] = '\''
             && no_trailing.[!i + 2] = '\'' ->
          i := !i + 3 ;
          let content = Buffer.create 64 in
          while
            !i + 2 < len
            && not
                 (no_trailing.[!i] = '\''
                 && no_trailing.[!i + 1] = '\''
                 && no_trailing.[!i + 2] = '\'')
          do
            Buffer.add_char content no_trailing.[!i] ;
            incr i
          done ;
          if !i + 2 < len then i := !i + 3 ;
          Buffer.add_char buf '"' ;
          String.iter
            (fun ch ->
              match ch with
              | '\n' -> Buffer.add_string buf "\\n"
              | '\r' -> Buffer.add_string buf "\\r"
              | '\t' -> Buffer.add_string buf "\\t"
              | '"' -> Buffer.add_string buf "\\\""
              | '\\' -> Buffer.add_string buf "\\\\"
              | _ -> Buffer.add_char buf ch)
            (Buffer.contents content) ;
          Buffer.add_char buf '"' ;
          process ()
      | '{' | '[' | '}' | ']' | ',' | ':' ->
          Buffer.add_char buf c ;
          incr i ;
          process ()
      | _ when is_ident_start c ->
          let start = !i in
          while !i < len && is_ident_char no_trailing.[!i] do
            incr i
          done ;
          let word = String.sub no_trailing start (!i - start) in
          let saved_i = !i in
          skip_ws () ;
          if !i < len && no_trailing.[!i] = ':' then (
            Buffer.add_char buf '"' ;
            Buffer.add_string buf word ;
            Buffer.add_char buf '"')
          else (
            i := saved_i ;
            match word with
            | "true" | "false" | "null" -> Buffer.add_string buf word
            | _ ->
                Buffer.add_char buf '"' ;
                Buffer.add_string buf word ;
                Buffer.add_char buf '"') ;
          process ()
      | _ ->
          Buffer.add_char buf c ;
          incr i ;
          process ()
  and copy_string () =
    if !i >= len then ()
    else
      let c = no_trailing.[!i] in
      Buffer.add_char buf c ;
      incr i ;
      match c with
      | '\\' ->
          if !i < len then (
            Buffer.add_char buf no_trailing.[!i] ;
            incr i) ;
          copy_string ()
      | '"' -> ()
      | _ -> copy_string ()
  in
  process () ;
  Buffer.contents buf

(* ── Public API ──────────────────────────────────────────── *)

let parse input =
  let json_str = normalize_to_json input in
  try Ok (Yojson.Safe.from_string json_str)
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "JSON parse error: %s" msg)

let parse_file path =
  try
    let ic = open_in path in
    let content = In_channel.input_all ic in
    close_in ic ;
    match parse content with
    | Ok _ as ok -> ok
    | Error msg -> Error (Printf.sprintf "%s: %s" path msg)
  with Sys_error msg -> Error msg

module Internal_for_tests = struct
  let strip_comments = strip_comments

  let normalize_to_json = normalize_to_json
end
