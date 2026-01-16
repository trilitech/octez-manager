(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(* Shared helper for parsing CLI --help output. *)

type value_kind =
  | Addr_port
  | Port
  | Addr
  | File
  | Dir
  | Path
  | Number
  | Float
  | Text

type arg_kind = Toggle | Value of value_kind

type option_entry = {
  names : string list;
  arg : string option;
  doc : string;
  kind : arg_kind;
}

type command_entry = {name : string; doc : string}

let contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else loop (i + 1)
  in
  if nlen = 0 then true else loop 0

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

let primary_name names =
  match
    List.find_opt
      (fun n -> String.length n > 2 && String.sub n 0 2 = "--")
      names
  with
  | Some long -> long
  | None -> ( match names with h :: _ -> h | [] -> "")

let display_names names =
  let longs =
    List.filter (fun n -> String.length n > 2 && String.sub n 0 2 = "--") names
  in
  if longs = [] then names else longs

let classify_arg_kind ~names ~arg ~doc =
  match arg with
  | None -> Toggle
  | Some placeholder ->
      let text =
        String.concat
          " "
          [
            String.lowercase_ascii placeholder;
            String.lowercase_ascii doc;
            String.concat " " (List.map String.lowercase_ascii names);
          ]
      in
      let has s = contains ~needle:(String.lowercase_ascii s) text in
      if has "addr:port" || has "address:port" then Value Addr_port
      else if has "port" then Value Port
      else if has "addr" || has "address" || has "host" then Value Addr
      else if has "file" then Value File
      else if has "dir" || has "directory" then Value Dir
      else if has "path" then Value Path
      else if has "mode" then Value Text
      else if has "float" then Value Float
      else if has "int" || has "num" || has "number" then Value Number
      else Value Text

let split_spec_doc_default raw_line =
  let line = String.trim raw_line in
  let len = String.length line in
  let rec find_gap idx =
    if idx >= len - 1 then None
    else if line.[idx] = '\t' then Some idx
    else if line.[idx] = ' ' && line.[idx + 1] = ' ' then Some idx
    else find_gap (idx + 1)
  in
  match find_gap 0 with
  | Some idx ->
      let gap = if line.[idx] = '\t' then 1 else 2 in
      ( String.sub line 0 idx |> String.trim,
        String.sub line (idx + gap) (len - (idx + gap)) |> String.trim )
  | None -> (String.trim line, "")

(* Clap outputs often use ": " instead of aligned double spaces; fall back to
   splitting on the first occurrence of ": " when tabs/double-spaces are
   absent. *)
let split_spec_doc_baker raw_line =
  let line = String.trim raw_line in
  match split_spec_doc_default raw_line with
  | spec, doc when doc <> "" -> (spec, doc)
  | _ -> (
      match String.index_opt line ':' with
      | None -> (line, "")
      | Some idx ->
          if idx + 1 < String.length line && line.[idx + 1] = ' ' then
            let spec = String.sub line 0 idx |> String.trim in
            let doc =
              String.sub line (idx + 1) (String.length line - idx - 1)
              |> String.trim
            in
            (spec, doc)
          else (line, ""))

let clean_placeholder s =
  let trimmed = String.trim s in
  let len = String.length trimmed in
  if len = 0 then ""
  else
    let rec drop_trailing i =
      if i <= 0 then 0
      else
        match trimmed.[i - 1] with
        | ',' | ';' | ')' -> drop_trailing (i - 1)
        | _ -> i
    in
    let stop = drop_trailing len in
    String.sub trimmed 0 stop

let clean_name name =
  let trimmed = String.trim name in
  let len = String.length trimmed in
  let rec drop_trailing i =
    if i <= 0 then 0
    else
      match trimmed.[i - 1] with
      | ':' | ';' | ',' -> drop_trailing (i - 1)
      | _ -> i
  in
  String.sub trimmed 0 (drop_trailing len)

let parse_spec spec =
  let tokens =
    spec |> String.split_on_char ' '
    |> List.filter (fun s -> String.trim s <> "")
  in
  let names, arg_tokens =
    List.fold_left
      (fun (names, args) tok ->
        let tok = String.trim tok in
        if tok = "," then (names, args)
        else if String.length tok > 0 && tok.[0] = '-' then
          let name, inline_arg =
            match String.index_opt tok '=' with
            | None -> (tok, None)
            | Some idx ->
                let name = String.sub tok 0 idx in
                let placeholder =
                  String.sub tok (idx + 1) (String.length tok - idx - 1)
                in
                (name, Some placeholder)
          in
          ( clean_name name :: names,
            match inline_arg with None -> args | Some a -> a :: args )
        else (names, tok :: args))
      ([], [])
      tokens
  in
  let arg =
    match List.rev arg_tokens |> List.filter_map trim_nonempty with
    | [] -> None
    | x :: _ -> Some (clean_placeholder x)
  in
  (List.rev names, arg)

let split_bracket_arg token =
  match String.index_opt token '[' with
  | None -> None
  | Some idx ->
      let len = String.length token in
      if len <= idx + 1 || token.[len - 1] <> ']' then None
      else
        let name = String.sub token 0 idx in
        let inside = String.sub token (idx + 1) (len - idx - 2) in
        Some (name, inside)

let parse_spec_cmdliner spec =
  let tokens =
    spec |> String.split_on_char ' '
    |> List.filter (fun s -> String.trim s <> "")
  in
  let names, arg_tokens =
    List.fold_left
      (fun (names, args) tok ->
        let tok = String.trim tok in
        if tok = "," then (names, args)
        else if String.length tok > 0 && tok.[0] = '-' then
          let tok = clean_name tok in
          match split_bracket_arg tok with
          | Some (raw_name, inside) -> (
              let name = clean_name raw_name in
              let inside = String.trim inside in
              let arg =
                if inside = "" then None
                else if inside.[0] = '=' then
                  Some (String.sub inside 1 (String.length inside - 1))
                else Some inside
              in
              ( name :: names,
                match arg with None -> args | Some a -> a :: args ))
          | None -> (
              match String.index_opt tok '=' with
              | None -> (tok :: names, args)
              | Some idx ->
                  let name = String.sub tok 0 idx in
                  let placeholder =
                    String.sub tok (idx + 1) (String.length tok - idx - 1)
                  in
                  ( clean_name name :: names,
                    clean_placeholder placeholder :: args ))
        else (names, tok :: args))
      ([], [])
      tokens
  in
  let arg =
    match List.rev arg_tokens |> List.filter_map trim_nonempty with
    | [] -> None
    | x :: _ -> Some (clean_placeholder x)
  in
  (List.rev names, arg)

(* Check if a line looks like an option definition line from --help output. *)
let is_option_line_node line =
  let trimmed = String.trim line in
  if
    trimmed = ""
    || trimmed.[0] <> '-'
    || String.for_all (fun c -> c = '-') trimmed
  then false
  else
    let double_dash_count =
      let rec count acc i =
        if i >= String.length trimmed - 2 then acc
        else if
          trimmed.[i] = ' ' && trimmed.[i + 1] = '-' && trimmed.[i + 2] = '-'
        then count (acc + 1) (i + 3)
        else count acc (i + 1)
      in
      count 0 0
    in
    double_dash_count <= 1

let is_option_line_baker line =
  let trimmed = String.trim line in
  if
    trimmed = ""
    || trimmed.[0] <> '-'
    || String.for_all (fun c -> c = '-') trimmed
    || not (String.contains trimmed ' ')
  then false
  else
    let double_dash_count =
      let rec count acc i =
        if i >= String.length trimmed - 2 then acc
        else if
          trimmed.[i] = ' ' && trimmed.[i + 1] = '-' && trimmed.[i + 2] = '-'
        then count (acc + 1) (i + 3)
        else count acc (i + 1)
      in
      count 0 0
    in
    double_dash_count <= 1

let is_option_line_cmdliner line =
  let trimmed = String.trim line in
  trimmed <> ""
  && trimmed.[0] = '-'
  && not (String.for_all (fun c -> c = '-') trimmed)

let strip_ansi s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec skip_escape i =
    if i >= len then len
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' -> i + 1
      | _ -> skip_escape (i + 1)
  in
  let rec loop i =
    if i >= len then ()
    else if s.[i] = '\027' then
      let next =
        if i + 1 < len && s.[i + 1] = '[' then skip_escape (i + 2)
        else skip_escape (i + 1)
      in
      loop next
    else (
      Buffer.add_char buf s.[i] ;
      loop (i + 1))
  in
  loop 0 ;
  Buffer.contents buf

let parse_help_with ~is_option_line ~split_spec_doc ~parse_spec output =
  let lines = String.split_on_char '\n' output in
  let finalize current acc =
    match current with None -> acc | Some r -> r :: acc
  in
  let is_stop_section line =
    let trimmed = String.trim line in
    List.mem
      trimmed
      ["EXAMPLES"; "BUGS"; "SEE ALSO"; "EXIT STATUS"; "ENVIRONMENT"]
  in
  let rec loop acc current = function
    | [] -> List.rev (finalize current acc)
    | line :: _ when is_stop_section line -> List.rev (finalize current acc)
    | line :: rest when is_option_line line ->
        let acc = finalize current acc in
        let spec, doc = split_spec_doc line in
        let names, arg = parse_spec spec in
        let kind = classify_arg_kind ~names ~arg ~doc in
        let entry = {names; arg; doc; kind} in
        loop acc (Some entry) rest
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "" then loop acc current rest
        else
          let current =
            match current with
            | None -> None
            | Some opt ->
                let doc =
                  if opt.doc = "" then trimmed else opt.doc ^ " " ^ trimmed
                in
                let kind =
                  classify_arg_kind ~names:opt.names ~arg:opt.arg ~doc
                in
                Some {opt with doc; kind}
          in
          loop acc current rest
  in
  loop [] None lines

let parse_help_node =
  parse_help_with
    ~is_option_line:is_option_line_node
    ~split_spec_doc:split_spec_doc_default
    ~parse_spec

let parse_help_baker =
  parse_help_with
    ~is_option_line:is_option_line_baker
    ~split_spec_doc:split_spec_doc_baker
    ~parse_spec

let is_section_header line =
  let trimmed = String.trim line in
  let len = String.length trimmed in
  len > 0 && String.for_all (fun c -> (c >= 'A' && c <= 'Z') || c = ' ') trimmed

let extract_section_lines ~header lines =
  let rec find = function
    | [] -> []
    | line :: rest ->
        if String.trim line = header then collect [] rest else find rest
  and collect acc = function
    | [] -> List.rev acc
    | line :: _ when is_section_header line -> List.rev acc
    | line :: rest -> collect (line :: acc) rest
  in
  find lines

let parse_cmdliner_options output =
  let lines = String.split_on_char '\n' output in
  let section_lines =
    extract_section_lines ~header:"OPTIONS" lines
    @ extract_section_lines ~header:"COMMON OPTIONS" lines
  in
  let section_text = String.concat "\n" section_lines in
  parse_help_with
    ~is_option_line:is_option_line_cmdliner
    ~split_spec_doc:split_spec_doc_default
    ~parse_spec:parse_spec_cmdliner
    section_text

let looks_like_command_line line =
  let trimmed = String.trim line in
  let len = String.length trimmed in
  if len = 0 then false
  else
    let has_space = String.contains trimmed ' ' in
    let has_bracket = String.contains trimmed '[' in
    let has_command =
      contains ~needle:"COMMAND" trimmed || contains ~needle:"OPTION" trimmed
    in
    has_space && (has_bracket || has_command)

let parse_cmdliner_commands output =
  let lines = String.split_on_char '\n' output in
  let cmd_lines = extract_section_lines ~header:"COMMANDS" lines in
  let finalize current acc =
    match current with None -> acc | Some r -> r :: acc
  in
  let rec loop acc current = function
    | [] -> List.rev (finalize current acc)
    | line :: rest when String.trim line = "" -> loop acc current rest
    | line :: rest when looks_like_command_line line ->
        let acc = finalize current acc in
        let trimmed = String.trim line in
        let name =
          match String.index_opt trimmed ' ' with
          | None -> trimmed
          | Some idx -> String.sub trimmed 0 idx
        in
        loop acc (Some {name; doc = ""}) rest
    | line :: rest -> (
        match current with
        | None -> loop acc current rest
        | Some cmd ->
            let trimmed = String.trim line in
            let doc =
              if cmd.doc = "" then trimmed else cmd.doc ^ " " ^ trimmed
            in
            loop acc (Some {cmd with doc}) rest)
  in
  loop [] None cmd_lines

(* Baker/accuser global options parsing.
   The octez-baker --help output has a section:
   "Global options (must come before the command):"
   followed by option lines like:
   "  -f --password-filename <filename>: path to the password filename"
*)

let is_baker_global_section_header line =
  let trimmed = String.trim line in
  (* Only match lines that START with "Global options" - not usage lines that
     happen to contain "[global options]" *)
  String.length trimmed > 0
  &&
  let lower = String.lowercase_ascii trimmed in
  let prefix = "global options" in
  String.length lower >= String.length prefix
  && String.sub lower 0 (String.length prefix) = prefix

let extract_baker_global_section lines =
  let rec find = function
    | [] -> []
    | line :: rest ->
        if is_baker_global_section_header line then collect [] rest
        else find rest
  and collect acc = function
    | [] -> List.rev acc
    | line :: _ when String.trim line = "" && acc <> [] ->
        (* Stop at first blank line after collecting some options *)
        List.rev acc
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "" then collect acc rest
        else if
          (* Stop if we hit a new section (e.g. "Commands related to...") *)
          String.length trimmed > 0
          && trimmed.[0] <> '-'
          && contains ~needle:":" trimmed
          && not (contains ~needle:"--" trimmed)
        then List.rev acc
        else collect (line :: acc) rest
  in
  find lines

let parse_baker_global_options output =
  let lines = String.split_on_char '\n' output in
  let global_lines = extract_baker_global_section lines in
  let section_text = String.concat "\n" global_lines in
  parse_help_with
    ~is_option_line:is_option_line_baker
    ~split_spec_doc:split_spec_doc_baker
    ~parse_spec
    section_text

let extract_baker_global_option_names output =
  let options = parse_baker_global_options output in
  List.concat_map (fun opt -> opt.names) options

(** Classify an argument as global or command based on known global option names.
    Returns `Global if the arg starts with a known global option, `Command otherwise. *)
let classify_arg ~global_options arg =
  let starts_with_opt opt =
    let len = String.length opt in
    String.length arg >= len && String.sub arg 0 len = opt
  in
  if List.exists starts_with_opt global_options then `Global else `Command

(** Split a list of extra args into (global_args, command_args).
    Global args are those that match known global option names.
    Handles option values: if a global option is followed by a non-option value,
    that value is also considered global. *)
let split_extra_args ~global_options args =
  let rec loop globals commands = function
    | [] -> (List.rev globals, List.rev commands)
    | arg :: rest -> (
        match classify_arg ~global_options arg with
        | `Global -> (
            (* Check if next arg is the option's value (doesn't start with -) *)
            match rest with
            | value :: rest' when String.length value > 0 && value.[0] <> '-' ->
                loop (value :: arg :: globals) commands rest'
            | _ -> loop (arg :: globals) commands rest)
        | `Command -> loop globals (arg :: commands) rest)
  in
  loop [] [] args
