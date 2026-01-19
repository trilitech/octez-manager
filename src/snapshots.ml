(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let base_url = "https://snapshots.tzinit.org"

type entry = {
  network : string;
  slug : string;
  label : string;
  download_url : string option;
  history_mode : string option;
  metadata : (string * string) list;
}

let is_http_url s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  String.starts_with ~prefix:"http://" trimmed
  || String.starts_with ~prefix:"https://" trimmed

let slug_of_url u =
  let trimmed = String.trim u in
  let segments =
    trimmed |> String.split_on_char '/' |> List.rev |> List.filter (( <> ) "")
  in
  match segments with
  | seg :: _ ->
      let seg =
        if String.ends_with ~suffix:".json" seg then
          String.sub seg 0 (String.length seg - 5)
        else seg
      in
      let seg =
        match String.split_on_char '?' seg with hd :: _ -> hd | [] -> seg
      in
      Some (String.lowercase_ascii seg)
  | [] -> None

let slug_of_network n =
  let trimmed = String.trim n in
  if trimmed = "" then None
  else if is_http_url trimmed then slug_of_url trimmed
  else Some (String.lowercase_ascii trimmed)

let sanitize_kind_input k =
  let trimmed = String.trim k in
  if trimmed = "" then None
  else
    let lower = String.lowercase_ascii trimmed in
    let buf = Buffer.create (String.length lower) in
    String.iter
      (function
        | ':' | ' ' | '\t' | '\n' -> ()
        | ('a' .. 'z' | '0' .. '9' | '-' | '_') as c -> Buffer.add_char buf c
        | c -> Buffer.add_char buf c)
      lower ;
    let slug = Buffer.contents buf in
    if slug = "" then None else Some slug

let fetch_html_curl url =
  let cmd =
    Printf.sprintf
      "curl -fsL --max-time 10 --connect-timeout 5 -w '\n\
       %%{http_code}' %s 2>/dev/null"
      (Common.sh_quote url)
  in
  match Common.run_out ["/bin/sh"; "-c"; cmd] with
  | Ok out -> (
      let lines = String.split_on_char '\n' out |> List.rev in
      match lines with
      | code_str :: rev_body -> (
          match int_of_string_opt (String.trim code_str) with
          | Some 0 ->
              Error (`Msg (Printf.sprintf "curl failed (HTTP 0) for %s" url))
          | Some code ->
              let body = String.concat "\n" (List.rev rev_body) in
              Ok (code, body)
          | None -> Error (`Msg (Printf.sprintf "curl parse error for %s" url)))
      | [] -> Error (`Msg (Printf.sprintf "curl returned no status for %s" url))
      )
  | Error (`Msg msg) ->
      Error (`Msg (Printf.sprintf "curl fetch failed: %s" msg))

let fetch_html url = fetch_html_curl url

let fetch_html_ref = ref fetch_html

let find_sub ?(start = 0) ~sub s =
  let len = String.length s in
  let sub_len = String.length sub in
  let rec aux idx =
    if idx + sub_len > len then None
    else if String.sub s idx sub_len = sub then Some idx
    else aux (idx + 1)
  in
  aux start

let find_char ?(start = 0) s ch =
  let len = String.length s in
  let rec aux idx =
    if idx >= len then None else if s.[idx] = ch then Some idx else aux (idx + 1)
  in
  aux start

let replace_all ~sub ~by s =
  let sub_len = String.length sub in
  if sub_len = 0 then s
  else
    let buf = Buffer.create (String.length s) in
    let rec aux idx =
      match find_sub s ~sub ~start:idx with
      | None ->
          Buffer.add_substring buf s idx (String.length s - idx) ;
          Buffer.contents buf
      | Some pos ->
          Buffer.add_substring buf s idx (pos - idx) ;
          Buffer.add_string buf by ;
          aux (pos + sub_len)
    in
    aux 0

let decode_entities s =
  s |> replace_all ~sub:"&nbsp;" ~by:" " |> replace_all ~sub:"&amp;" ~by:"&"

let extract_between ~start_marker ~end_marker s =
  match find_sub ~sub:start_marker s with
  | None -> None
  | Some start_idx ->
      let content_start = start_idx + String.length start_marker in
      find_sub ~sub:end_marker s ~start:content_start
      |> Option.map (fun end_idx ->
          String.sub s content_start (end_idx - content_start))

let strip_tags s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec aux idx inside_tag =
    if idx >= len then Buffer.contents buf
    else
      match (s.[idx], inside_tag) with
      | '<', _ -> aux (idx + 1) true
      | '>', true -> aux (idx + 1) false
      | _, true -> aux (idx + 1) true
      | c, false ->
          Buffer.add_char buf c ;
          aux (idx + 1) false
  in
  aux 0 false

let parse_metadata_block block =
  let text = block |> strip_tags |> decode_entities in
  let lines = String.split_on_char '\n' text in
  lines
  |> List.filter_map (fun line ->
      let trimmed = String.trim line in
      if trimmed = "" then None
      else
        match String.index_opt trimmed ':' with
        | None -> None
        | Some idx ->
            let key = String.sub trimmed 0 idx |> String.trim in
            let value =
              String.sub trimmed (idx + 1) (String.length trimmed - idx - 1)
              |> String.trim
            in
            if key = "" then None else Some (key, value))

let metadata_from_html html =
  match extract_between ~start_marker:"<pre>" ~end_marker:"</pre>" html with
  | Some block -> Some (parse_metadata_block block)
  | None ->
      extract_between
        ~start_marker:"<pre><code>"
        ~end_marker:"</code></pre>"
        html
      |> Option.map parse_metadata_block

let make_entry ~network_slug ~slug ~label ~metadata =
  let download_url = List.assoc_opt "HTTPS" metadata in
  let history_mode = List.assoc_opt "History mode" metadata in
  {
    network = network_slug;
    slug;
    label = (if String.trim label = "" then slug else label);
    download_url;
    history_mode;
    metadata;
  }

let fetch_entry_with_impl ~fetch ~network_slug ~slug ~label =
  let url = Format.sprintf "%s/%s/%s.html" base_url network_slug slug in
  match fetch url with
  | Error _ as e -> e
  | Ok (code, body) ->
      if code = 404 then Ok None
      else if Cohttp.Code.is_success code then
        match metadata_from_html body with
        | Some metadata ->
            Ok (Some (make_entry ~network_slug ~slug ~label ~metadata))
        | None ->
            Error
              (`Msg
                 (Format.sprintf
                    "Snapshot '%s' page did not expose metadata (network %s)."
                    slug
                    network_slug))
      else
        Error
          (`Msg
             (Format.sprintf "HTTP %d while fetching snapshot page %s" code url))

let fetch_entry ~network_slug ~slug ~label :
    (entry option, Rresult.R.msg) result =
  fetch_entry_with_impl ~fetch:!fetch_html_ref ~network_slug ~slug ~label

let parse_anchors html ~network_slug =
  let prefix = Printf.sprintf "href=\"/%s/" network_slug in
  let rec loop idx acc =
    match find_sub html ~sub:prefix ~start:idx with
    | None -> List.rev acc
    | Some start_idx -> (
        let slug_start = start_idx + String.length prefix in
        match find_sub html ~sub:".html" ~start:slug_start with
        | None -> List.rev acc
        | Some slug_end -> (
            let slug = String.sub html slug_start (slug_end - slug_start) in
            match find_sub html ~sub:"\">" ~start:slug_end with
            | None -> loop (slug_end + 1) acc
            | Some label_arrow -> (
                let label_start = label_arrow + 2 in
                match find_char html '<' ~start:label_start with
                | None -> loop label_start acc
                | Some label_end ->
                    let label =
                      String.sub html label_start (label_end - label_start)
                      |> String.trim
                    in
                    let acc =
                      if label = "" || List.exists (fun (s, _) -> s = slug) acc
                      then acc
                      else (slug, label) :: acc
                    in
                    loop (label_end + 1) acc)))
  in
  loop 0 []

let default_candidates =
  ["rolling"; "full"; "full50"; "archive"]
  |> List.map (fun slug -> (slug, slug))

let list_with_impl ~fetch ~network_slug =
  let root = fetch (base_url ^ "/") in
  let base_candidates =
    match root with
    | Ok (_, html) ->
        let entries = parse_anchors html ~network_slug in
        if entries = [] then default_candidates else entries
    | Error _ -> default_candidates
  in
  let rec gather acc = function
    | [] -> Ok (List.rev acc)
    | (slug, label) :: rest -> (
        match fetch_entry_with_impl ~fetch ~network_slug ~slug ~label with
        | Ok (Some entry) -> gather (entry :: acc) rest
        | Ok None ->
            (* Just skip missing entries instead of failing *)
            gather acc rest
        | Error (`Msg e) ->
            (* Log error but continue with other candidates *)
            Common.append_debug_log
              (Printf.sprintf
                 "Failed to fetch snapshot metadata for %s/%s: %s"
                 network_slug
                 slug
                 e) ;
            gather acc rest)
  in
  match gather [] base_candidates with
  | Ok [] ->
      Error
        (`Msg
           (Printf.sprintf
              "No snapshots advertised for network '%s' on \
               snapshots.tzinit.org."
              network_slug))
  | other -> other

let with_fetch (fetch : string -> (int * string, Rresult.R.msg) result) f =
  let original = !fetch_html_ref in
  fetch_html_ref := fetch ;
  Fun.protect ~finally:(fun () -> fetch_html_ref := original) f

module For_tests = struct
  let with_fetch fetch f = with_fetch fetch f

  let fetch_html_with ~try_eio ~try_curl : (int * string, Rresult.R.msg) result
      =
    let try_eio_safe () =
      try try_eio ()
      with exn ->
        Error
          (`Msg (Printf.sprintf "eio fetch exn: %s" (Printexc.to_string exn)))
    in
    match try_eio_safe () with Ok _ as ok -> ok | Error _ -> try_curl ()
end

let list ~network_slug : (entry list, Rresult.R.msg) result =
  list_with_impl ~fetch:!fetch_html_ref ~network_slug
