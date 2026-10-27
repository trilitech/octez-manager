(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Test inventory contract.

    Usage: [check_test_inventory --manifest FILE --dune FILE [--dune FILE]...]

    Every [executable]/[executables] stanza in the given dune files must be
    classified in the manifest, every source module must belong to a stanza
    (unless the manifest marks it obsolete), and the manifest must agree with
    what [dune runtest] actually runs.

    Exit codes: 0 contract holds, 1 violations (listed on stdout), 2 setup
    error (unreadable file, parse error, malformed manifest, bad usage). *)

(* ============================================================ *)
(* Minimal s-expression reader (dune syntax subset) *)
(* ============================================================ *)

type sexp = Atom of string | List of sexp list

exception Parse_error of string

let parse_error fmt = Printf.ksprintf (fun m -> raise (Parse_error m)) fmt

let parse_string ~file text =
  let len = String.length text in
  let pos = ref 0 in
  let peek () = if !pos < len then Some text.[!pos] else None in
  let advance () = incr pos in
  let rec skip_blank () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
        advance () ;
        skip_blank ()
    | Some ';' ->
        while match peek () with Some '\n' | None -> false | _ -> true do
          advance ()
        done ;
        skip_blank ()
    | Some '#' when !pos + 1 < len && text.[!pos + 1] = '|' ->
        pos := !pos + 2 ;
        while
          !pos + 1 < len && not (text.[!pos] = '|' && text.[!pos + 1] = '#')
        do
          advance ()
        done ;
        if !pos + 1 >= len then
          parse_error "%s: unterminated block comment" file ;
        pos := !pos + 2 ;
        skip_blank ()
    | Some '#' when !pos + 1 < len && text.[!pos + 1] = ';' ->
        pos := !pos + 2 ;
        ignore (read_sexp ()) ;
        skip_blank ()
    | _ -> ()
  and read_string () =
    let buf = Buffer.create 16 in
    advance () ;
    let rec loop () =
      match peek () with
      | None -> parse_error "%s: unterminated string" file
      | Some '"' -> advance ()
      | Some '\\' when !pos + 1 < len ->
          Buffer.add_char buf text.[!pos + 1] ;
          pos := !pos + 2 ;
          loop ()
      | Some c ->
          Buffer.add_char buf c ;
          advance () ;
          loop ()
    in
    loop () ;
    Atom (Buffer.contents buf)
  and read_atom () =
    let start = !pos in
    while
      match peek () with
      | None | Some (' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"') -> false
      | _ -> true
    do
      advance ()
    done ;
    Atom (String.sub text start (!pos - start))
  and read_list () =
    advance () ;
    let rec loop acc =
      skip_blank () ;
      match peek () with
      | None -> parse_error "%s: unbalanced parenthesis" file
      | Some ')' ->
          advance () ;
          List (List.rev acc)
      | Some _ -> loop (read_sexp () :: acc)
    in
    loop []
  and read_sexp () =
    skip_blank () ;
    match peek () with
    | None -> parse_error "%s: unexpected end of input" file
    | Some '(' -> read_list ()
    | Some ')' -> parse_error "%s: unexpected ')' at offset %d" file !pos
    | Some '"' -> read_string ()
    | Some _ -> read_atom ()
  in
  let rec all acc =
    skip_blank () ;
    if !pos >= len then List.rev acc else all (read_sexp () :: acc)
  in
  all []

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error msg -> parse_error "%s" msg

(* ============================================================ *)
(* Dune stanza model *)
(* ============================================================ *)

type stanza = {kind : string; names : string list; modules : string list option}

let field name fields =
  List.find_map
    (function
      | List (Atom f :: values) when String.equal f name -> Some values
      | _ -> None)
    fields

let atoms values =
  List.filter_map (function Atom a -> Some a | List _ -> None) values

let rec all_atoms = function
  | Atom a -> [a]
  | List l -> List.concat_map all_atoms l

(** Executable names referenced as [%{exe:NAME.exe}] or [%{dep:NAME.exe}]. *)
let referenced_exes sexp =
  let re = Str.regexp "%{\\(exe\\|dep\\):\\([A-Za-z0-9_]+\\)\\.exe}" in
  all_atoms sexp
  |> List.concat_map (fun atom ->
      let rec scan from acc =
        match Str.search_forward re atom from with
        | i -> scan (i + 1) (Str.matched_group 2 atom :: acc)
        | exception Not_found -> acc
      in
      scan 0 [])

let is_runtest_rule fields =
  match field "alias" fields with Some [Atom "runtest"] -> true | _ -> false

type dune_file = {
  stanzas : stanza list;
  runtest_rule_exes : string list;
  src_dir : string;
}

let load_dune path =
  let sexps = parse_string ~file:path (read_file path) in
  let stanza kind fields =
    let names =
      match (field "name" fields, field "names" fields) with
      | Some n, _ | None, Some n -> atoms n
      | None, None -> []
    in
    let modules =
      Option.map
        (fun m -> List.map String.lowercase_ascii (atoms m))
        (field "modules" fields)
    in
    {kind; names; modules}
  in
  let stanzas, rule_exes =
    List.fold_left
      (fun (stanzas, exes) sexp ->
        match sexp with
        | List
            (Atom
               (("executable" | "executables" | "test" | "tests" | "library") as
                kind)
            :: fields) ->
            (stanza kind fields :: stanzas, exes)
        | List (Atom "rule" :: fields) when is_runtest_rule fields ->
            (stanzas, referenced_exes sexp @ exes)
        | _ -> (stanzas, exes))
      ([], [])
      sexps
  in
  {
    stanzas = List.rev stanzas;
    runtest_rule_exes = rule_exes;
    src_dir = Filename.dirname path;
  }

(* ============================================================ *)
(* Manifest *)
(* ============================================================ *)

type klass = Deterministic | Env_gated | Manual | Obsolete

type entry = {name : string; klass : klass}

let parse_entry file = function
  | List fields as sexp -> (
      let one key =
        match field key fields with Some [Atom v] -> Some v | _ -> None
      in
      let require key =
        match one key with
        | Some v when String.trim v <> "" -> ()
        | _ -> parse_error "%s: entry needs a non-empty (%s ...)" file key
      in
      match (one "name", one "class") with
      | Some name, Some "deterministic" -> {name; klass = Deterministic}
      | Some name, Some "env-gated" ->
          require "gate" ;
          {name; klass = Env_gated}
      | Some name, Some "manual" ->
          require "reason" ;
          {name; klass = Manual}
      | Some name, Some "obsolete" ->
          require "reason" ;
          {name; klass = Obsolete}
      | _ ->
          parse_error
            "%s: malformed entry %s"
            file
            (String.concat " " (all_atoms sexp)))
  | Atom a -> parse_error "%s: unexpected atom %s" file a

let load_manifest path =
  let entries =
    List.map (parse_entry path) (parse_string ~file:path (read_file path))
  in
  List.iter
    (fun e ->
      if
        List.length
          (List.filter (fun e' -> String.equal e'.name e.name) entries)
        > 1
      then parse_error "%s: duplicate entry %s" path e.name)
    entries ;
  entries

(* ============================================================ *)
(* Contract *)
(* ============================================================ *)

let class_name = function
  | Deterministic -> "deterministic"
  | Env_gated -> "env-gated"
  | Manual -> "manual"
  | Obsolete -> "obsolete"

let mem name names = List.exists (String.equal name) names

let source_modules dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".ml")
  |> List.map (fun f -> String.lowercase_ascii (Filename.chop_suffix f ".ml"))
  |> List.sort String.compare

(** Violations of the contract, one message each. *)
let violations ~manifest dunes =
  let stanzas = List.concat_map (fun d -> d.stanzas) dunes in
  let names_of kinds =
    List.concat_map (fun s -> if mem s.kind kinds then s.names else []) stanzas
  in
  let tests = names_of ["test"; "tests"] in
  let executables = names_of ["executable"; "executables"] in
  let rule_exes = List.concat_map (fun d -> d.runtest_rule_exes) dunes in
  let runs n = mem n tests || mem n rule_exes in
  let declared n = mem n tests || mem n executables in
  let entry n = List.find_opt (fun e -> String.equal e.name n) manifest in
  let unclassified =
    List.filter_map
      (fun n ->
        if Option.is_none (entry n) && not (mem n rule_exes) then
          Some (Printf.sprintf "unclassified executable stanza: %s" n)
        else None)
      executables
  in
  let per_entry =
    List.filter_map
      (fun e ->
        match e.klass with
        | Deterministic when not (declared e.name) ->
            Some (Printf.sprintf "dead entry, no stanza: %s" e.name)
        | Deterministic when not (runs e.name) ->
            Some
              (Printf.sprintf
                 "deterministic test not run by runtest: %s"
                 e.name)
        | (Env_gated | Manual) when not (declared e.name) ->
            Some (Printf.sprintf "dead entry, no stanza: %s" e.name)
        | (Env_gated | Manual | Obsolete) when runs e.name ->
            Some
              (Printf.sprintf
                 "%s target attached to runtest: %s"
                 (class_name e.klass)
                 e.name)
        | Obsolete when declared e.name ->
            Some (Printf.sprintf "obsolete entry still has a stanza: %s" e.name)
        | _ -> None)
      manifest
  in
  let orphans =
    List.concat_map
      (fun d ->
        if List.exists (fun s -> Option.is_none s.modules) d.stanzas then []
        else
          let owned =
            List.concat_map
              (fun s -> Option.value ~default:[] s.modules)
              d.stanzas
          in
          source_modules d.src_dir
          |> List.filter_map (fun m ->
              let obsolete =
                match entry m with
                | Some {klass = Obsolete; _} -> true
                | _ -> false
              in
              if mem m owned || obsolete then None
              else
                Some
                  (Printf.sprintf
                     "source not attached to any stanza: %s.ml"
                     (Filename.concat d.src_dir m))))
      dunes
  in
  unclassified @ per_entry @ orphans

(* ============================================================ *)
(* Entry point *)
(* ============================================================ *)

let usage = "check_test_inventory --manifest FILE --dune FILE [--dune FILE]..."

let () =
  let manifest = ref None and dunes = ref [] in
  let spec =
    [
      ("--manifest", Arg.String (fun f -> manifest := Some f), "FILE manifest");
      ("--dune", Arg.String (fun f -> dunes := f :: !dunes), "FILE dune file");
    ]
  in
  let bad_usage msg =
    prerr_endline (msg ^ "\n" ^ usage) ;
    exit 2
  in
  (try
     Arg.parse_argv Sys.argv spec (fun a -> bad_usage ("unexpected " ^ a)) usage
   with Arg.Bad msg | Arg.Help msg -> bad_usage msg) ;
  match (!manifest, List.rev !dunes) with
  | None, _ | _, [] ->
      bad_usage "--manifest and at least one --dune are required"
  | Some manifest_path, dune_paths -> (
      match (load_manifest manifest_path, List.map load_dune dune_paths) with
      | exception (Parse_error msg | Sys_error msg) ->
          prerr_endline ("setup error: " ^ msg) ;
          exit 2
      | manifest, dunes -> (
          match violations ~manifest dunes with
          | [] -> print_endline "test inventory: OK"
          | vs ->
              List.iter print_endline vs ;
              exit 1))
