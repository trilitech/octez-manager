(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Test inventory contract.

    Usage: [check_test_inventory --manifest FILE --dune FILE [--dune FILE]...]

    Every [test]/[tests]/[executable]/[executables] stanza in the given dune
    files must be classified in the manifest. A [deterministic] entry must be
    an unconditional [test]/[tests] stanza: only those make [dune runtest]
    run the suite and fail on its exit code; a rule merely referencing an
    executable proves nothing about its outcome. Every source module must
    belong to a stanza unless the manifest marks it [obsolete].

    Exit codes: 0 contract holds, 1 violations (listed on stdout), 2 setup
    error (unreadable file, parse error, malformed manifest, bad usage). *)

(* ============================================================ *)
(* Minimal s-expression reader (dune syntax subset) *)
(* ============================================================ *)

type sexp = Atom of string | List of sexp list

exception Parse_error of string

let parse_error fmt = Printf.ksprintf (fun m -> raise (Parse_error m)) fmt

type cursor = {file : string; text : string; mutable pos : int}

let peek c = if c.pos < String.length c.text then Some c.text.[c.pos] else None

let next_is c s =
  let n = String.length s in
  c.pos + n <= String.length c.text
  && String.equal (String.sub c.text c.pos n) s

let skip_while c pred =
  while match peek c with Some ch -> pred ch | None -> false do
    c.pos <- c.pos + 1
  done

let skip_block_comment c =
  c.pos <- c.pos + 2 ;
  while c.pos + 1 < String.length c.text && not (next_is c "|#") do
    c.pos <- c.pos + 1
  done ;
  if not (next_is c "|#") then
    parse_error "%s: unterminated block comment" c.file ;
  c.pos <- c.pos + 2

let read_string c =
  if next_is c "\"\\|" || next_is c "\"\\>" then
    parse_error "%s: dune block strings are not supported" c.file ;
  let buf = Buffer.create 16 in
  c.pos <- c.pos + 1 ;
  let rec loop () =
    match peek c with
    | None -> parse_error "%s: unterminated string" c.file
    | Some '"' -> c.pos <- c.pos + 1
    | Some '\\' when c.pos + 1 < String.length c.text ->
        Buffer.add_char buf c.text.[c.pos + 1] ;
        c.pos <- c.pos + 2 ;
        loop ()
    | Some ch ->
        Buffer.add_char buf ch ;
        c.pos <- c.pos + 1 ;
        loop ()
  in
  loop () ;
  Atom (Buffer.contents buf)

let read_atom c =
  let start = c.pos in
  skip_while c (function
    | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' -> false
    | _ -> true) ;
  Atom (String.sub c.text start (c.pos - start))

let rec skip_blank c =
  match peek c with
  | Some (' ' | '\t' | '\n' | '\r') ->
      c.pos <- c.pos + 1 ;
      skip_blank c
  | Some ';' ->
      skip_while c (fun ch -> ch <> '\n') ;
      skip_blank c
  | Some '#' when next_is c "#|" ->
      skip_block_comment c ;
      skip_blank c
  | Some '#' when next_is c "#;" ->
      c.pos <- c.pos + 2 ;
      ignore (read_sexp c) ;
      skip_blank c
  | _ -> ()

and read_list c =
  c.pos <- c.pos + 1 ;
  let rec loop acc =
    skip_blank c ;
    match peek c with
    | None -> parse_error "%s: unbalanced parenthesis" c.file
    | Some ')' ->
        c.pos <- c.pos + 1 ;
        List (List.rev acc)
    | Some _ -> loop (read_sexp c :: acc)
  in
  loop []

and read_sexp c =
  skip_blank c ;
  match peek c with
  | None -> parse_error "%s: unexpected end of input" c.file
  | Some '(' -> read_list c
  | Some ')' -> parse_error "%s: unexpected ')' at offset %d" c.file c.pos
  | Some '"' -> read_string c
  | Some _ -> read_atom c

let parse_string ~file text =
  let c = {file; text; pos = 0} in
  let rec all acc =
    skip_blank c ;
    if c.pos >= String.length text then List.rev acc
    else all (read_sexp c :: acc)
  in
  all []

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error msg -> parse_error "%s" msg

(* ============================================================ *)
(* Dune stanza model *)
(* ============================================================ *)

type stanza = {
  kind : string;
  names : string list;
  modules : string list option;
  conditional : bool;
      (** Has [enabled_if], [build_if] or an [action] override. *)
}

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

let stanza_of kind fields =
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
  let conditional =
    List.exists
      (fun f -> Option.is_some (field f fields))
      ["enabled_if"; "build_if"; "action"]
  in
  {kind; names; modules; conditional}

type dune_file = {
  stanzas : stanza list;
  rule_exes : string list;  (** Executables named [%{exe:NAME.exe}] in rules *)
  src_dir : string;
}

let exe_reference = Str.regexp "%{exe:\\([A-Za-z0-9_]+\\)\\.exe}"

let exe_references sexp =
  List.concat_map
    (fun atom ->
      let rec scan from acc =
        match Str.search_forward exe_reference atom from with
        | i -> scan (i + 1) (Str.matched_group 1 atom :: acc)
        | exception Not_found -> acc
      in
      scan 0 [])
    (all_atoms sexp)

let load_dune path =
  let sexps = parse_string ~file:path (read_file path) in
  let classify (stanzas, exes) = function
    | List
        (Atom
           (("executable" | "executables" | "test" | "tests" | "library") as
            kind)
        :: fields) ->
        (stanza_of kind fields :: stanzas, exes)
    | List (Atom "rule" :: _) as rule -> (stanzas, exe_references rule @ exes)
    | List (Atom (("subdir" | "include" | "dynamic_include") as kind) :: _) ->
        parse_error "%s: (%s ...) is not supported by the inventory" path kind
    | _ -> (stanzas, exes)
  in
  let stanzas, rule_exes = List.fold_left classify ([], []) sexps in
  {stanzas = List.rev stanzas; rule_exes; src_dir = Filename.dirname path}

(* ============================================================ *)
(* Manifest *)
(* ============================================================ *)

type klass = Deterministic | Env_gated | Manual | Tool | Obsolete

type entry = {name : string; klass : klass}

let class_name = function
  | Deterministic -> "deterministic"
  | Env_gated -> "env-gated"
  | Manual -> "manual"
  | Tool -> "tool"
  | Obsolete -> "obsolete"

let parse_entry file = function
  | List fields as sexp -> (
      let one key =
        match field key fields with Some [Atom v] -> Some v | _ -> None
      in
      let require key =
        match one key with
        | Some v when not (String.equal (String.trim v) "") -> ()
        | _ -> parse_error "%s: entry needs a non-empty (%s ...)" file key
      in
      let with_field key name klass =
        require key ;
        {name; klass}
      in
      match (one "name", one "class") with
      | Some name, Some "deterministic" -> {name; klass = Deterministic}
      | Some name, Some "env-gated" -> with_field "gate" name Env_gated
      | Some name, Some "manual" -> with_field "reason" name Manual
      | Some name, Some "tool" -> with_field "reason" name Tool
      | Some name, Some "obsolete" -> with_field "reason" name Obsolete
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
  let names = List.map (fun e -> e.name) entries in
  let sorted = List.sort_uniq String.compare names in
  if List.compare_lengths sorted names <> 0 then
    parse_error "%s: duplicate entries" path ;
  entries

(* ============================================================ *)
(* Contract *)
(* ============================================================ *)

let mem name names = List.exists (String.equal name) names

let is_test s = mem s.kind ["test"; "tests"]

let is_executable s = mem s.kind ["executable"; "executables"]

(** OCaml compilation-unit names only: excludes generated files such as the
    [*.pp.ml] that instrumented builds leave next to the sources. *)
let is_module_name m =
  String.length m > 0
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       m

let source_modules dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter_map (fun f -> Filename.chop_suffix_opt ~suffix:".ml" f)
  |> List.map String.lowercase_ascii
  |> List.filter is_module_name |> List.sort String.compare

let lookup manifest n = List.find_opt (fun e -> String.equal e.name n) manifest

let unclassified_violations ~manifest stanzas =
  stanzas
  |> List.filter (fun s -> is_test s || is_executable s)
  |> List.concat_map (fun s -> List.map (fun n -> (s, n)) s.names)
  |> List.filter_map (fun (s, n) ->
      if Option.is_some (lookup manifest n) then None
      else Some (Printf.sprintf "unclassified %s stanza: %s" s.kind n))

let stanza_violation ~rule_exes e s =
  let fail fmt = Printf.ksprintf Option.some fmt in
  match e.klass with
  | Obsolete -> fail "obsolete entry still has a stanza: %s" e.name
  | Deterministic when not (is_test s) ->
      fail "deterministic test not run by runtest: %s" e.name
  | Deterministic when s.conditional ->
      fail
        "deterministic test is conditional (enabled_if/build_if/action): %s"
        e.name
  | Deterministic -> None
  | (Env_gated | Manual | Tool) when is_test s ->
      fail
        "%s target registered as a runtest test: %s"
        (class_name e.klass)
        e.name
  | Tool when not (mem e.name rule_exes) ->
      fail "tool not referenced by any rule: %s" e.name
  | Env_gated | Manual | Tool -> None

(** Every stanza carrying the entry's name must satisfy its class. *)
let entry_violation ~rule_exes stanzas e =
  match (e.klass, List.filter (fun s -> mem e.name s.names) stanzas) with
  | Obsolete, [] -> None
  | _, [] -> Some (Printf.sprintf "dead entry, no stanza: %s" e.name)
  | _, named -> List.find_map (stanza_violation ~rule_exes e) named

let orphan_violations ~manifest dune =
  if List.exists (fun s -> Option.is_none s.modules) dune.stanzas then []
  else
    let owned =
      List.concat_map (fun s -> Option.value ~default:[] s.modules) dune.stanzas
    in
    source_modules dune.src_dir
    |> List.filter_map (fun m ->
        match lookup manifest m with
        | Some {klass = Obsolete; _} -> None
        | _ when mem m owned -> None
        | _ ->
            Some
              (Printf.sprintf
                 "source not attached to any stanza: %s.ml"
                 (Filename.concat dune.src_dir m)))

(** Violations of the contract, one message each. *)
let violations ~manifest dunes =
  let stanzas = List.concat_map (fun d -> d.stanzas) dunes in
  let rule_exes = List.concat_map (fun d -> d.rule_exes) dunes in
  unclassified_violations ~manifest stanzas
  @ List.filter_map (entry_violation ~rule_exes stanzas) manifest
  @ List.concat_map (orphan_violations ~manifest) dunes

(* ============================================================ *)
(* Entry point *)
(* ============================================================ *)

let usage = "check_test_inventory --manifest FILE --dune FILE [--dune FILE]..."

let bad_usage msg =
  prerr_endline (msg ^ "\n" ^ usage) ;
  exit 2

let parse_args () =
  let manifest = ref None and dunes = ref [] in
  let spec =
    [
      ("--manifest", Arg.String (fun f -> manifest := Some f), "FILE manifest");
      ("--dune", Arg.String (fun f -> dunes := f :: !dunes), "FILE dune file");
    ]
  in
  (try
     Arg.parse_argv Sys.argv spec (fun a -> bad_usage ("unexpected " ^ a)) usage
   with Arg.Bad msg | Arg.Help msg -> bad_usage msg) ;
  match (!manifest, List.rev !dunes) with
  | None, _ | _, [] ->
      bad_usage "--manifest and at least one --dune are required"
  | Some manifest, dunes -> (manifest, dunes)

let () =
  let manifest_path, dune_paths = parse_args () in
  match (load_manifest manifest_path, List.map load_dune dune_paths) with
  | exception (Parse_error msg | Sys_error msg) ->
      prerr_endline ("setup error: " ^ msg) ;
      exit 2
  | manifest, dunes -> (
      match violations ~manifest dunes with
      | [] -> print_endline "test inventory: OK"
      | vs ->
          List.iter print_endline vs ;
          exit 1)
