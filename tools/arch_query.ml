(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Architecture index query tool.

    Provides a CLI for querying [docs/architecture.db] with canned queries
    and fuzzy intent search. *)

let db_path =
  match Sys.getenv_opt "ARCH_DB_PATH" with
  | Some p -> p
  | None -> "docs/architecture.db"

(* ========================================================================== *)
(* Database helpers                                                           *)
(* ========================================================================== *)

let open_db () =
  if not (Sys.file_exists db_path) then (
    Printf.eprintf
      "Error: %s not found.\nRun: dune exec -- tools/arch_index.exe\n"
      db_path ;
    exit 1) ;
  let db = Sqlite3.db_open db_path in
  ignore (Sqlite3.exec db "PRAGMA foreign_keys = ON") ;
  db

let print_table ~headers rows =
  let col_widths =
    List.mapi
      (fun i h ->
        List.fold_left
          (fun acc row ->
            let cell = List.nth row i in
            max acc (String.length cell))
          (String.length h)
          rows)
      headers
  in
  (* Header *)
  List.iteri
    (fun i h ->
      let w = List.nth col_widths i in
      Printf.printf "%-*s" (w + 2) h)
    headers ;
  print_newline () ;
  List.iter (fun w -> Printf.printf "%s  " (String.make w '-')) col_widths ;
  print_newline () ;
  (* Rows *)
  List.iter
    (fun row ->
      List.iteri
        (fun i cell ->
          let w = List.nth col_widths i in
          Printf.printf "%-*s" (w + 2) cell)
        row ;
      print_newline ())
    rows

let query_rows db sql =
  let rows = ref [] in
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h -> rows := Array.to_list row :: !rows)
       sql) ;
  List.rev !rows

(* ========================================================================== *)
(* Fuzzy text search                                                          *)
(* ========================================================================== *)

let stop_words =
  [
    "the";
    "a";
    "an";
    "is";
    "are";
    "was";
    "were";
    "be";
    "been";
    "being";
    "have";
    "has";
    "had";
    "do";
    "does";
    "did";
    "will";
    "would";
    "could";
    "should";
    "may";
    "might";
    "shall";
    "can";
    "to";
    "of";
    "in";
    "for";
    "on";
    "with";
    "at";
    "by";
    "from";
    "as";
    "into";
    "through";
    "during";
    "before";
    "after";
    "and";
    "but";
    "or";
    "nor";
    "not";
    "so";
    "yet";
    "it";
    "its";
    "this";
    "that";
    "these";
    "those";
  ]

(** Normalize and tokenize text into a set of lowercase words,
    excluding stop words and very short tokens. *)
let tokenize text =
  let buf = Buffer.create (String.length text) in
  (* Replace non-alphanum with spaces *)
  String.iter
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
      then Buffer.add_char buf (Char.lowercase_ascii c)
      else Buffer.add_char buf ' ')
    text ;
  let words = String.split_on_char ' ' (Buffer.contents buf) in
  let words =
    List.filter
      (fun w -> String.length w > 1 && not (List.mem w stop_words))
      words
  in
  (* Deduplicate *)
  List.sort_uniq String.compare words

(** Jaccard similarity between two word sets: |intersection| / |union|. *)
let jaccard_similarity words1 words2 =
  let set1 = List.sort_uniq String.compare words1 in
  let set2 = List.sort_uniq String.compare words2 in
  let intersection =
    List.filter (fun w -> List.mem w set2) set1 |> List.length
  in
  let union = List.sort_uniq String.compare (set1 @ set2) |> List.length in
  if union = 0 then 0.0 else float_of_int intersection /. float_of_int union

(** Check if all query words appear as substrings in target words.
    Returns a bonus score based on substring coverage. *)
let string_contains ~needle haystack =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0) ;
    true
  with Not_found -> false

let substring_score query_words target_words =
  let matched =
    List.filter
      (fun qw ->
        List.exists (fun tw -> string_contains ~needle:qw tw) target_words)
      query_words
  in
  if List.length query_words = 0 then 0.0
  else
    float_of_int (List.length matched) /. float_of_int (List.length query_words)

(** Combined similarity: max of Jaccard and substring matching. *)
let text_similarity query_text target_text =
  let q = tokenize query_text in
  let t = tokenize target_text in
  let jaccard = jaccard_similarity q t in
  let substr = substring_score q t in
  max jaccard substr

(* ========================================================================== *)
(* Shared SQL fragments                                                       *)
(* ========================================================================== *)

(** Names excluded from duplicate detection.

    These are common interface names (e.g. PAGE_SIG callbacks, scheduler
    lifecycle) that legitimately appear in many modules with the same
    signature. *)
let duplicate_excluded_names =
  [
    "register";
    "init";
    "view";
    "update";
    "refresh";
    "move";
    "back";
    "noop";
    "service_select";
    "service_cycle";
    "keymap";
    "has_modal";
    "handled_keys";
    "handle_modal_key";
    "handle_key";
    "header";
    "name";
    "shutdown";
    "start";
    "clear";
    "stop";
    "tick";
    "get";
    "clear_cache";
    "started";
    "shutdown_requested";
  ]

(** SQL WHERE clause fragment that filters out trivial/interface duplicates. *)
let duplicate_where_clause =
  let quoted = List.map (Printf.sprintf "'%s'") duplicate_excluded_names in
  Printf.sprintf
    "f.is_alias = 0 AND f.name NOT LIKE 'let*' AND f.signature NOT IN ('unit', \
     'string', 'int', 'float', 'bool', 'Mutex.t') AND f.signature NOT LIKE \
     '''a%%' AND f.name NOT IN (%s)"
    (String.concat ", " quoted)

(* ========================================================================== *)
(* Commands                                                                   *)
(* ========================================================================== *)

let cmd_stats () =
  let db = open_db () in
  let get sql =
    let r = ref "0" in
    ignore (Sqlite3.exec_not_null db ~cb:(fun row _h -> r := row.(0)) sql) ;
    !r
  in
  Printf.printf "Architecture Index Statistics\n" ;
  Printf.printf "=============================\n" ;
  Printf.printf
    "Modules:             %5s\n"
    (get "SELECT COUNT(*) FROM modules") ;
  Printf.printf
    "  with .mli:         %5s\n"
    (get "SELECT COUNT(*) FROM modules WHERE has_mli = 1") ;
  Printf.printf
    "  large (>500 loc):  %5s\n"
    (get "SELECT COUNT(*) FROM modules WHERE lines > 500") ;
  Printf.printf
    "Functions:           %5s\n"
    (get "SELECT COUNT(*) FROM functions") ;
  Printf.printf
    "  exposed:           %5s\n"
    (get "SELECT COUNT(*) FROM functions WHERE exposed = 1") ;
  Printf.printf
    "  documented:        %5s\n"
    (get "SELECT COUNT(*) FROM functions WHERE intent IS NOT NULL") ;
  Printf.printf
    "  large (>50 loc):   %5s\n"
    (get "SELECT COUNT(*) FROM functions WHERE line_end - line_start + 1 > 50") ;
  Printf.printf "Types:               %5s\n" (get "SELECT COUNT(*) FROM types") ;
  Printf.printf
    "  record:            %5s\n"
    (get "SELECT COUNT(*) FROM types WHERE kind = 'record'") ;
  Printf.printf
    "  variant:           %5s\n"
    (get "SELECT COUNT(*) FROM types WHERE kind = 'variant'") ;
  Printf.printf
    "  documented:        %5s\n"
    (get "SELECT COUNT(*) FROM types WHERE intent IS NOT NULL") ;
  Printf.printf
    "Record fields:       %5s\n"
    (get "SELECT COUNT(*) FROM type_fields") ;
  Printf.printf
    "Variant constructors:%5s\n"
    (get "SELECT COUNT(*) FROM type_constructors") ;
  ignore (Sqlite3.db_close db)

let cmd_search ~threshold ~kind query =
  let db = open_db () in
  let results = ref [] in
  let sql, label =
    match kind with
    | `Functions ->
        ( "SELECT m.path, f.name, f.signature, COALESCE(f.intent, '') FROM \
           functions f JOIN modules m ON f.module_id = m.id",
          "function" )
    | `Types ->
        ( "SELECT m.path, t.name, t.kind || COALESCE(' = ' || t.manifest, ''), \
           COALESCE(t.intent, '') FROM types t JOIN modules m ON t.module_id = \
           m.id",
          "type" )
    | `All ->
        ( "SELECT m.path, f.name, f.signature, COALESCE(f.intent, '') FROM \
           functions f JOIN modules m ON f.module_id = m.id UNION ALL SELECT \
           m.path, t.name, t.kind || COALESCE(' = ' || t.manifest, ''), \
           COALESCE(t.intent, '') FROM types t JOIN modules m ON t.module_id = \
           m.id",
          "function/type" )
  in
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h ->
         let path = row.(0) in
         let name = row.(1) in
         let sig_or_kind = row.(2) in
         let intent = row.(3) in
         (* Score against intent, name, and signature *)
         let intent_score = text_similarity query intent in
         let name_score = text_similarity query name in
         let sig_score = text_similarity query sig_or_kind in
         let combined =
           (* Weight intent highest, then name, then signature *)
           max (max intent_score (name_score *. 0.8)) (sig_score *. 0.5)
         in
         if combined >= threshold then
           results := (combined, path, name, sig_or_kind, intent) :: !results)
       sql) ;
  let sorted =
    List.sort (fun (s1, _, _, _, _) (s2, _, _, _, _) -> compare s2 s1) !results
  in
  let top =
    if List.length sorted > 30 then List.filteri (fun i _ -> i < 30) sorted
    else sorted
  in
  if top = [] then
    Printf.printf
      "No %ss found matching \"%s\" (threshold: %.0f%%)\n"
      label
      query
      (threshold *. 100.0)
  else (
    Printf.printf
      "Found %d %ss matching \"%s\" (showing top %d, threshold: %.0f%%):\n\n"
      (List.length sorted)
      label
      query
      (List.length top)
      (threshold *. 100.0) ;
    List.iter
      (fun (score, path, name, _sig, intent) ->
        Printf.printf "  %3.0f%%  %s:%s\n" (score *. 100.0) path name ;
        if intent <> "" then
          let preview =
            let line =
              match String.index_opt intent '\n' with
              | Some i -> String.sub intent 0 i
              | None -> intent
            in
            if String.length line > 80 then String.sub line 0 77 ^ "..."
            else line
          in
          Printf.printf "        %s\n" preview)
      top) ;
  ignore (Sqlite3.db_close db)

let cmd_search_types ~field_names ~field_types =
  let db = open_db () in
  let conditions = ref [] in
  List.iter
    (fun name ->
      conditions :=
        Printf.sprintf
          "t.id IN (SELECT type_id FROM type_fields WHERE field_name LIKE \
           '%%%s%%')"
          name
        :: !conditions)
    field_names ;
  List.iter
    (fun typ ->
      conditions :=
        Printf.sprintf
          "t.id IN (SELECT type_id FROM type_fields WHERE field_type LIKE \
           '%%%s%%')"
          typ
        :: !conditions)
    field_types ;
  if !conditions = [] then (
    Printf.eprintf "Error: provide at least --field or --field-type\n" ;
    exit 1) ;
  let where = String.concat " AND " !conditions in
  let sql =
    Printf.sprintf
      "SELECT m.path, t.name, GROUP_CONCAT(tf.field_name || ': ' || \
       tf.field_type, ', ') FROM types t JOIN modules m ON t.module_id = m.id \
       JOIN type_fields tf ON t.id = tf.type_id WHERE %s GROUP BY t.id ORDER \
       BY m.path"
      where
  in
  let rows = query_rows db sql in
  if rows = [] then Printf.printf "No types found matching the criteria.\n"
  else (
    Printf.printf "Found %d matching types:\n\n" (List.length rows) ;
    List.iter
      (fun row ->
        match row with
        | [path; name; fields] ->
            Printf.printf "  %s.%s\n" path name ;
            let field_list = String.split_on_char ',' fields in
            List.iter
              (fun f -> Printf.printf "    %s\n" (String.trim f))
              field_list ;
            print_newline ()
        | _ -> ())
      rows) ;
  ignore (Sqlite3.db_close db)

let cmd_duplicates () =
  let db = open_db () in
  let sql =
    Printf.sprintf
      "SELECT f.name, f.signature, COUNT(DISTINCT m.id) as cnt, \
       GROUP_CONCAT(m.path, ', ') FROM functions f JOIN modules m ON \
       f.module_id = m.id WHERE %s GROUP BY f.name, f.signature HAVING cnt > 1 \
       ORDER BY cnt DESC"
      duplicate_where_clause
  in
  let rows = query_rows db sql in
  Printf.printf "Found %d groups of duplicate functions:\n\n" (List.length rows) ;
  List.iter
    (fun row ->
      match row with
      | [name; signature; count; modules] ->
          Printf.printf "  %s  (%sx)\n" name count ;
          Printf.printf
            "    sig: %s\n"
            (if String.length signature > 70 then
               String.sub signature 0 67 ^ "..."
             else signature) ;
          Printf.printf "    in:  %s\n\n" modules
      | _ -> ())
    rows ;
  ignore (Sqlite3.db_close db)

let cmd_mutables () =
  let db = open_db () in
  (* Summary by kind *)
  Printf.printf "Mutable pattern usage summary:\n\n" ;
  let summary_sql =
    "SELECT kind, COUNT(*) as cnt FROM mutable_usages GROUP BY kind ORDER BY \
     cnt DESC"
  in
  let summary = query_rows db summary_sql in
  List.iter
    (fun row ->
      match row with
      | [kind; cnt] -> Printf.printf "  %-15s %s occurrences\n" kind cnt
      | _ -> ())
    summary ;
  Printf.printf "\n" ;
  (* Mutable record fields *)
  let mf_sql =
    "SELECT m.path, t.name, tf.field_name FROM type_fields tf JOIN types t ON \
     tf.type_id = t.id JOIN modules m ON t.module_id = m.id WHERE \
     tf.is_mutable = 1 ORDER BY m.path, t.name"
  in
  let mf_rows = query_rows db mf_sql in
  Printf.printf "Mutable record fields (%d):\n\n" (List.length mf_rows) ;
  List.iter
    (fun row ->
      match row with
      | [path; typ; field] -> Printf.printf "  %s: %s.%s\n" path typ field
      | _ -> ())
    mf_rows ;
  Printf.printf "\n" ;
  (* Functions with most mutable usages *)
  let fn_sql =
    "SELECT m.path || ':' || f.name, mu.kind, COUNT(*) as cnt FROM \
     mutable_usages mu JOIN functions f ON mu.function_id = f.id JOIN modules \
     m ON f.module_id = m.id GROUP BY f.id, mu.kind HAVING cnt >= 3 ORDER BY \
     cnt DESC LIMIT 30"
  in
  let fn_rows = query_rows db fn_sql in
  Printf.printf "Functions with 3+ mutable pattern usages (top 30):\n\n" ;
  print_table ~headers:["Function"; "Kind"; "Count"] fn_rows ;
  ignore (Sqlite3.db_close db)

let cmd_large_files ~min_lines =
  let db = open_db () in
  let sql =
    Printf.sprintf
      "SELECT m.path, m.lines, m.has_mli, (SELECT COUNT(*) FROM functions f \
       WHERE f.module_id = m.id) as fns FROM modules m WHERE m.lines > %d \
       ORDER BY m.lines DESC"
      min_lines
  in
  let rows = query_rows db sql in
  print_table ~headers:["Path"; "Lines"; "Has .mli"; "Functions"] rows ;
  ignore (Sqlite3.db_close db)

let cmd_large_functions ~min_lines =
  let db = open_db () in
  let sql =
    Printf.sprintf
      "SELECT m.path || ':' || f.name, f.line_end - f.line_start + 1, \
       COALESCE(SUBSTR(f.signature, 1, 60), '') FROM functions f JOIN modules \
       m ON f.module_id = m.id WHERE f.name NOT LIKE 'let*' AND f.line_end - \
       f.line_start + 1 > %d ORDER BY f.line_end - f.line_start + 1 DESC"
      min_lines
  in
  let rows = query_rows db sql in
  print_table ~headers:["Function"; "Lines"; "Signature"] rows ;
  ignore (Sqlite3.db_close db)

let cmd_missing_docs () =
  let db = open_db () in
  Printf.printf "Exposed functions without documentation:\n\n" ;
  let sql =
    "SELECT m.path, COUNT(*) as cnt FROM functions f JOIN modules m ON \
     f.module_id = m.id WHERE f.exposed = 1 AND f.intent IS NULL GROUP BY m.id \
     ORDER BY cnt DESC"
  in
  let rows = query_rows db sql in
  print_table ~headers:["Module"; "Missing docs"] rows ;
  Printf.printf
    "\nTotal: %d exposed functions without docs\n"
    (List.fold_left
       (fun acc row ->
         match row with _ :: n :: _ -> acc + int_of_string n | _ -> acc)
       0
       rows) ;
  ignore (Sqlite3.db_close db)

let cmd_missing_mli () =
  let db = open_db () in
  let sql =
    "SELECT m.path, m.lines, (SELECT COUNT(*) FROM functions f WHERE \
     f.module_id = m.id) as fns FROM modules m WHERE m.has_mli = 0 ORDER BY \
     m.lines DESC"
  in
  let rows = query_rows db sql in
  print_table ~headers:["Module"; "Lines"; "Functions"] rows ;
  ignore (Sqlite3.db_close db)

let cmd_god_modules ~min_fns =
  let db = open_db () in
  let sql =
    Printf.sprintf
      "SELECT m.path, COUNT(*) as fns, m.lines FROM functions f JOIN modules m \
       ON f.module_id = m.id GROUP BY m.id HAVING fns > %d ORDER BY fns DESC"
      min_fns
  in
  let rows = query_rows db sql in
  print_table ~headers:["Module"; "Functions"; "Lines"] rows ;
  ignore (Sqlite3.db_close db)

let cmd_unsafe_strings () =
  let db = open_db () in
  Printf.printf "String-typed record fields that may need newtypes:\n\n" ;
  let sql =
    "SELECT tf.field_name, COUNT(*) as cnt, GROUP_CONCAT(DISTINCT m.path || \
     '.' || t.name) FROM type_fields tf JOIN types t ON tf.type_id = t.id JOIN \
     modules m ON t.module_id = m.id WHERE tf.field_type = 'string' GROUP BY \
     tf.field_name HAVING cnt >= 3 ORDER BY cnt DESC"
  in
  let rows = query_rows db sql in
  print_table ~headers:["Field name"; "Occurrences"; "Types"] rows ;
  ignore (Sqlite3.db_close db)

let cmd_sql query =
  let db = open_db () in
  let first_row = ref true in
  let rc =
    Sqlite3.exec
      db
      ~cb:(fun row headers ->
        if !first_row then (
          Printf.printf "%s\n" (String.concat " | " (Array.to_list headers)) ;
          Array.iter
            (fun h -> Printf.printf "%s-+-" (String.make (String.length h) '-'))
            headers ;
          print_newline () ;
          first_row := false) ;
        let cells =
          Array.map
            (fun cell -> match cell with Some s -> s | None -> "NULL")
            row
        in
        Printf.printf "%s\n" (String.concat " | " (Array.to_list cells)))
      query
  in
  (match rc with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Printf.eprintf
        "SQL error (%s): %s\n"
        (Sqlite3.Rc.to_string rc)
        (Sqlite3.errmsg db)) ;
  ignore (Sqlite3.db_close db)

let cmd_refresh () =
  let code = Sys.command "opam exec -- dune exec -- tools/arch_index.exe" in
  exit code

(* ========================================================================== *)
(* Metrics: machine-readable JSON output for CI                               *)
(* ========================================================================== *)

(** Metrics that get WORSE when they increase. *)
let worse_when_higher =
  [
    "duplicate_groups";
    "large_files";
    "large_functions";
    "missing_docs";
    "missing_mli";
    "god_modules";
    "unsafe_string_fields";
    "mutable_fields";
    "functions_with_mutables";
  ]

(** Metrics that get WORSE when they decrease. *)
let worse_when_lower = ["doc_coverage_pct"]

let cmd_metrics output_file =
  let db = open_db () in
  let get sql =
    let r = ref "0" in
    ignore (Sqlite3.exec_not_null db ~cb:(fun row _h -> r := row.(0)) sql) ;
    !r
  in
  let geti sql = int_of_string (get sql) in
  let total_fns = geti "SELECT COUNT(*) FROM functions" in
  let documented_fns =
    geti "SELECT COUNT(*) FROM functions WHERE intent IS NOT NULL"
  in
  let doc_pct =
    if total_fns > 0 then
      let raw =
        100.0 *. float_of_int documented_fns /. float_of_int total_fns
      in
      Float.round (raw *. 10.0) /. 10.0
    else 0.0
  in
  let metrics =
    [
      ("modules", string_of_int (geti "SELECT COUNT(*) FROM modules"));
      ("total_functions", string_of_int total_fns);
      ( "exposed_functions",
        string_of_int (geti "SELECT COUNT(*) FROM functions WHERE exposed = 1")
      );
      ("documented_functions", string_of_int documented_fns);
      ("doc_coverage_pct", Printf.sprintf "%.1f" doc_pct);
      ("total_types", string_of_int (geti "SELECT COUNT(*) FROM types"));
      ("record_fields", string_of_int (geti "SELECT COUNT(*) FROM type_fields"));
      ( "variant_constructors",
        string_of_int (geti "SELECT COUNT(*) FROM type_constructors") );
      ( "duplicate_groups",
        string_of_int
          (geti
             (Printf.sprintf
                "SELECT COUNT(*) FROM (SELECT f.name, f.signature FROM \
                 functions f JOIN modules m ON f.module_id = m.id WHERE %s \
                 GROUP BY f.name, f.signature HAVING COUNT(DISTINCT m.id) > 1)"
                duplicate_where_clause)) );
      ( "large_files",
        string_of_int (geti "SELECT COUNT(*) FROM modules WHERE lines > 500") );
      ( "large_functions",
        string_of_int
          (geti
             "SELECT COUNT(*) FROM functions WHERE line_end - line_start + 1 > \
              50") );
      ( "missing_docs",
        string_of_int
          (geti
             "SELECT COUNT(*) FROM functions WHERE exposed = 1 AND intent IS \
              NULL") );
      ( "missing_mli",
        string_of_int (geti "SELECT COUNT(*) FROM modules WHERE has_mli = 0") );
      ( "god_modules",
        string_of_int
          (geti
             "SELECT COUNT(*) FROM (SELECT module_id FROM functions GROUP BY \
              module_id HAVING COUNT(*) > 30)") );
      ( "unsafe_string_fields",
        string_of_int
          (geti
             "SELECT COUNT(*) FROM (SELECT field_name FROM type_fields WHERE \
              field_type = 'string' GROUP BY field_name HAVING COUNT(*) >= 3)")
      );
      ( "mutable_fields",
        string_of_int
          (geti "SELECT COUNT(*) FROM type_fields WHERE is_mutable = 1") );
      ( "functions_with_mutables",
        string_of_int
          (geti "SELECT COUNT(DISTINCT function_id) FROM mutable_usages") );
      ( "atomic_usages",
        string_of_int
          (geti
             "SELECT COUNT(*) FROM mutable_usages WHERE kind LIKE 'atomic_%'")
      );
    ]
  in
  ignore (Sqlite3.db_close db) ;
  (* Output JSON *)
  let json =
    Printf.sprintf
      "{\n%s\n}"
      (String.concat
         ",\n"
         (List.map
            (fun (k, v) ->
              (* Detect if value is numeric *)
              let json_val =
                try
                  ignore (float_of_string v) ;
                  v
                with _ -> Printf.sprintf "\"%s\"" v
              in
              Printf.sprintf "  \"%s\": %s" k json_val)
            metrics))
  in
  match output_file with
  | Some path ->
      let oc = open_out path in
      output_string oc json ;
      output_char oc '\n' ;
      close_out oc ;
      Printf.printf "Metrics written to %s\n" path
  | None -> print_endline json

(* -------------------------------------------------------------------------- *)
(* Compare two metrics JSON files                                             *)
(* -------------------------------------------------------------------------- *)

(** Simple JSON parser for flat { "key": number } objects. *)
let parse_metrics_json path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic ;
  let tbl = Hashtbl.create 32 in
  (* Match "key": value patterns *)
  let re = Str.regexp "\"\\([^\"]+\\)\": *\\([0-9.]+\\)" in
  let pos = ref 0 in
  (try
     while true do
       let _ = Str.search_forward re s !pos in
       let key = Str.matched_group 1 s in
       let value = float_of_string (Str.matched_group 2 s) in
       Hashtbl.replace tbl key value ;
       pos := Str.match_end ()
     done
   with Not_found -> ()) ;
  tbl

type comparison_result = {
  regressions : (string * float * float) list;
  improvements : (string * float * float) list;
  unchanged : (string * float) list;
}

let load_accept_file () =
  let path = ".metrics-accept" in
  if Sys.file_exists path then (
    let ic = open_in path in
    let lines = ref [] in
    (try
       while true do
         let line = String.trim (input_line ic) in
         (* Skip empty lines and comments *)
         if line <> "" && not (String.length line > 0 && line.[0] = '#') then
           (* Take metric name before any whitespace/comment *)
           let metric =
             match String.index_opt line ' ' with
             | Some i -> String.sub line 0 i
             | None -> line
           in
           lines := metric :: !lines
       done
     with End_of_file -> ()) ;
    close_in ic ;
    !lines)
  else []

(* -------------------------------------------------------------------------- *)
(* Per-item detail queries for regression diagnostics                         *)
(* -------------------------------------------------------------------------- *)

(** Return per-item detail lines for a regressing metric.

    Each function queries the current architecture DB and returns a list of
    short human-readable strings describing individual items that contribute
    to the metric count. The [max] parameter limits output to the N largest
    or most relevant items. *)

let detail_large_functions db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path || ':' || f.name, f.line_end - f.line_start + 1 as lc \
       FROM functions f JOIN modules m ON f.module_id = m.id WHERE f.name NOT \
       LIKE 'let*' AND lc > 50 ORDER BY lc DESC LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [loc; lines] -> Printf.sprintf "%s (%s lines)" loc lines
      | _ -> "?")

let detail_large_files db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path, m.lines FROM modules m WHERE m.lines > 500 ORDER BY \
       m.lines DESC LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [path; lines] -> Printf.sprintf "%s (%s lines)" path lines
      | _ -> "?")

let detail_duplicate_groups db ~max =
  let sql =
    Printf.sprintf
      "SELECT f.name, f.signature, COUNT(DISTINCT m.id) as cnt, \
       GROUP_CONCAT(m.path, ', ') FROM functions f JOIN modules m ON \
       f.module_id = m.id WHERE %s GROUP BY f.name, f.signature HAVING cnt > 1 \
       ORDER BY cnt DESC LIMIT %d"
      duplicate_where_clause
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [name; _sig; cnt; modules] ->
          Printf.sprintf "%s (%sx) in: %s" name cnt modules
      | _ -> "?")

let detail_missing_docs db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path || ':' || f.name FROM functions f JOIN modules m ON \
       f.module_id = m.id WHERE f.exposed = 1 AND f.intent IS NULL ORDER BY \
       m.path, f.name LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row -> match row with [loc] -> loc | _ -> "?")

let detail_missing_mli db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path, m.lines FROM modules m WHERE m.has_mli = 0 ORDER BY \
       m.lines DESC LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [path; lines] -> Printf.sprintf "%s (%s lines)" path lines
      | _ -> "?")

let detail_god_modules db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path, COUNT(*) as fns FROM functions f JOIN modules m ON \
       f.module_id = m.id GROUP BY m.id HAVING fns > 30 ORDER BY fns DESC \
       LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [path; fns] -> Printf.sprintf "%s (%s functions)" path fns
      | _ -> "?")

let detail_unsafe_string_fields db ~max =
  let sql =
    Printf.sprintf
      "SELECT tf.field_name, COUNT(*) as cnt FROM type_fields tf WHERE \
       tf.field_type = 'string' GROUP BY tf.field_name HAVING cnt >= 3 ORDER \
       BY cnt DESC LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [field; cnt] -> Printf.sprintf "%s (%sx)" field cnt
      | _ -> "?")

let detail_mutable_fields db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path || ':' || t.name || '.' || tf.field_name FROM type_fields \
       tf JOIN types t ON tf.type_id = t.id JOIN modules m ON t.module_id = \
       m.id WHERE tf.is_mutable = 1 ORDER BY m.path, t.name LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row -> match row with [loc] -> loc | _ -> "?")

let detail_functions_with_mutables db ~max =
  let sql =
    Printf.sprintf
      "SELECT m.path || ':' || f.name, mu.kind, COUNT(*) as cnt FROM \
       mutable_usages mu JOIN functions f ON mu.function_id = f.id JOIN \
       modules m ON f.module_id = m.id GROUP BY f.id, mu.kind ORDER BY cnt \
       DESC LIMIT %d"
      max
  in
  query_rows db sql
  |> List.map (fun row ->
      match row with
      | [loc; kind; cnt] -> Printf.sprintf "%s (%s: %sx)" loc kind cnt
      | _ -> "?")

(** Look up the detail function for a given metric name.

    Returns [None] for metrics that are not tracked per-item (e.g.
    [doc_coverage_pct] which is a derived percentage). *)
let detail_for_metric metric db ~max =
  match metric with
  | "large_functions" -> Some (detail_large_functions db ~max)
  | "large_files" -> Some (detail_large_files db ~max)
  | "duplicate_groups" -> Some (detail_duplicate_groups db ~max)
  | "missing_docs" -> Some (detail_missing_docs db ~max)
  | "missing_mli" -> Some (detail_missing_mli db ~max)
  | "god_modules" -> Some (detail_god_modules db ~max)
  | "unsafe_string_fields" -> Some (detail_unsafe_string_fields db ~max)
  | "mutable_fields" -> Some (detail_mutable_fields db ~max)
  | "functions_with_mutables" -> Some (detail_functions_with_mutables db ~max)
  | _ -> None

let try_open_db () =
  if Sys.file_exists db_path then (
    let db = Sqlite3.db_open db_path in
    ignore (Sqlite3.exec db "PRAGMA foreign_keys = ON") ;
    Some db)
  else None

let cmd_compare baseline_path current_path =
  let accepted = load_accept_file () in
  let baseline = parse_metrics_json baseline_path in
  let current = parse_metrics_json current_path in
  let regressions = ref [] in
  let improvements = ref [] in
  let unchanged = ref [] in
  Hashtbl.iter
    (fun key cur_val ->
      match Hashtbl.find_opt baseline key with
      | None -> () (* New metric, skip *)
      | Some base_val ->
          if cur_val = base_val then unchanged := (key, cur_val) :: !unchanged
          else
            let is_regression =
              if List.mem key worse_when_higher then cur_val > base_val
              else if List.mem key worse_when_lower then cur_val < base_val
              else false
            in
            let is_improvement =
              if List.mem key worse_when_higher then cur_val < base_val
              else if List.mem key worse_when_lower then cur_val > base_val
              else false
            in
            if is_regression then
              regressions := (key, base_val, cur_val) :: !regressions
            else if is_improvement then
              improvements := (key, base_val, cur_val) :: !improvements
            else unchanged := (key, cur_val) :: !unchanged)
    current ;
  let result =
    {
      regressions = List.sort compare !regressions;
      improvements = List.sort compare !improvements;
      unchanged = List.sort compare !unchanged;
    }
  in
  (* Separate accepted regressions from blocking ones *)
  let blocking, accepted_regressions =
    List.partition
      (fun (key, _, _) -> not (List.mem key accepted))
      result.regressions
  in
  (* Open architecture DB for per-item detail (optional) *)
  let db_opt = try_open_db () in
  let print_metric_detail key =
    match db_opt with
    | None -> ()
    | Some db -> (
        match detail_for_metric key db ~max:10 with
        | None | Some [] -> ()
        | Some items ->
            List.iter (fun item -> Printf.printf "    - %s\n" item) items)
  in
  let print_metric_list (key, base_val, cur_val) =
    let arrow = if cur_val > base_val then "+" else "" in
    Printf.printf
      "  %s: %.0f -> %.0f (%s%.0f)\n"
      key
      base_val
      cur_val
      arrow
      (cur_val -. base_val) ;
    print_metric_detail key
  in
  (* Print report *)
  if blocking <> [] then (
    Printf.printf "REGRESSIONS (CI will fail):\n" ;
    List.iter print_metric_list blocking ;
    print_newline ()) ;
  if result.improvements <> [] then (
    Printf.printf "Improvements:\n" ;
    List.iter print_metric_list result.improvements ;
    print_newline ()) ;
  if accepted_regressions <> [] then (
    Printf.printf "Accepted regressions (via .metrics-accept):\n" ;
    List.iter print_metric_list accepted_regressions ;
    print_newline ()) ;
  (* Close the DB if opened *)
  Option.iter (fun db -> ignore (Sqlite3.db_close db)) db_opt ;
  (* Exit code: 1 if blocking regressions *)
  if blocking <> [] then (
    Printf.printf "FAILED: %d metric(s) regressed.\n" (List.length blocking) ;
    exit 1)
  else
    Printf.printf
      "OK: No blocking regressions (%d improvements, %d unchanged, %d accepted).\n"
      (List.length result.improvements)
      (List.length result.unchanged)
      (List.length accepted_regressions)

(* ========================================================================== *)
(* Cmdliner CLI definition                                                    *)
(* ========================================================================== *)

open Cmdliner

let threshold_opt =
  Arg.(
    value & opt float 0.3
    & info
        ["threshold"; "t"]
        ~docv:"FLOAT"
        ~doc:"Minimum similarity threshold (0.0-1.0, default 0.3)")

let kind_opt =
  let kinds =
    Arg.enum [("all", `All); ("functions", `Functions); ("types", `Types)]
  in
  Arg.(
    value & opt kinds `All
    & info
        ["kind"; "k"]
        ~docv:"KIND"
        ~doc:"Search kind: all, functions, or types")

let search_query =
  Arg.(value & pos_all string [] & info [] ~docv:"WORDS" ~doc:"Search terms")

let min_lines_opt ~default ~doc =
  Arg.(value & opt int default & info ["min"] ~docv:"N" ~doc)

let field_names_opt =
  Arg.(
    value & opt_all string []
    & info ["field"; "f"] ~docv:"NAME" ~doc:"Field name to search for")

let field_types_opt =
  Arg.(
    value & opt_all string []
    & info
        ["field-type"; "T"]
        ~docv:"TYPE"
        ~doc:"Field type to search for (e.g. 'string', 'int')")

let sql_query =
  Arg.(
    required & pos 0 (some string) None & info [] ~docv:"SQL" ~doc:"SQL query")

(* -- Subcommands -- *)

let search_cmd =
  let doc = "Fuzzy search functions and types by intent, name, or signature" in
  let run threshold kind words =
    let query = String.concat " " words in
    if query = "" then (
      Printf.eprintf "Error: provide search terms\n" ;
      exit 1) ;
    cmd_search ~threshold ~kind query
  in
  Cmd.v
    (Cmd.info "search" ~doc)
    Term.(const run $ threshold_opt $ kind_opt $ search_query)

let search_types_cmd =
  let doc = "Find types by field names and/or field types" in
  let run field_names field_types =
    cmd_search_types ~field_names ~field_types
  in
  Cmd.v
    (Cmd.info "type-search" ~doc)
    Term.(const run $ field_names_opt $ field_types_opt)

let duplicates_cmd =
  let doc = "Find duplicate functions across modules" in
  Cmd.v (Cmd.info "duplicates" ~doc) Term.(const cmd_duplicates $ const ())

let mutables_cmd =
  let doc = "Show mutable patterns (ref, :=, !, mutable fields, Atomic)" in
  Cmd.v (Cmd.info "mutables" ~doc) Term.(const cmd_mutables $ const ())

let large_files_cmd =
  let doc = "Show large files" in
  let run min = cmd_large_files ~min_lines:min in
  Cmd.v
    (Cmd.info "large-files" ~doc)
    Term.(
      const run
      $ min_lines_opt ~default:500 ~doc:"Minimum line count (default: 500)")

let large_functions_cmd =
  let doc = "Show large functions" in
  let run min = cmd_large_functions ~min_lines:min in
  Cmd.v
    (Cmd.info "large-functions" ~doc)
    Term.(
      const run
      $ min_lines_opt ~default:50 ~doc:"Minimum line count (default: 50)")

let missing_docs_cmd =
  let doc = "Show exposed functions without documentation" in
  Cmd.v (Cmd.info "missing-docs" ~doc) Term.(const cmd_missing_docs $ const ())

let missing_mli_cmd =
  let doc = "Show modules without .mli interface files" in
  Cmd.v (Cmd.info "missing-mli" ~doc) Term.(const cmd_missing_mli $ const ())

let god_modules_cmd =
  let doc = "Show modules with too many functions" in
  let run min = cmd_god_modules ~min_fns:min in
  Cmd.v
    (Cmd.info "god-modules" ~doc)
    Term.(
      const run
      $ min_lines_opt ~default:30 ~doc:"Minimum function count (default: 30)")

let unsafe_strings_cmd =
  let doc = "Show string-typed fields that may need newtypes" in
  Cmd.v
    (Cmd.info "unsafe-strings" ~doc)
    Term.(const cmd_unsafe_strings $ const ())

let stats_cmd =
  let doc = "Show architecture index statistics" in
  Cmd.v (Cmd.info "stats" ~doc) Term.(const cmd_stats $ const ())

let sql_cmd =
  let doc = "Run raw SQL query against the architecture database" in
  Cmd.v (Cmd.info "sql" ~doc) Term.(const cmd_sql $ sql_query)

let refresh_cmd =
  let doc = "Rebuild the architecture index from .cmt files" in
  Cmd.v (Cmd.info "refresh" ~doc) Term.(const cmd_refresh $ const ())

let metrics_output_opt =
  Arg.(
    value
    & opt (some string) None
    & info
        ["output"; "o"]
        ~docv:"FILE"
        ~doc:"Write JSON to FILE instead of stdout")

let metrics_cmd =
  let doc =
    "Output code quality metrics as JSON (for CI comparison and badges)"
  in
  Cmd.v (Cmd.info "metrics" ~doc) Term.(const cmd_metrics $ metrics_output_opt)

let baseline_arg =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"BASELINE" ~doc:"Baseline metrics JSON file")

let current_arg =
  Arg.(
    required
    & pos 1 (some string) None
    & info [] ~docv:"CURRENT" ~doc:"Current metrics JSON file")

let compare_cmd =
  let doc =
    "Compare two metrics JSON files. Exits 1 if any metric regressed."
  in
  Cmd.v
    (Cmd.info "compare" ~doc)
    Term.(const cmd_compare $ baseline_arg $ current_arg)

let main_cmd =
  let doc = "Query the octez-manager architecture index" in
  let info = Cmd.info "arch-query" ~doc ~version:"0.1.0" in
  Cmd.group
    info
    [
      search_cmd;
      search_types_cmd;
      duplicates_cmd;
      mutables_cmd;
      large_files_cmd;
      large_functions_cmd;
      missing_docs_cmd;
      missing_mli_cmd;
      god_modules_cmd;
      unsafe_strings_cmd;
      stats_cmd;
      sql_cmd;
      refresh_cmd;
      metrics_cmd;
      compare_cmd;
    ]

let () = exit (Cmd.eval main_cmd)
