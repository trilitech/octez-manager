(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Architecture index generator.

    Scans .cmt/.cmti files produced by dune build and populates
    [docs/architecture.db] with modules, functions, types, record fields,
    and variant constructors. *)

(* -------------------------------------------------------------------------- *)
(* Database helpers                                                           *)
(* -------------------------------------------------------------------------- *)

let db_path =
  match Sys.getenv_opt "ARCH_DB_PATH" with
  | Some p -> p
  | None -> "docs/architecture.db"

let schema_path =
  match Sys.getenv_opt "ARCH_SCHEMA_PATH" with
  | Some p -> p
  | None -> "docs/architecture-schema.sql"

let open_db () =
  let db = Sqlite3.db_open db_path in
  ignore (Sqlite3.exec db "PRAGMA foreign_keys = ON") ;
  ignore (Sqlite3.exec db "PRAGMA journal_mode = WAL") ;
  db

let exec_exn db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Printf.eprintf
        "SQL error (%s): %s\nQuery: %s\n"
        (Sqlite3.Rc.to_string rc)
        (Sqlite3.errmsg db)
        sql ;
      exit 1

let exec_stmt db stmt =
  match Sqlite3.step stmt with
  | Sqlite3.Rc.DONE -> ignore (Sqlite3.reset stmt)
  | rc ->
      Printf.eprintf
        "Statement error (%s): %s\n"
        (Sqlite3.Rc.to_string rc)
        (Sqlite3.errmsg db) ;
      ignore (Sqlite3.reset stmt)

let last_insert_rowid db = Int64.to_int (Sqlite3.last_insert_rowid db)

let bind_text stmt idx v = ignore (Sqlite3.bind stmt idx (Sqlite3.Data.TEXT v))

let bind_int stmt idx v =
  ignore (Sqlite3.bind stmt idx (Sqlite3.Data.INT (Int64.of_int v)))

let bind_bool stmt idx v =
  ignore (Sqlite3.bind stmt idx (Sqlite3.Data.INT (if v then 1L else 0L)))

let bind_text_opt stmt idx = function
  | Some v -> bind_text stmt idx v
  | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)

(* -------------------------------------------------------------------------- *)
(* Schema initialisation                                                      *)
(* -------------------------------------------------------------------------- *)

let init_schema db =
  let ic = open_in schema_path in
  let n = in_channel_length ic in
  let sql = really_input_string ic n in
  close_in ic ;
  exec_exn db sql

(* -------------------------------------------------------------------------- *)
(* Preserve hand-written intent fields across re-index                        *)
(* -------------------------------------------------------------------------- *)

type intent_backup = {
  module_intents : (string * string) list; (* path -> intent *)
  function_intents : (string * string * string) list; (* path, name -> intent *)
  type_intents : (string * string * string) list; (* path, name -> intent *)
}

let backup_intents db =
  let module_intents = ref [] in
  let function_intents = ref [] in
  let type_intents = ref [] in
  (* Modules *)
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _headers ->
         module_intents := (row.(0), row.(1)) :: !module_intents)
       "SELECT path, intent FROM modules WHERE intent IS NOT NULL") ;
  (* Functions *)
  (try
     ignore
       (Sqlite3.exec_not_null
          db
          ~cb:(fun row _headers ->
            function_intents := (row.(0), row.(1), row.(2)) :: !function_intents)
          "SELECT m.path, f.name, f.intent FROM functions f JOIN modules m ON \
           f.module_id = m.id WHERE f.intent IS NOT NULL")
   with _ -> ()) ;
  (* Types *)
  (try
     ignore
       (Sqlite3.exec_not_null
          db
          ~cb:(fun row _headers ->
            type_intents := (row.(0), row.(1), row.(2)) :: !type_intents)
          "SELECT m.path, t.name, t.intent FROM types t JOIN modules m ON \
           t.module_id = m.id WHERE t.intent IS NOT NULL")
   with _ -> ()) ;
  {
    module_intents = !module_intents;
    function_intents = !function_intents;
    type_intents = !type_intents;
  }

let restore_intents db backup =
  let stmt_mod =
    Sqlite3.prepare db "UPDATE modules SET intent = ? WHERE path = ?"
  in
  let stmt_fn =
    Sqlite3.prepare
      db
      "UPDATE functions SET intent = ? WHERE name = ? AND module_id = (SELECT \
       id FROM modules WHERE path = ?)"
  in
  let stmt_ty =
    Sqlite3.prepare
      db
      "UPDATE types SET intent = ? WHERE name = ? AND module_id = (SELECT id \
       FROM modules WHERE path = ?)"
  in
  List.iter
    (fun (path, intent) ->
      bind_text stmt_mod 1 intent ;
      bind_text stmt_mod 2 path ;
      exec_stmt db stmt_mod)
    backup.module_intents ;
  List.iter
    (fun (path, name, intent) ->
      bind_text stmt_fn 1 intent ;
      bind_text stmt_fn 2 name ;
      bind_text stmt_fn 3 path ;
      exec_stmt db stmt_fn)
    backup.function_intents ;
  List.iter
    (fun (path, name, intent) ->
      bind_text stmt_ty 1 intent ;
      bind_text stmt_ty 2 name ;
      bind_text stmt_ty 3 path ;
      exec_stmt db stmt_ty)
    backup.type_intents ;
  ignore (Sqlite3.finalize stmt_mod) ;
  ignore (Sqlite3.finalize stmt_fn) ;
  ignore (Sqlite3.finalize stmt_ty)

(* -------------------------------------------------------------------------- *)
(* Source-path mapping                                                        *)
(* -------------------------------------------------------------------------- *)

(** Project root, derived from the build directory.
    E.g. if build_dir is [/foo/bar/_build/default/src], project_root is [/foo/bar]. *)
let project_root = ref ""

(** Map a .cmt source file path to a relative source path under [src/].
    Handles ppx-preprocessed files (e.g. [src/ui/data.pp.ml] -> [src/ui/data.ml]).
    Returns [None] if the file doesn't belong to the project sources. *)
let source_path_of_cmt (info : Cmt_format.cmt_infos) =
  let try_strip_pp p =
    let dir = Filename.dirname p in
    let base = Filename.basename p in
    match String.split_on_char '.' base with
    | name :: "pp" :: rest ->
        let original = Filename.concat dir (String.concat "." (name :: rest)) in
        if Sys.file_exists original then Some original else None
    | _ -> None
  in
  let try_resolve p =
    if Sys.file_exists p then Some p
    else
      match try_strip_pp p with
      | Some _ as r -> r
      | None ->
          (* Try resolving relative to project root *)
          if !project_root <> "" then
            let abs = Filename.concat !project_root p in
            if Sys.file_exists abs then Some abs else try_strip_pp abs
          else None
  in
  match info.cmt_sourcefile with
  | Some path when String.length path > 0 -> try_resolve path
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* Type printing helper                                                       *)
(* -------------------------------------------------------------------------- *)
let type_to_string ty = Format.asprintf "%a" Printtyp.type_expr ty

(* -------------------------------------------------------------------------- *)
(* Doc-comment extraction                                                     *)
(* -------------------------------------------------------------------------- *)

(** Extract the first doc-comment line from OCaml attributes.
    Doc comments are stored as [\[@ocaml.doc "..."\]] attributes. *)
let extract_doc (attrs : Parsetree.attributes) =
  List.find_map
    (fun (attr : Parsetree.attribute) ->
      if attr.attr_name.txt = "ocaml.doc" || attr.attr_name.txt = "doc" then
        match attr.attr_payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ( {
                        pexp_desc =
                          Pexp_constant
                            {pconst_desc = Pconst_string (s, _, _); _};
                        _;
                      },
                      _ );
                _;
              };
            ] ->
            let trimmed = String.trim s in
            if trimmed = "" then None else Some trimmed
        | _ -> None
      else None)
    attrs

(* -------------------------------------------------------------------------- *)
(* Scanning .cmt/.cmti files                                                  *)
(* -------------------------------------------------------------------------- *)

let find_cmt_files build_dir =
  let files = ref [] in
  let rec walk dir =
    let entries = Sys.readdir dir in
    Array.iter
      (fun entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then walk path
        else if
          Filename.check_suffix path ".cmt"
          || Filename.check_suffix path ".cmti"
        then files := path :: !files)
      entries
  in
  walk build_dir ;
  List.sort String.compare !files

(* -------------------------------------------------------------------------- *)
(* Exposed-name collection from .cmti files                                   *)
(* -------------------------------------------------------------------------- *)

(** Collect names exposed in .cmti (interface) files. Returns two tables:
    - exposed: (module_name, name) -> true
    - docs: (module_name, name) -> doc string *)
let collect_exposed cmti_files =
  let exposed_tbl = Hashtbl.create 256 in
  let doc_tbl = Hashtbl.create 256 in
  List.iter
    (fun path ->
      try
        match Cmt_format.read path with
        | _, Some info -> (
            let modname = info.cmt_modname in
            match info.cmt_annots with
            | Interface sg ->
                List.iter
                  (fun (item : Typedtree.signature_item) ->
                    match item.sig_desc with
                    | Tsig_value vd -> (
                        let name = Ident.name vd.val_id in
                        Hashtbl.replace exposed_tbl (modname, name) true ;
                        match extract_doc vd.val_attributes with
                        | Some doc ->
                            Hashtbl.replace doc_tbl (modname, name) doc
                        | None -> ())
                    | Tsig_type (_, tds) ->
                        List.iter
                          (fun (td : Typedtree.type_declaration) ->
                            let name = Ident.name td.typ_id in
                            Hashtbl.replace exposed_tbl (modname, name) true ;
                            match extract_doc td.typ_attributes with
                            | Some doc ->
                                Hashtbl.replace doc_tbl (modname, name) doc
                            | None -> ())
                          tds
                    | _ -> ())
                  sg.sig_items
            | _ -> ())
        | _ -> ()
      with exn ->
        Printf.eprintf
          "Warning: failed to read cmti %s: %s\n"
          path
          (Printexc.to_string exn))
    cmti_files ;
  (exposed_tbl, doc_tbl)

(* -------------------------------------------------------------------------- *)
(* Insert helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let insert_module db stmt_mod ~path ~lines ~has_mli =
  let now =
    Printf.sprintf
      "%04d-%02d-%02dT%02d:%02d:%02d"
      (let t = Unix.gmtime (Unix.gettimeofday ()) in
       t.tm_year + 1900)
      (let t = Unix.gmtime (Unix.gettimeofday ()) in
       t.tm_mon + 1)
      (let t = Unix.gmtime (Unix.gettimeofday ()) in
       t.tm_mday)
      (let t = Unix.gmtime (Unix.gettimeofday ()) in
       t.tm_hour)
      (let t = Unix.gmtime (Unix.gettimeofday ()) in
       t.tm_min)
      (let t = Unix.gmtime (Unix.gettimeofday ()) in
       t.tm_sec)
  in
  bind_text stmt_mod 1 path ;
  bind_int stmt_mod 2 lines ;
  bind_text stmt_mod 3 now ;
  bind_bool stmt_mod 4 has_mli ;
  exec_stmt db stmt_mod ;
  last_insert_rowid db

let insert_function db stmt_fn ~module_id ~name ~signature ~line_start ~line_end
    ~exposed ~is_alias ~intent =
  bind_int stmt_fn 1 module_id ;
  bind_text stmt_fn 2 name ;
  bind_text_opt stmt_fn 3 signature ;
  bind_int stmt_fn 4 line_start ;
  bind_int stmt_fn 5 line_end ;
  bind_bool stmt_fn 6 exposed ;
  bind_bool stmt_fn 7 is_alias ;
  bind_text_opt stmt_fn 8 intent ;
  exec_stmt db stmt_fn ;
  last_insert_rowid db

let insert_type db stmt_ty ~module_id ~name ~kind ~line_start ~line_end ~exposed
    ~manifest ~intent =
  bind_int stmt_ty 1 module_id ;
  bind_text stmt_ty 2 name ;
  bind_text stmt_ty 3 kind ;
  bind_int stmt_ty 4 line_start ;
  bind_int stmt_ty 5 line_end ;
  bind_bool stmt_ty 6 exposed ;
  bind_text_opt stmt_ty 7 manifest ;
  bind_text_opt stmt_ty 8 intent ;
  exec_stmt db stmt_ty ;
  last_insert_rowid db

let insert_field db stmt_fld ~type_id ~field_name ~field_type ~position
    ~is_mutable =
  bind_int stmt_fld 1 type_id ;
  bind_text stmt_fld 2 field_name ;
  bind_text stmt_fld 3 field_type ;
  bind_int stmt_fld 4 position ;
  bind_bool stmt_fld 5 is_mutable ;
  exec_stmt db stmt_fld

let insert_mutable_usage db stmt_mut ~function_id ~kind ~line =
  bind_int stmt_mut 1 function_id ;
  bind_text stmt_mut 2 kind ;
  bind_int stmt_mut 3 line ;
  exec_stmt db stmt_mut

(* -------------------------------------------------------------------------- *)
(* Mutable pattern detection in expressions                                   *)
(* -------------------------------------------------------------------------- *)

(** Check if string starts with prefix *)
let starts_with ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(** Detect mutable pattern kind from a path *)
let mutable_kind_of_path (path : Path.t) =
  let s = Path.name path in
  if s = "Stdlib.ref" || s = "ref" then Some "ref"
  else if s = "Stdlib.!" || s = "!" then Some "ref_deref"
  else if s = "Stdlib.:=" || s = ":=" then Some "ref_assign"
  else if s = "Stdlib.incr" || s = "incr" then Some "ref_assign"
  else if s = "Stdlib.decr" || s = "decr" then Some "ref_assign"
  else if s = "Atomic.make" || s = "Stdlib.Atomic.make" then Some "atomic_make"
  else if s = "Atomic.get" || s = "Stdlib.Atomic.get" then Some "atomic_get"
  else if s = "Atomic.set" || s = "Stdlib.Atomic.set" then Some "atomic_set"
  else if
    starts_with ~prefix:"Atomic." s || starts_with ~prefix:"Stdlib.Atomic." s
  then Some "atomic_other"
  else None

(** Recursively scan an expression for mutable patterns.
    Returns a list of (kind, line) pairs. *)
let rec scan_expr_for_mutables (expr : Typedtree.expression) =
  let line = expr.exp_loc.loc_start.pos_lnum in
  let from_desc =
    match expr.exp_desc with
    | Texp_ident (path, _, _) -> (
        match mutable_kind_of_path path with
        | Some kind -> [(kind, line)]
        | None -> [])
    | Texp_apply (fn, args) ->
        let fn_muts = scan_expr_for_mutables fn in
        let arg_muts =
          List.concat_map
            (fun (_, arg_opt) ->
              match arg_opt with
              | Some e -> scan_expr_for_mutables e
              | None -> [])
            args
        in
        fn_muts @ arg_muts
    | Texp_let (_, vbs, body) ->
        let vb_muts =
          List.concat_map
            (fun (vb : Typedtree.value_binding) ->
              scan_expr_for_mutables vb.vb_expr)
            vbs
        in
        vb_muts @ scan_expr_for_mutables body
    | Texp_function (_, body) -> (
        match body with
        | Typedtree.Tfunction_body e -> scan_expr_for_mutables e
        | Typedtree.Tfunction_cases {cases; _} -> scan_cases_for_mutables cases)
    | Texp_match (e, _, cases, _) ->
        scan_expr_for_mutables e @ scan_cases_for_mutables cases
    | Texp_try (e, cases, _) ->
        scan_expr_for_mutables e @ scan_cases_for_mutables cases
    | Texp_tuple es -> List.concat_map scan_expr_for_mutables es
    | Texp_construct (_, _, es) -> List.concat_map scan_expr_for_mutables es
    | Texp_variant (_, Some e) -> scan_expr_for_mutables e
    | Texp_record {fields; extended_expression; _} ->
        let field_muts =
          Array.to_list fields
          |> List.concat_map (fun (_, def) ->
              match def with
              | Typedtree.Overridden (_, e) -> scan_expr_for_mutables e
              | Typedtree.Kept _ -> [])
        in
        let ext_muts =
          match extended_expression with
          | Some e -> scan_expr_for_mutables e
          | None -> []
        in
        field_muts @ ext_muts
    | Texp_field (e, _, _) -> scan_expr_for_mutables e
    | Texp_setfield (e1, _, _, e2) ->
        (* Field assignment is a mutable operation *)
        [("mutable_field", line)]
        @ scan_expr_for_mutables e1 @ scan_expr_for_mutables e2
    | Texp_array es -> List.concat_map scan_expr_for_mutables es
    | Texp_ifthenelse (e1, e2, e3_opt) -> (
        scan_expr_for_mutables e1 @ scan_expr_for_mutables e2
        @ match e3_opt with Some e3 -> scan_expr_for_mutables e3 | None -> [])
    | Texp_sequence (e1, e2) ->
        scan_expr_for_mutables e1 @ scan_expr_for_mutables e2
    | Texp_while (e1, e2) ->
        scan_expr_for_mutables e1 @ scan_expr_for_mutables e2
    | Texp_for (_, _, e1, e2, _, e3) ->
        scan_expr_for_mutables e1 @ scan_expr_for_mutables e2
        @ scan_expr_for_mutables e3
    | Texp_send (e, _) -> scan_expr_for_mutables e
    | Texp_new _ -> []
    | Texp_instvar _ -> []
    | Texp_setinstvar (_, _, _, e) ->
        [("mutable_field", line)] @ scan_expr_for_mutables e
    | Texp_override (_, es) ->
        List.concat_map (fun (_, _, e) -> scan_expr_for_mutables e) es
    | Texp_letmodule (_, _, _, _, e) -> scan_expr_for_mutables e
    | Texp_letexception (_, e) -> scan_expr_for_mutables e
    | Texp_assert (e, _) -> scan_expr_for_mutables e
    | Texp_lazy e -> scan_expr_for_mutables e
    | Texp_object _ -> []
    | Texp_pack _ -> []
    | Texp_letop {body; _} -> scan_cases_for_mutables [body]
    | Texp_unreachable -> []
    | Texp_extension_constructor _ -> []
    | Texp_open (_, e) -> scan_expr_for_mutables e
    | _ -> []
  in
  from_desc

(** Helper to scan cases *)
and scan_cases_for_mutables cases =
  List.concat_map
    (fun (c : Typedtree.value Typedtree.case) -> scan_expr_for_mutables c.c_rhs)
    cases

let insert_constructor db stmt_ctor ~type_id ~constructor_name ~position
    ~arg_types =
  bind_int stmt_ctor 1 type_id ;
  bind_text stmt_ctor 2 constructor_name ;
  bind_int stmt_ctor 3 position ;
  bind_text_opt stmt_ctor 4 arg_types ;
  exec_stmt db stmt_ctor

(* -------------------------------------------------------------------------- *)
(* Process a single .cmt file                                                 *)
(* -------------------------------------------------------------------------- *)

let process_cmt db ~exposed_tbl ~doc_tbl ~stmt_mod ~stmt_fn ~stmt_ty ~stmt_fld
    ~stmt_ctor ~stmt_mut path =
  match Cmt_format.read path with
  | _, None -> ()
  | _, Some info -> (
      (* Only process Implementation (not Interface -- we use .cmti for
       exposed-name detection only) *)
      match info.cmt_annots with
      | Implementation structure -> (
          match source_path_of_cmt info with
          | None -> ()
          | Some src_path ->
              let modname = info.cmt_modname in
              (* Store path relative to project root if possible *)
              let rel_path =
                if !project_root <> "" then
                  let prefix = !project_root ^ "/" in
                  if
                    String.length src_path >= String.length prefix
                    && String.sub src_path 0 (String.length prefix) = prefix
                  then
                    String.sub
                      src_path
                      (String.length prefix)
                      (String.length src_path - String.length prefix)
                  else src_path
                else src_path
              in
              (* Count source lines *)
              let lines =
                let ic = open_in src_path in
                let n = ref 0 in
                (try
                   while true do
                     ignore (input_line ic) ;
                     incr n
                   done
                 with End_of_file -> ()) ;
                close_in ic ;
                !n
              in
              (* Check if .mli exists *)
              let has_mli =
                let mli = Filename.remove_extension src_path ^ ".mli" in
                Sys.file_exists mli
              in
              let module_id =
                insert_module db stmt_mod ~path:rel_path ~lines ~has_mli
              in
              (* Process structure items *)
              List.iter
                (fun (item : Typedtree.structure_item) ->
                  match item.str_desc with
                  | Tstr_value (_, vbs) ->
                      List.iter
                        (fun (vb : Typedtree.value_binding) ->
                          match vb.vb_pat.pat_desc with
                          | Tpat_var (id, _, _) ->
                              let name = Ident.name id in
                              let signature =
                                Some (type_to_string vb.vb_pat.pat_type)
                              in
                              (* Use pattern location for start to exclude doc comments *)
                              let line_start =
                                vb.vb_pat.pat_loc.loc_start.pos_lnum
                              in
                              let line_end = vb.vb_loc.loc_end.pos_lnum in
                              let exposed =
                                Hashtbl.mem exposed_tbl (modname, name)
                              in
                              (* Detect delegation aliases: let f = Module.f *)
                              let is_alias =
                                match vb.vb_expr.exp_desc with
                                | Texp_ident (Path.Pdot _, _, _) -> true
                                | _ -> false
                              in
                              (* Prefer .mli doc; fall back to .ml doc *)
                              let intent =
                                match
                                  Hashtbl.find_opt doc_tbl (modname, name)
                                with
                                | Some _ as d -> d
                                | None -> extract_doc vb.vb_attributes
                              in
                              let function_id =
                                insert_function
                                  db
                                  stmt_fn
                                  ~module_id
                                  ~name
                                  ~signature
                                  ~line_start
                                  ~line_end
                                  ~exposed
                                  ~is_alias
                                  ~intent
                              in
                              (* Scan for mutable patterns in the expression *)
                              let mutables =
                                scan_expr_for_mutables vb.vb_expr
                              in
                              List.iter
                                (fun (kind, line) ->
                                  insert_mutable_usage
                                    db
                                    stmt_mut
                                    ~function_id
                                    ~kind
                                    ~line)
                                mutables
                          | _ -> ())
                        vbs
                  | Tstr_type (_, tds) ->
                      List.iter
                        (fun (td : Typedtree.type_declaration) ->
                          let name = Ident.name td.typ_id in
                          (* Use name location for start to exclude doc comments *)
                          let line_start = td.typ_name.loc.loc_start.pos_lnum in
                          let line_end = td.typ_loc.loc_end.pos_lnum in
                          let exposed =
                            Hashtbl.mem exposed_tbl (modname, name)
                          in
                          let kind, manifest =
                            match td.typ_type.type_kind with
                            | Type_record _ -> ("record", None)
                            | Type_variant _ -> ("variant", None)
                            | Type_open -> ("open", None)
                            | Type_abstract _ -> (
                                match td.typ_type.type_manifest with
                                | Some ty -> ("alias", Some (type_to_string ty))
                                | None -> ("abstract", None))
                          in
                          let intent =
                            match Hashtbl.find_opt doc_tbl (modname, name) with
                            | Some _ as d -> d
                            | None -> extract_doc td.typ_attributes
                          in
                          let type_id =
                            insert_type
                              db
                              stmt_ty
                              ~module_id
                              ~name
                              ~kind
                              ~line_start
                              ~line_end
                              ~exposed
                              ~manifest
                              ~intent
                          in
                          (* Insert record fields *)
                          match td.typ_type.type_kind with
                          | Type_record (labels, _) ->
                              List.iteri
                                (fun position (ld : Types.label_declaration) ->
                                  let field_name = Ident.name ld.ld_id in
                                  let field_type = type_to_string ld.ld_type in
                                  let is_mutable =
                                    ld.ld_mutable = Asttypes.Mutable
                                  in
                                  insert_field
                                    db
                                    stmt_fld
                                    ~type_id
                                    ~field_name
                                    ~field_type
                                    ~position
                                    ~is_mutable)
                                labels
                          | Type_variant (constrs, _) ->
                              List.iteri
                                (fun position
                                     (cd : Types.constructor_declaration)
                                   ->
                                  let constructor_name = Ident.name cd.cd_id in
                                  let arg_types =
                                    match cd.cd_args with
                                    | Cstr_tuple [] -> None
                                    | Cstr_tuple args ->
                                        Some
                                          (String.concat
                                             ", "
                                             (List.map type_to_string args))
                                    | Cstr_record labels ->
                                        Some
                                          (String.concat
                                             ", "
                                             (List.map
                                                (fun (ld :
                                                       Types.label_declaration)
                                                   ->
                                                  Printf.sprintf
                                                    "%s: %s"
                                                    (Ident.name ld.ld_id)
                                                    (type_to_string ld.ld_type))
                                                labels))
                                  in
                                  insert_constructor
                                    db
                                    stmt_ctor
                                    ~type_id
                                    ~constructor_name
                                    ~position
                                    ~arg_types)
                                constrs
                          | _ -> ())
                        tds
                  | _ -> ())
                structure.str_items)
      | _ -> ())

(* -------------------------------------------------------------------------- *)
(* Main                                                                       *)
(* -------------------------------------------------------------------------- *)

let () =
  let build_dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "_build/default/src"
  in
  (* Derive project root from build_dir: strip _build/default/... suffix *)
  (let abs_build =
     if Filename.is_relative build_dir then
       Filename.concat (Sys.getcwd ()) build_dir
     else build_dir
   in
   match
     String.split_on_char '/' abs_build
     |> List.to_seq
     |> Seq.find_index (fun s -> s = "_build")
   with
   | Some idx ->
       let parts = String.split_on_char '/' abs_build in
       let root_parts = List.filteri (fun i _ -> i < idx) parts in
       project_root := String.concat "/" root_parts
   | None -> ()) ;
  if !project_root <> "" then Printf.printf "Project root: %s\n%!" !project_root ;
  Printf.printf "Scanning %s for .cmt/.cmti files...\n%!" build_dir ;
  let all_files = find_cmt_files build_dir in
  let cmt_files =
    List.filter (fun f -> Filename.check_suffix f ".cmt") all_files
  in
  let cmti_files =
    List.filter (fun f -> Filename.check_suffix f ".cmti") all_files
  in
  Printf.printf
    "Found %d .cmt and %d .cmti files\n%!"
    (List.length cmt_files)
    (List.length cmti_files) ;

  (* Collect exposed names and doc comments from .cmti files *)
  let exposed_tbl, doc_tbl = collect_exposed cmti_files in
  Printf.printf
    "Found %d exposed names, %d doc comments\n%!"
    (Hashtbl.length exposed_tbl)
    (Hashtbl.length doc_tbl) ;

  (* Open or create database *)
  let db = open_db () in

  (* Backup intents before wiping *)
  let backup = backup_intents db in
  Printf.printf
    "Backed up %d module intents, %d function intents, %d type intents\n%!"
    (List.length backup.module_intents)
    (List.length backup.function_intents)
    (List.length backup.type_intents) ;

  (* Drop and recreate auto-populated tables *)
  exec_exn db "DROP TABLE IF EXISTS mutable_usages" ;
  exec_exn db "DROP TABLE IF EXISTS type_constructors" ;
  exec_exn db "DROP TABLE IF EXISTS type_fields" ;
  exec_exn db "DROP TABLE IF EXISTS types" ;
  exec_exn db "DROP TABLE IF EXISTS calls" ;
  exec_exn db "DROP TABLE IF EXISTS functions" ;
  exec_exn db "DROP TABLE IF EXISTS modules" ;
  (* Drop views that reference these tables *)
  exec_exn db "DROP VIEW IF EXISTS v_large_files" ;
  exec_exn db "DROP VIEW IF EXISTS v_large_functions" ;
  exec_exn db "DROP VIEW IF EXISTS v_undocumented" ;
  exec_exn db "DROP VIEW IF EXISTS v_unsafe_params" ;
  exec_exn db "DROP VIEW IF EXISTS v_low_coverage" ;
  exec_exn db "DROP VIEW IF EXISTS v_most_called" ;
  exec_exn db "DROP VIEW IF EXISTS v_open_tasks" ;
  exec_exn db "DROP VIEW IF EXISTS v_type_fields" ;
  exec_exn db "DROP VIEW IF EXISTS v_types_with_field_type" ;
  exec_exn db "DROP VIEW IF EXISTS v_variant_constructors" ;
  exec_exn db "DROP VIEW IF EXISTS v_mutable_fields" ;
  exec_exn db "DROP VIEW IF EXISTS v_mutable_functions" ;

  (* Re-create schema *)
  init_schema db ;

  (* Prepare statements *)
  let stmt_mod =
    Sqlite3.prepare
      db
      "INSERT INTO modules (path, lines, last_analyzed, has_mli) VALUES (?, ?, \
       ?, ?)"
  in
  let stmt_fn =
    Sqlite3.prepare
      db
      "INSERT OR REPLACE INTO functions (module_id, name, signature, \
       line_start, line_end, exposed, is_alias, intent) VALUES (?, ?, ?, ?, ?, \
       ?, ?, ?)"
  in
  let stmt_ty =
    Sqlite3.prepare
      db
      "INSERT OR REPLACE INTO types (module_id, name, kind, line_start, \
       line_end, exposed, manifest, intent) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt_fld =
    Sqlite3.prepare
      db
      "INSERT INTO type_fields (type_id, field_name, field_type, position, \
       is_mutable) VALUES (?, ?, ?, ?, ?)"
  in
  let stmt_ctor =
    Sqlite3.prepare
      db
      "INSERT INTO type_constructors (type_id, constructor_name, position, \
       arg_types) VALUES (?, ?, ?, ?)"
  in
  let stmt_mut =
    Sqlite3.prepare
      db
      "INSERT OR IGNORE INTO mutable_usages (function_id, kind, line) VALUES \
       (?, ?, ?)"
  in

  (* Process all .cmt files inside a transaction *)
  exec_exn db "BEGIN TRANSACTION" ;
  let n_modules = ref 0 in
  let n_functions = ref 0 in
  let n_types = ref 0 in
  List.iter
    (fun path ->
      (* We count by hooking into the insert functions. For simplicity
         we use a try/catch since some cmt files may fail. *)
      try
        process_cmt
          db
          ~exposed_tbl
          ~doc_tbl
          ~stmt_mod
          ~stmt_fn
          ~stmt_ty
          ~stmt_fld
          ~stmt_ctor
          ~stmt_mut
          path
      with exn ->
        Printf.eprintf
          "Warning: failed to process %s: %s\n"
          path
          (Printexc.to_string exn))
    cmt_files ;
  exec_exn db "COMMIT" ;

  (* Count results *)
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h -> n_modules := int_of_string row.(0))
       "SELECT COUNT(*) FROM modules") ;
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h -> n_functions := int_of_string row.(0))
       "SELECT COUNT(*) FROM functions") ;
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h -> n_types := int_of_string row.(0))
       "SELECT COUNT(*) FROM types") ;

  (* Restore intents *)
  restore_intents db backup ;

  (* Summary *)
  let n_fields = ref 0 in
  let n_ctors = ref 0 in
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h -> n_fields := int_of_string row.(0))
       "SELECT COUNT(*) FROM type_fields") ;
  ignore
    (Sqlite3.exec_not_null
       db
       ~cb:(fun row _h -> n_ctors := int_of_string row.(0))
       "SELECT COUNT(*) FROM type_constructors") ;
  Printf.printf
    "\n\
     Done! Indexed:\n\
    \  %d modules\n\
    \  %d functions\n\
    \  %d types (%d record fields, %d variant constructors)\n\
     Database: %s\n"
    !n_modules
    !n_functions
    !n_types
    !n_fields
    !n_ctors
    db_path ;

  ignore (Sqlite3.db_close db)
