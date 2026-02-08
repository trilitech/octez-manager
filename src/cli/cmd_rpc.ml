(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib

(** Get a service by instance name *)
let get_service instance =
  match Service_registry.find ~instance with
  | Ok (Some svc) -> Ok svc
  | Ok None -> Error (Printf.sprintf "Instance '%s' not found" instance)
  | Error (`Msg msg) -> Error msg

(** Create a synthetic service for a raw URL endpoint *)
let service_from_url url =
  Service.make
    ~instance:(Printf.sprintf "url:%s" url)
    ~role:"node"
    ~network:"unknown"
    ~history_mode:History_mode.Rolling
    ~data_dir:""
    ~rpc_addr:(Rpc_addr.of_string url)
    ~net_addr:""
    ~service_user:""
    ~app_bin_dir:""
    ~logging_mode:Logging_mode.Journald
    ()

(** Get public nodes from Taquito *)
let get_public_nodes () = Octez_manager_ui.Public_nodes_cache.get_services ()

(** Find a public node by index (1-based) or partial name match *)
let find_public_node query =
  let nodes = get_public_nodes () in
  (* Try as index first *)
  match int_of_string_opt query with
  | Some idx when idx >= 1 && idx <= List.length nodes ->
      Some (List.nth nodes (idx - 1))
  | _ ->
      (* Try partial name match (case-insensitive) *)
      let query_lower = String.lowercase_ascii query in
      List.find_opt
        (fun svc ->
          let name = String.lowercase_ascii svc.Service.instance in
          String.sub
            name
            0
            (min (String.length query_lower) (String.length name))
          = query_lower)
        nodes

(** Resolve service from --instance, --url, or --public options *)
let resolve_service instance_opt url_opt public_opt =
  match (instance_opt, url_opt, public_opt) with
  | Some _, Some _, _ | Some _, _, Some _ | _, Some _, Some _ ->
      Error
        "Cannot specify multiple target options (use only one of --instance, \
         --url, or --public)"
  | None, None, None ->
      Error "Must specify a target: --instance, --url, or --public"
  | Some instance, None, None -> get_service instance
  | None, Some url, None -> Ok (service_from_url url)
  | None, None, Some query -> (
      match find_public_node query with
      | Some svc -> Ok svc
      | None ->
          Error
            (Printf.sprintf
               "Public node '%s' not found. Use 'rpc public-nodes' to list \
                available nodes."
               query))

(** List available node instances *)
let list_instances () =
  match Service_registry.list () with
  | Error (`Msg msg) ->
      Printf.eprintf "Error listing instances: %s\n" msg ;
      []
  | Ok services ->
      List.filter (fun svc -> svc.Service.role = "node") services
      |> List.map (fun svc -> svc.Service.instance)

(** Execute a single RPC GET request *)
let execute_get service path =
  match Octez_manager_ui.Rpc_client.http_get_url service path with
  | Ok body -> (
      match Octez_manager_ui.Json_highlighter.highlight body with
      | Ok highlighted ->
          Printf.printf "%s\n" highlighted ;
          flush stdout
      | Error _ ->
          Printf.printf "%s\n" body ;
          flush stdout)
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg ;
      flush stderr

(** Get completions for a path prefix *)
let _get_completions service prefix =
  (* Split prefix into path segments *)
  let segments =
    String.split_on_char '/' prefix |> List.filter (fun s -> s <> "")
  in
  (* Get parent path and partial segment *)
  let parent_segs, partial =
    match List.rev segments with
    | [] -> ([], "")
    | last :: rest -> (List.rev rest, last)
  in
  (* Fetch entries at parent path *)
  let entries, _ =
    Octez_manager_ui.Rpc_describe.fetch_entries service ~segs:parent_segs
  in
  (* Filter by partial match *)
  List.filter_map
    (fun (e : Octez_manager_ui.Rpc_describe.entry) ->
      if
        String.length partial = 0
        || String.sub
             e.name
             0
             (min (String.length partial) (String.length e.name))
           = partial
      then
        let kind_str =
          match e.kind with
          | Octez_manager_ui.Rpc_describe.Sub -> "[SUB]"
          | Octez_manager_ui.Rpc_describe.Get -> "[GET]"
          | Octez_manager_ui.Rpc_describe.Dyn typ ->
              Printf.sprintf "[DYN:%s]" typ
        in
        Some (e.name, kind_str)
      else None)
    entries

(** Build full path from segments *)
let build_path segs = "/" ^ String.concat "/" segs

(** Completion callback for linenoise *)
let completion_callback service input =
  (* Only complete paths starting with / *)
  if String.length input = 0 || input.[0] <> '/' then []
  else
    let input_path = String.sub input 1 (String.length input - 1) in
    let segments =
      String.split_on_char '/' input_path |> List.filter (fun s -> s <> "")
    in
    (* Determine parent path and partial *)
    let parent_segs, partial =
      if String.length input > 0 && input.[String.length input - 1] = '/' then
        (segments, "")
      else
        match List.rev segments with
        | [] -> ([], "")
        | last :: rest -> (List.rev rest, last)
    in
    (* Fetch entries at parent path *)
    let entries, _ =
      Octez_manager_ui.Rpc_describe.fetch_entries service ~segs:parent_segs
    in
    (* Filter and build completions *)
    List.filter_map
      (fun (e : Octez_manager_ui.Rpc_describe.entry) ->
        let name = e.name in
        if
          String.length partial = 0
          || String.length name >= String.length partial
             && String.sub name 0 (String.length partial) = partial
        then Some (build_path (parent_segs @ [name]))
        else None)
      entries

(** Interactive RPC REPL with linenoise *)
let run_interactive service =
  Printf.printf "RPC Interactive Mode\n" ;
  Printf.printf
    "Instance: %s (%s)\n"
    service.Service.instance
    service.Service.network ;
  Printf.printf "Commands: /path (execute), Tab (complete), help, exit\n\n" ;
  flush stdout ;
  (* Set up linenoise completion *)
  LNoise.set_completion_callback (fun line_so_far ln_completions ->
      let completions = completion_callback service line_so_far in
      List.iter (LNoise.add_completion ln_completions) completions) ;
  (* Set up hints *)
  LNoise.set_hints_callback (fun line_so_far ->
      if String.length line_so_far > 0 && line_so_far.[0] = '/' then
        let completions = completion_callback service line_so_far in
        match completions with
        | c :: _ when c <> line_so_far ->
            let hint =
              String.sub
                c
                (String.length line_so_far)
                (String.length c - String.length line_so_far)
            in
            Some (hint, LNoise.Cyan, false)
        | _ -> None
      else None) ;
  (* Load history *)
  let history_file =
    Filename.concat (Paths.xdg_config_home ()) "octez-manager/rpc_history"
  in
  let history_dir = Filename.dirname history_file in
  (if not (Sys.file_exists history_dir) then
     try Unix.mkdir history_dir 0o755 with _ -> ()) ;
  ignore (LNoise.history_load ~filename:history_file) ;
  ignore (LNoise.history_set ~max_length:100) ;
  (* Main loop *)
  let rec loop () =
    match LNoise.linenoise "RPC> " with
    | None -> ()
    | Some line ->
        let line = String.trim line in
        if line = "" then loop ()
        else (
          ignore (LNoise.history_add line) ;
          ignore (LNoise.history_save ~filename:history_file) ;
          if line = "exit" || line = "quit" then ()
          else if line = "help" || line = "?" then (
            Printf.printf "\nCommands:\n" ;
            Printf.printf "  /path       Execute GET request\n" ;
            Printf.printf "  Tab         Auto-complete path\n" ;
            Printf.printf "  ↑/↓         Navigate history\n" ;
            Printf.printf "  help        Show this help\n" ;
            Printf.printf "  exit        Quit\n\n" ;
            flush stdout ;
            loop ())
          else if String.length line > 0 && line.[0] = '/' then (
            (* Strip [GET] suffix if present (user selected from completion) *)
            let path =
              if
                String.length line > 6
                && String.sub line (String.length line - 6) 6 = "/[GET]"
              then String.sub line 0 (String.length line - 6)
              else line
            in
            execute_get service path ;
            loop ())
          else (
            Printf.printf "Unknown command. Type 'help' for usage.\n" ;
            flush stdout ;
            loop ()))
  in
  loop () ;
  Printf.printf "Goodbye.\n" ;
  flush stdout

(** Common argument definitions *)
let instance_arg =
  Arg.(
    value
    & opt (some string) None
    & info ["i"; "instance"] ~doc:"Local instance name" ~docv:"INSTANCE")

let url_arg =
  Arg.(
    value
    & opt (some string) None
    & info
        ["u"; "url"]
        ~doc:"RPC endpoint URL (e.g., https://mainnet.tezos.ecadinfra.com)"
        ~docv:"URL")

let public_arg =
  Arg.(
    value
    & opt (some string) None
    & info
        ["p"; "public"]
        ~doc:"Public node (index or name from 'rpc public-nodes')"
        ~docv:"NODE")

(** rpc get command *)
let get_cmd =
  let doc = "Execute a GET request to an RPC endpoint" in
  let path_arg =
    Arg.(
      required & pos 0 (some string) None & info [] ~doc:"RPC path" ~docv:"PATH")
  in
  let term =
    Term.(
      ret
        (const (fun instance_opt url_opt public_opt path ->
             match resolve_service instance_opt url_opt public_opt with
             | Error msg -> Cli_helpers.cmdliner_error msg
             | Ok service ->
                 let path = if path.[0] = '/' then path else "/" ^ path in
                 execute_get service path ;
                 `Ok ())
        $ instance_arg $ url_arg $ public_arg $ path_arg))
  in
  Cmd.v (Cmd.info "get" ~doc) term

(** rpc interactive command *)
let interactive_cmd =
  let doc = "Start interactive RPC mode with completion" in
  let term =
    Term.(
      ret
        (const (fun instance_opt url_opt public_opt ->
             match resolve_service instance_opt url_opt public_opt with
             | Error msg -> Cli_helpers.cmdliner_error msg
             | Ok service ->
                 run_interactive service ;
                 `Ok ())
        $ instance_arg $ url_arg $ public_arg))
  in
  Cmd.v (Cmd.info "interactive" ~doc ~docs:"COMMANDS") term

(** rpc list command - list available endpoints *)
let list_cmd =
  let doc = "List available RPC endpoints at a path" in
  let path_arg =
    Arg.(value & pos 0 string "/" & info [] ~doc:"RPC path" ~docv:"PATH")
  in
  let term =
    Term.(
      ret
        (const (fun instance_opt url_opt public_opt path ->
             match resolve_service instance_opt url_opt public_opt with
             | Error msg -> Cli_helpers.cmdliner_error msg
             | Ok service ->
                 let segs =
                   String.split_on_char '/' path
                   |> List.filter (fun s -> s <> "")
                 in
                 let entries, source =
                   Octez_manager_ui.Rpc_describe.fetch_entries service ~segs
                 in
                 let source_str =
                   match source with `Describe -> "describe" | `None -> "none"
                 in
                 Printf.printf
                   "Path: /%s (source: %s)\n\n"
                   (String.concat "/" segs)
                   source_str ;
                 if entries = [] then Printf.printf "  (no entries)\n"
                 else
                   List.iter
                     (fun (e : Octez_manager_ui.Rpc_describe.entry) ->
                       let kind_str =
                         match e.kind with
                         | Octez_manager_ui.Rpc_describe.Sub -> "[SUB]"
                         | Octez_manager_ui.Rpc_describe.Get -> "[GET]"
                         | Octez_manager_ui.Rpc_describe.Dyn typ ->
                             Printf.sprintf "[DYN:%s]" typ
                       in
                       Printf.printf "  %-40s %s\n" e.name kind_str)
                     entries ;
                 `Ok ())
        $ instance_arg $ url_arg $ public_arg $ path_arg))
  in
  Cmd.v (Cmd.info "list" ~doc) term

(** rpc instances command - list node instances *)
let instances_cmd =
  let doc = "List available node instances" in
  let term =
    Term.(
      const (fun () ->
          let instances = list_instances () in
          if instances = [] then Printf.printf "No node instances found.\n"
          else (
            Printf.printf "Available node instances:\n" ;
            List.iter (fun i -> Printf.printf "  %s\n" i) instances) ;
          `Ok ())
      $ const ())
  in
  Cmd.v (Cmd.info "instances" ~doc) (Term.ret term)

(** rpc public-nodes command - list public nodes from Taquito *)
let public_nodes_cmd =
  let doc = "List available public RPC nodes from Taquito" in
  let term =
    Term.(
      const (fun () ->
          let nodes = get_public_nodes () in
          if nodes = [] then Printf.printf "No public nodes available.\n"
          else (
            Printf.printf "Available public RPC nodes:\n" ;
            Printf.printf
              "  Use -p/--public with index or name (e.g., -p 1 or -p ecad)\n\n" ;
            List.iteri
              (fun i svc ->
                Printf.printf
                  "  %2d. %-30s  %s\n"
                  (i + 1)
                  svc.Service.instance
                  (Rpc_addr.to_string svc.Service.rpc_addr))
              nodes) ;
          `Ok ())
      $ const ())
  in
  Cmd.v (Cmd.info "public-nodes" ~doc) (Term.ret term)

(** Main rpc command group *)
let rpc_cmd =
  let doc = "Query RPC endpoints on node instances" in
  let info = Cmd.info "rpc" ~doc in
  Cmd.group
    info
    [get_cmd; list_cmd; interactive_cmd; instances_cmd; public_nodes_cmd]
