(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
module HP = Octez_manager_lib.Help_parser
module H = Gen_completion_helpers

let ( let* ) = Result.bind

let escape_zsh_single = H.escape_zsh_single

let escape_zsh_description = H.escape_zsh_description

(** A node in the command tree.  [name] is the argument word for this command
    (e.g. ["import"]), [doc] its short description, [options] the flags it
    accepts, and [children] its subcommands (empty for leaf commands). *)
type cmd_node = {
  name : string;
  doc : string;
  options : HP.option_entry list;
  children : cmd_node list;
}

let write_file path contents =
  try
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () -> output_string oc contents) ;
    Ok ()
  with Sys_error msg ->
    Error (`Msg ("Failed to write to file '" ^ path ^ "': " ^ msg))

let run_help binary args =
  let argv = ["env"; "MANPAGER=cat"; "PAGER=cat"; "TERM=dumb"; binary] @ args in
  match Cmd_runner.run_out argv with
  | Ok out -> Ok (HP.strip_ansi out)
  | Error (`Msg msg) -> Error (`Msg msg)

let resolve_binary explicit =
  match explicit with
  | Some path -> Ok path
  | None -> (
      if Sys.file_exists "./octez-manager" then Ok "./octez-manager"
      else
        match Paths.which "octez-manager" with
        | Some path -> Ok path
        | None -> Error (`Msg "octez-manager binary not found"))

let load_commands binary =
  let* help = run_help binary ["--help=plain"] in
  Ok (HP.parse_cmdliner_commands help)

let load_options binary args =
  let* help = run_help binary (args @ ["--help=plain"]) in
  Ok (HP.parse_cmdliner_options help)

let load_instance_actions binary =
  (* Run with an invalid action to get the error message listing valid actions.
     We use sh -c to redirect stderr to stdout so we can capture the error
     message. *)
  let placeholder = "__invalid_action_placeholder__" in
  let cmd =
    Printf.sprintf "%s instance _ %s 2>&1" (H.quote_shell binary) placeholder
  in
  let argv = ["sh"; "-c"; cmd] in
  match Cmd_runner.run_out argv with
  | Ok output | Error (`Msg output) ->
      let extract_actions s =
        match String.split_on_char '\'' s with
        | parts ->
            parts
            |> List.filteri (fun i _ -> i mod 2 = 1)
            |> List.filter (fun s ->
                s <> "" && s <> placeholder && not (String.contains s ' '))
      in
      Ok (extract_actions output)

let is_valid_cmd_name = H.is_valid_cmd_name

(** Discover the direct subcommands of [path] by running
    [binary path... --help=plain] and parsing the COMMANDS section. *)
let load_subcommands binary path =
  let* help = run_help binary (path @ ["--help=plain"]) in
  let subs = HP.parse_cmdliner_commands help in
  Ok
    (List.filter
       (fun (sub : HP.command_entry) -> is_valid_cmd_name sub.HP.name)
       subs)

let dedupe_options entries =
  let seen = Hashtbl.create 32 in
  let add acc entry =
    let key = String.concat "|" entry.HP.names in
    if Hashtbl.mem seen key then acc
    else (
      Hashtbl.add seen key () ;
      entry :: acc)
  in
  List.rev (List.fold_left add [] entries)

(** Recursively build the command tree rooted at [entry].  The [path]
    accumulates the argv words from the root to [entry]'s parent.  Discovery
    is done by invoking the binary at every level, so the tree correctly
    reflects the actual CLI hierarchy regardless of depth. *)
let rec build_cmd_tree binary path (entry : HP.command_entry) =
  let path' = path @ [entry.HP.name] in
  let* options_raw = load_options binary path' in
  let options = dedupe_options options_raw in
  let* raw_children = load_subcommands binary path' in
  let* children =
    List.fold_left
      (fun acc (sub : HP.command_entry) ->
        let* acc = acc in
        let* node = build_cmd_tree binary path' sub in
        Ok (acc @ [node]))
      (Ok [])
      raw_children
  in
  Ok {name = entry.HP.name; doc = entry.HP.doc; options; children}

(** Flatten all options across a subtree (used for bash kind-based
    completion). *)
let rec all_options node =
  node.options @ List.concat_map all_options node.children

(* ---- Variable naming ---------------------------------------------------- *)

let sanitize_var s = String.map (fun c -> if c = '-' then '_' else c) s

(** Zsh option array name for a command path.
    E.g. [opts_var {|["snapshot"; "import"]|}] = ["opts_snapshot_import"]. *)
let opts_var path = "opts_" ^ String.concat "_" (List.map sanitize_var path)

(** Bash option variable name for a command path.
    E.g. [bash_opts_var {|["snapshot"; "import"]|}] = ["snapshot_import_opts"].
*)
let bash_opts_var path =
  String.concat "_" (List.map sanitize_var path) ^ "_opts"

(* ---- Zsh rendering helpers ----------------------------------------------- *)

let zsh_option_specs (entry : HP.option_entry) =
  let doc =
    if entry.HP.doc = "" then ""
    else "[" ^ escape_zsh_description entry.HP.doc ^ "]"
  in
  match entry.HP.arg with
  | None -> List.map (fun name -> "'" ^ name ^ doc ^ "'") entry.HP.names
  | Some arg ->
      let primary = HP.primary_name entry.HP.names in
      let path_exclusions =
        [
          "--node-instance";
          "--endpoint";
          "--node-endpoint";
          "--dal-endpoint";
          "--rpc-addr";
          "--net-addr";
          "--address";
        ]
      in
      let action =
        if primary = "--history-mode" || primary = "--snapshot-history-mode"
        then "->history-modes"
        else if primary = "--snapshot-kind" then "->snapshot-kinds"
        else if primary = "--liquidity-baking-vote" then "->lb-votes"
        else if primary = "--service-user" then "_users"
        else if
          List.mem primary ["--endpoint"; "--node-endpoint"; "--dal-endpoint"]
        then "_urls"
        else if List.mem primary path_exclusions then ""
        else
          match entry.HP.kind with
          | HP.Value HP.Dir -> "_directories"
          | HP.Value HP.File | HP.Value HP.Path -> "_files"
          | _ -> ""
      in
      List.map
        (fun name ->
          "'" ^ name ^ doc ^ ":" ^ escape_zsh_single arg ^ ":" ^ action ^ "'")
        entry.HP.names

let render_zsh_list name entries =
  let buf = Buffer.create 256 in
  Buffer.add_string buf ("  local -a " ^ name ^ "\n") ;
  Buffer.add_string buf ("  " ^ name ^ "=(\n") ;
  List.iter
    (fun (value, doc) ->
      let entry =
        if doc = "" then "'" ^ escape_zsh_single value ^ "'"
        else "'" ^ escape_zsh_single value ^ ":" ^ escape_zsh_single doc ^ "'"
      in
      Buffer.add_string buf ("    " ^ entry ^ "\n"))
    entries ;
  Buffer.add_string buf "  )\n\n" ;
  Buffer.contents buf

let render_zsh_options name (options : HP.option_entry list) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf ("  local -a " ^ name ^ "\n") ;
  Buffer.add_string buf ("  " ^ name ^ "=(\n") ;
  List.iter
    (fun opt ->
      zsh_option_specs opt
      |> List.iter (fun spec -> Buffer.add_string buf ("    " ^ spec ^ "\n")))
    options ;
  Buffer.add_string buf "  )\n\n" ;
  Buffer.contents buf

let collect_kinds options =
  let path_exclusions =
    [
      "--node-instance";
      "--endpoint";
      "--node-endpoint";
      "--dal-endpoint";
      "--rpc-addr";
      "--net-addr";
      "--address";
    ]
  in
  let to_kind opt =
    let primary = HP.primary_name opt.HP.names in
    if primary = "--service-user" || List.mem primary path_exclusions then None
    else
      match opt.kind with
      | HP.Value HP.Dir -> Some (primary, `Dir)
      | HP.Value HP.File | HP.Value HP.Path -> Some (primary, `File)
      | _ -> None
  in
  options |> List.filter_map to_kind

(* ---- Zsh renderer -------------------------------------------------------- *)

(** Generate the zsh completion script.

    The completion is structured as a recursive case dispatch:
    - [case $words[1] in] dispatches on the top-level command
    - Inside any command arm that has subcommands, [case $words[N] in] at the
      appropriate depth dispatches on the subcommand at that level
    - Leaf commands (no subcommands) fall through to [_arguments] with their
      flags

    The [instance] command is special-cased because its second word is a
    runtime-dynamic instance name (not a static subcommand) and its third word
    is an action. *)
let render_zsh ~roots ~instance_actions =
  (* [sp n] returns a string of [n * 2] spaces used for indentation. *)
  let sp n = String.make (n * 2) ' ' in
  let buf = Buffer.create 8192 in
  Buffer.add_string buf "#compdef octez-manager\n\n" ;
  Buffer.add_string
    buf
    "# Autogenerated by tools/gen_completion.ml. Do not edit.\n\n" ;
  Buffer.add_string buf "_octez-manager() {\n" ;
  (* static command and action lists *)
  let commands = List.map (fun n -> (n.name, n.doc)) roots in
  Buffer.add_string buf (render_zsh_list "commands" commands) ;
  Buffer.add_string buf (render_zsh_list "instance_actions" instance_actions) ;
  Buffer.add_string buf "  local -a history_modes\n" ;
  Buffer.add_string buf "  history_modes=(\n" ;
  Buffer.add_string buf "    'archive:Full archive mode'\n" ;
  Buffer.add_string buf "    'full:Full mode'\n" ;
  Buffer.add_string buf "    'rolling:Rolling mode'\n" ;
  Buffer.add_string buf "  )\n\n" ;
  Buffer.add_string buf "  local -a snapshot_kinds\n" ;
  Buffer.add_string buf "  snapshot_kinds=(\n" ;
  Buffer.add_string buf "    'rolling:Rolling snapshot'\n" ;
  Buffer.add_string buf "    'full:Full snapshot'\n" ;
  Buffer.add_string buf "    'full\\:50:Full snapshot with 50 cycles'\n" ;
  Buffer.add_string buf "    'archive:Archive snapshot'\n" ;
  Buffer.add_string buf "  )\n\n" ;
  Buffer.add_string buf "  local -a lb_votes\n" ;
  Buffer.add_string buf "  lb_votes=(\n" ;
  Buffer.add_string buf "    'on:Vote for liquidity baking'\n" ;
  Buffer.add_string buf "    'off:Vote against liquidity baking'\n" ;
  Buffer.add_string buf "    'pass:Abstain from voting (default)'\n" ;
  Buffer.add_string buf "  )\n\n" ;
  (* declare one option array per tree node *)
  let rec declare_opts path node =
    Buffer.add_string buf (render_zsh_options (opts_var path) node.options) ;
    List.iter
      (fun child -> declare_opts (path @ [child.name]) child)
      node.children
  in
  List.iter (fun node -> declare_opts [node.name] node) roots ;
  (* main dispatch via _arguments -C *)
  Buffer.add_string buf "  _arguments -C \\\n" ;
  Buffer.add_string buf "    '1: :->command' \\\n" ;
  Buffer.add_string buf "    '*:: :->args'\n\n" ;
  Buffer.add_string buf "  case $state in\n" ;
  Buffer.add_string buf "    command)\n" ;
  Buffer.add_string
    buf
    "      _describe -t commands 'octez-manager commands' commands\n" ;
  Buffer.add_string buf "      ;;\n" ;
  Buffer.add_string buf "    args)\n" ;
  Buffer.add_string buf "      case $words[1] in\n" ;
  (* instance arm: dynamic instance-name lookup, then action, then flags *)
  Buffer.add_string buf "        instance)\n" ;
  Buffer.add_string buf "          if (( CURRENT == 2 )); then\n" ;
  Buffer.add_string
    buf
    "            local -a instances\n\
    \            instances=()\n\
    \            local cmd=\"\"\n\
    \            if [[ -x ./octez-manager ]]; then\n\
    \              cmd=./octez-manager\n\
    \            elif [[ -x _build/bin/octez-manager ]]; then\n\
    \              cmd=_build/bin/octez-manager\n\
    \            elif command -v octez-manager >/dev/null 2>&1; then\n\
    \              cmd=octez-manager\n\
    \            fi\n\
    \            if [[ -n $cmd ]]; then\n\
    \              local out\n\
    \              out=$($cmd list 2>/dev/null)\n\
    \              if [[ -n $out ]]; then\n\
    \                while IFS= read -r line; do\n\
    \                  if [[ $line == *'('* ]]; then\n\
    \                    local inst=${line%%[[:space:]]*}\n\
    \                    instances+=(\"$inst\")\n\
    \                  fi\n\
    \                done <<<\"$out\"\n\
    \              fi\n\
    \            fi\n\
    \            if (( ${#instances} )); then\n\
    \              typeset -U instances\n\
    \              _describe -t instances 'instances' instances\n\
    \            else\n\
    \              _message 'instance name'\n\
    \            fi\n" ;
  Buffer.add_string buf "          elif (( CURRENT == 3 )); then\n" ;
  Buffer.add_string
    buf
    "            _describe -t actions 'instance actions' instance_actions\n" ;
  Buffer.add_string buf "          else\n" ;
  Buffer.add_string buf "            _arguments \\\n" ;
  Buffer.add_string buf "              $opts_instance\n" ;
  Buffer.add_string buf "          fi\n" ;
  Buffer.add_string buf "          ;;\n" ;
  (* recursive emission for all other commands.
     [depth] is 0-indexed depth in the CLI tree (top-level commands = 0).
     [path] is the full argv path to the node, used for variable names.
     Indentation:
       arm header  = sp (4 + depth * 3)
       arm content = sp (4 + depth * 3 + 1)
     Word-index for next-level dispatch = depth + 2 (zsh $words is 1-indexed;
     inside _arguments -C '*:: :->args', $words[1] is the current command). *)
  let rec emit_node depth path node =
    if node.name <> "instance" then begin
      let arm_ind = 4 + (depth * 3) in
      let next_word = depth + 2 in
      Printf.bprintf buf "%s%s)\n" (sp arm_ind) node.name ;
      if node.children = [] then begin
        Printf.bprintf buf "%s_arguments \\\n" (sp (arm_ind + 1)) ;
        Printf.bprintf buf "%s$%s\n" (sp (arm_ind + 2)) (opts_var path)
      end
      else begin
        let subcmd_arr =
          "subcmds_" ^ String.concat "_" (List.map sanitize_var path)
        in
        Printf.bprintf buf "%slocal -a %s\n" (sp (arm_ind + 1)) subcmd_arr ;
        Printf.bprintf buf "%s%s=(\n" (sp (arm_ind + 1)) subcmd_arr ;
        List.iter
          (fun child ->
            let entry =
              if child.doc = "" then "'" ^ escape_zsh_single child.name ^ "'"
              else
                "'"
                ^ escape_zsh_single child.name
                ^ ":"
                ^ escape_zsh_description child.doc
                ^ "'"
            in
            Printf.bprintf buf "%s%s\n" (sp (arm_ind + 2)) entry)
          node.children ;
        Printf.bprintf buf "%s)\n" (sp (arm_ind + 1)) ;
        Printf.bprintf
          buf
          "%sif (( CURRENT == %d )); then\n"
          (sp (arm_ind + 1))
          next_word ;
        Printf.bprintf buf "%sif [[ $cur == -* ]]; then\n" (sp (arm_ind + 2)) ;
        Printf.bprintf buf "%s_arguments \\\n" (sp (arm_ind + 3)) ;
        Printf.bprintf buf "%s$%s\n" (sp (arm_ind + 4)) (opts_var path) ;
        Printf.bprintf buf "%selse\n" (sp (arm_ind + 2)) ;
        Printf.bprintf
          buf
          "%s_describe -t subcommands '%s subcommands' %s\n"
          (sp (arm_ind + 3))
          node.name
          subcmd_arr ;
        Printf.bprintf buf "%sfi\n" (sp (arm_ind + 2)) ;
        Printf.bprintf buf "%selse\n" (sp (arm_ind + 1)) ;
        Printf.bprintf buf "%scase $words[%d] in\n" (sp (arm_ind + 2)) next_word ;
        List.iter
          (fun child -> emit_node (depth + 1) (path @ [child.name]) child)
          node.children ;
        Printf.bprintf buf "%sesac\n" (sp (arm_ind + 2)) ;
        Printf.bprintf buf "%sfi\n" (sp (arm_ind + 1))
      end ;
      Printf.bprintf buf "%s;;\n" (sp (arm_ind + 1))
    end
  in
  List.iter (fun node -> emit_node 0 [node.name] node) roots ;
  Buffer.add_string buf "      esac\n" ;
  (* State handlers for enum values: nested _arguments calls (inside the arms
     above) use ->state-name actions.  When such an action fires, _arguments
     sets $state and returns.  We handle the resulting state here, still inside
     the args) arm, so that the arrays declared at the top of the function are
     in scope. *)
  Buffer.add_string buf "      case $state in\n" ;
  Buffer.add_string buf "        history-modes)\n" ;
  Buffer.add_string
    buf
    "          _describe -t history-modes 'history modes' history_modes\n" ;
  Buffer.add_string buf "          ;;\n" ;
  Buffer.add_string buf "        snapshot-kinds)\n" ;
  Buffer.add_string
    buf
    "          _describe -t snapshot-kinds 'snapshot kinds' snapshot_kinds\n" ;
  Buffer.add_string buf "          ;;\n" ;
  Buffer.add_string buf "        lb-votes)\n" ;
  Buffer.add_string
    buf
    "          _describe -t lb-votes 'liquidity baking votes' lb_votes\n" ;
  Buffer.add_string buf "          ;;\n" ;
  Buffer.add_string buf "      esac\n" ;
  Buffer.add_string buf "      ;;\n" ;
  Buffer.add_string buf "  esac\n" ;
  Buffer.add_string buf "}\n\n" ;
  Buffer.add_string buf "if [[ -n $ZSH_VERSION ]]; then\n" ;
  Buffer.add_string buf "  compdef _octez-manager octez-manager\n" ;
  Buffer.add_string buf "fi\n" ;
  Buffer.contents buf

(* ---- Bash renderer ------------------------------------------------------- *)

(** Generate the bash completion script.

    Like the zsh renderer, this walks the [cmd_node] tree recursively.  Bash
    uses absolute word positions ([COMP_WORDS] is 0-indexed, [COMP_CWORD] is
    the cursor position), so the word index at depth D is [D + 2] (position 1
    is the top-level command, position 2 is the first subcommand, etc.).

    The [instance] command is special-cased identically to the zsh renderer. *)
let render_bash ~roots ~instance_actions ~kinds =
  let unique_list items =
    let seen = Hashtbl.create 32 in
    let add acc item =
      if Hashtbl.mem seen item then acc
      else (
        Hashtbl.add seen item () ;
        item :: acc)
    in
    List.rev (List.fold_left add [] items)
  in
  let sp n = String.make (n * 2) ' ' in
  let buf = Buffer.create 8192 in
  Buffer.add_string buf "# Bash completion for octez-manager\n" ;
  Buffer.add_string
    buf
    "# Autogenerated by tools/gen_completion.ml. Do not edit.\n\n" ;
  Buffer.add_string buf "_octez_manager_list_instances() {\n" ;
  Buffer.add_string
    buf
    "  local cmd=\"\"\n\
    \  if [[ -x ./octez-manager ]]; then\n\
    \    cmd=./octez-manager\n\
    \  elif [[ -x _build/bin/octez-manager ]]; then\n\
    \    cmd=_build/bin/octez-manager\n\
    \  elif command -v octez-manager >/dev/null 2>&1; then\n\
    \    cmd=octez-manager\n\
    \  fi\n\n\
    \  if [[ -n $cmd ]]; then\n\
    \    local line\n\
    \    while IFS= read -r line; do\n\
    \      if [[ $line == *\"(\"* ]]; then\n\
    \        printf '%s\\n' \"${line%%[[:space:]]*}\"\n\
    \      fi\n\
    \    done < <(\"$cmd\" list 2>/dev/null)\n\
    \  fi\n\
     }\n\n" ;
  Buffer.add_string buf "_octez_manager() {\n" ;
  Buffer.add_string buf "  local cur prev cmd action opts\n" ;
  Buffer.add_string buf "  COMPREPLY=()\n" ;
  Buffer.add_string buf "  cur=\"${COMP_WORDS[COMP_CWORD]}\"\n" ;
  Buffer.add_string buf "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n\n" ;
  let cmd_names = List.map (fun n -> n.name) roots in
  Printf.bprintf buf "  local commands=\"%s\"\n" (String.concat " " cmd_names) ;
  Printf.bprintf
    buf
    "  local instance_actions=\"%s\"\n"
    (String.concat " " instance_actions) ;
  Buffer.add_string buf "  local history_modes=\"archive full rolling\"\n" ;
  Buffer.add_string
    buf
    "  local snapshot_kinds=\"rolling full full:50 archive\"\n" ;
  Buffer.add_string buf "  local lb_votes=\"on off pass\"\n" ;
  (* declare one option variable per tree node *)
  let rec declare_opts path node =
    let var = bash_opts_var path in
    let names = List.concat_map (fun o -> o.HP.names) node.options in
    Printf.bprintf buf "  local %s=\"%s\"\n" var (String.concat " " names) ;
    List.iter
      (fun child -> declare_opts (path @ [child.name]) child)
      node.children
  in
  List.iter (fun node -> declare_opts [node.name] node) roots ;
  Buffer.add_string buf "\n" ;
  (* prev-word completions for URLs, enum flags, file/dir arguments *)
  Buffer.add_string
    buf
    "  if [[ $prev == --endpoint || $prev == --node-endpoint || $prev == \
     --dal-endpoint ]]; then\n\
    \    if declare -F _urls >/dev/null; then\n\
    \      _urls\n\
    \    else\n\
    \      COMPREPLY=( $(compgen -W \"http:// https://\" -- \"$cur\") )\n\
    \    fi\n\
    \    return 0\n\
    \  fi\n\n" ;
  Buffer.add_string buf "  case \"$prev\" in\n" ;
  Buffer.add_string
    buf
    "    --history-mode|--snapshot-history-mode)\n\
    \      COMPREPLY=( $(compgen -W \"$history_modes\" -- \"$cur\") )\n\
    \      return 0\n\
    \      ;;\n" ;
  Buffer.add_string
    buf
    "    --snapshot-kind)\n\
    \      COMPREPLY=( $(compgen -W \"$snapshot_kinds\" -- \"$cur\") )\n\
    \      return 0\n\
    \      ;;\n" ;
  Buffer.add_string
    buf
    "    --liquidity-baking-vote)\n\
    \      COMPREPLY=( $(compgen -W \"$lb_votes\" -- \"$cur\") )\n\
    \      return 0\n\
    \      ;;\n" ;
  Buffer.add_string
    buf
    "    --service-user)\n\
    \      COMPREPLY=( $(compgen -A user -- \"$cur\") )\n\
    \      return 0\n\
    \      ;;\n" ;
  let add_case pattern action =
    if pattern = "" then ()
    else
      Buffer.add_string
        buf
        ("    " ^ pattern ^ ")\n      COMPREPLY=( $(" ^ action
       ^ " -- \"$cur\") )\n      return 0\n      ;;\n")
  in
  let dir_opts =
    List.filter (fun (_, kind) -> kind = `Dir) kinds |> List.map fst
  in
  let file_opts =
    List.filter (fun (_, kind) -> kind = `File) kinds |> List.map fst
  in
  let builtin_dir_opts =
    ["--app-bin-dir"; "--data-dir"; "--base-dir"; "--node-data-dir"]
  in
  let builtin_file_opts = ["--log-file"; "--ui-logfile"; "--snapshot-uri"] in
  add_case (String.concat "|" builtin_dir_opts) "compgen -d" ;
  add_case (String.concat "|" builtin_file_opts) "compgen -f" ;
  let dir_opts =
    dir_opts
    |> List.filter (fun opt -> not (List.mem opt builtin_dir_opts))
    |> unique_list
  in
  let file_opts =
    file_opts
    |> List.filter (fun opt -> not (List.mem opt builtin_file_opts))
    |> unique_list
  in
  add_case (String.concat "|" dir_opts) "compgen -d" ;
  add_case (String.concat "|" file_opts) "compgen -f" ;
  Buffer.add_string buf "  esac\n\n" ;
  (* top-level command dispatch *)
  Buffer.add_string buf "  if [[ $COMP_CWORD -eq 1 ]]; then\n" ;
  Buffer.add_string
    buf
    "    COMPREPLY=( $(compgen -W \"$commands --help\" -- \"$cur\") )\n" ;
  Buffer.add_string buf "    return 0\n  fi\n\n" ;
  Buffer.add_string buf "  cmd=\"${COMP_WORDS[1]}\"\n" ;
  Buffer.add_string buf "  if [[ $cmd == -* ]]; then\n" ;
  Buffer.add_string
    buf
    "    COMPREPLY=( $(compgen -W \"$commands --help\" -- \"$cur\") )\n" ;
  Buffer.add_string buf "    return 0\n  fi\n\n" ;
  Buffer.add_string buf "  case \"$cmd\" in\n" ;
  (* instance arm: dynamic instance name, then action, then flags *)
  Buffer.add_string buf "    instance)\n" ;
  Buffer.add_string buf "      if [[ $COMP_CWORD -eq 2 ]]; then\n" ;
  Buffer.add_string
    buf
    "        local instances\n\
    \        instances=\"$(_octez_manager_list_instances)\"\n\
    \        if [[ -n $instances ]]; then\n\
    \          COMPREPLY=( $(compgen -W \"$instances\" -- \"$cur\") )\n\
    \        fi\n\
    \        return 0\n\
    \      fi\n" ;
  Buffer.add_string buf "      if [[ $COMP_CWORD -eq 3 ]]; then\n" ;
  Buffer.add_string
    buf
    "        COMPREPLY=( $(compgen -W \"$instance_actions --help\" -- \
     \"$cur\") )\n\
    \        return 0\n\
    \      fi\n" ;
  Buffer.add_string buf "      if [[ $cur == -* ]]; then\n" ;
  Buffer.add_string
    buf
    "        COMPREPLY=( $(compgen -W \"$instance_opts\" -- \"$cur\") )\n" ;
  Buffer.add_string buf "      fi\n" ;
  Buffer.add_string buf "      return 0\n" ;
  Buffer.add_string buf "      ;;\n" ;
  (* recursive emission for all other commands.
     [depth] is 0-indexed depth in the CLI tree.
     [path] is the full argv path, used for variable names.
     Indentation:
       arm header  = sp (2 + depth * 3)
       arm content = sp (2 + depth * 3 + 1)
     Word position for next-level dispatch = depth + 2 (COMP_WORDS[0] is the
     binary name; COMP_WORDS[1] is the top-level command; COMP_CWORD counts
     from 0).
     Only depth-0 arms emit [return 0]; nested arms fall through to the
     parent arm's [return 0]. *)
  let rec emit_node depth path node =
    if node.name <> "instance" then begin
      let arm_ind = 2 + (depth * 3) in
      let cword = depth + 2 in
      Printf.bprintf buf "%s%s)\n" (sp arm_ind) node.name ;
      if node.children = [] then begin
        Printf.bprintf buf "%sif [[ $cur == -* ]]; then\n" (sp (arm_ind + 1)) ;
        Printf.bprintf
          buf
          "%sCOMPREPLY=( $(compgen -W \"$%s\" -- \"$cur\") )\n"
          (sp (arm_ind + 2))
          (bash_opts_var path) ;
        Printf.bprintf buf "%sfi\n" (sp (arm_ind + 1))
      end
      else begin
        Printf.bprintf
          buf
          "%sif [[ $COMP_CWORD -eq %d ]]; then\n"
          (sp (arm_ind + 1))
          cword ;
        Printf.bprintf buf "%sif [[ $cur == -* ]]; then\n" (sp (arm_ind + 2)) ;
        Printf.bprintf
          buf
          "%sCOMPREPLY=( $(compgen -W \"$%s\" -- \"$cur\") )\n"
          (sp (arm_ind + 3))
          (bash_opts_var path) ;
        Printf.bprintf buf "%selse\n" (sp (arm_ind + 2)) ;
        let subcmd_names =
          String.concat " " (List.map (fun c -> c.name) node.children)
        in
        Printf.bprintf
          buf
          "%sCOMPREPLY=( $(compgen -W \"%s\" -- \"$cur\") )\n"
          (sp (arm_ind + 3))
          subcmd_names ;
        Printf.bprintf buf "%sfi\n" (sp (arm_ind + 2)) ;
        Printf.bprintf buf "%selse\n" (sp (arm_ind + 1)) ;
        Printf.bprintf
          buf
          "%slocal sub%d=\"${COMP_WORDS[%d]}\"\n"
          (sp (arm_ind + 2))
          depth
          cword ;
        Printf.bprintf buf "%scase \"$sub%d\" in\n" (sp (arm_ind + 2)) depth ;
        List.iter
          (fun child -> emit_node (depth + 1) (path @ [child.name]) child)
          node.children ;
        Printf.bprintf buf "%sesac\n" (sp (arm_ind + 2)) ;
        Printf.bprintf buf "%sfi\n" (sp (arm_ind + 1))
      end ;
      if depth = 0 then Printf.bprintf buf "%sreturn 0\n" (sp (arm_ind + 1)) ;
      Printf.bprintf buf "%s;;\n" (sp (arm_ind + 1))
    end
  in
  List.iter (fun node -> emit_node 0 [node.name] node) roots ;
  Buffer.add_string buf "  esac\n}\n\n" ;
  Buffer.add_string buf "complete -F _octez_manager octez-manager\n" ;
  Buffer.contents buf

(* ---- Entry point --------------------------------------------------------- *)

let () =
  let binary_arg = ref None in
  let out_dir = ref "completions" in
  let usage = "gen_completion [--binary PATH] [--out-dir DIR]" in
  Arg.parse
    [
      ("--binary", Arg.String (fun s -> binary_arg := Some s), "Binary path");
      ("--out-dir", Arg.Set_string out_dir, "Output directory");
    ]
    (fun _ -> ())
    usage ;
  let result =
    let* binary = resolve_binary !binary_arg in
    let* cmds = load_commands binary in
    let* roots =
      List.fold_left
        (fun acc (cmd : HP.command_entry) ->
          let* acc = acc in
          let* node = build_cmd_tree binary [] cmd in
          Ok (acc @ [node]))
        (Ok [])
        cmds
    in
    let* action_names = load_instance_actions binary in
    let instance_actions = List.map (fun name -> (name, "")) action_names in
    let zsh = render_zsh ~roots ~instance_actions in
    let all_opts = List.concat_map all_options roots in
    let kinds = collect_kinds all_opts in
    let bash =
      render_bash
        ~roots
        ~instance_actions:(List.map fst instance_actions)
        ~kinds
    in
    let zsh_path = Filename.concat !out_dir "octez-manager.zsh" in
    let bash_path = Filename.concat !out_dir "octez-manager.bash" in
    let* () = write_file zsh_path zsh in
    let* () = write_file bash_path bash in
    Ok ()
  in
  match result with
  | Ok () -> ()
  | Error (`Msg msg) ->
      prerr_endline ("gen_completion: " ^ msg) ;
      exit 1
