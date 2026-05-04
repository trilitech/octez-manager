(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox management page — list sandboxes, topology view, actions.

    LAYOUT RULE: All layouts use Flex_layout / Grid_layout / Box_widget.
    No manual string alignment, no Pane_layout. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module T = Themed_text
open Octez_manager_lib
open Rresult

let ( let* ) = Result.bind

let name = "sandbox"

(* ─── State (types re-exported from Sandbox_view) ───────────────────────── *)

(** Re-use the type definitions from Sandbox_view to avoid duplication. *)
type node_info = Sandbox_view.node_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  peers : string list;  (** Configured --peer addresses from extra_args *)
}

type baker_info = Sandbox_view.baker_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  delegate_count : int;
  baker_ck_aliases : string list;
      (** Consensus-key aliases from OCTEZ_BAKER_DELEGATES_CSV. *)
}

type accuser_info = Sandbox_view.accuser_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
}

type sandbox_info = Sandbox_view.sandbox_info = {
  group : Group.t;
  nodes : node_info list;
  bakers : baker_info list;
  accusers : accuser_info list;
}

type state = Sandbox_view.state = {sandboxes : sandbox_info list; cursor : int}

type msg = unit

type pstate = state Navigation.t

(* ─── Async stake% cache ────────────────────────────────────────────────── *)

(** Stake fetch uses two tables.
    [stake_fetching] prevents duplicate in-flight fetches.
    [stake_results] is the cached result; it is read directly by [view] via
    [get_stake_pct] on every render frame, so no dirty-flag signal is needed
    when a new result arrives.  The cache is only cleared when the baker
    allocation changes (detected by comparing aliases before/after a reload). *)
let stake_fetching : (string, unit) Hashtbl.t = Hashtbl.create 4

let stake_results : (string, float) Hashtbl.t = Hashtbl.create 4

let stake_lock = Mutex.create ()

let get_stake_pct group_name =
  Mutex.protect stake_lock (fun () -> Hashtbl.find_opt stake_results group_name)

(** Map a consensus-key alias [delegate-(3n+1)] to its base alias [delegate-(3n)].
    Returns [None] for non-CK aliases or unrecognised formats. *)
let ck_alias_to_base_alias alias =
  match String.split_on_char '-' alias with
  | ["delegate"; ns] -> (
      match int_of_string_opt ns with
      | Some n when n mod 3 = 1 -> Some (Printf.sprintf "delegate-%d" (n - 1))
      | _ -> None)
  | _ -> None

let schedule_stake_fetch ~group_name ~endpoint ~wallet_dir ~baker_ck_aliases =
  let should_fetch =
    Mutex.protect stake_lock (fun () ->
        if
          Hashtbl.mem stake_fetching group_name
          || Hashtbl.mem stake_results group_name
        then false
        else (
          Hashtbl.replace stake_fetching group_name () ;
          true))
  in
  if should_fetch then
    Background_runner.enqueue (fun () ->
        (* Compute baker-controlled base-delegate addresses from wallet. *)
        let only_addrs =
          match Yes_wallet_io.read_wallet_pkhs ~wallet_dir with
          | Error _ -> []
          | Ok pkhs ->
              let pkh_map = Hashtbl.create 64 in
              List.iter
                (fun (alias, addr) -> Hashtbl.replace pkh_map alias addr)
                pkhs ;
              baker_ck_aliases
              |> List.filter_map ck_alias_to_base_alias
              |> List.filter_map (Hashtbl.find_opt pkh_map)
              |> List.sort_uniq String.compare
        in
        let result =
          Yes_wallet_io.fetch_stake_pct ~endpoint ~only_addrs ~wallet_dir ()
        in
        (* The view reads stake% directly from get_stake_pct on every frame,
           so no dirty signal is needed — just update the cache. *)
        Mutex.protect stake_lock (fun () ->
            Hashtbl.remove stake_fetching group_name ;
            match result with
            | Ok pct -> Hashtbl.replace stake_results group_name pct
            | Error _ -> ()))

(* ─── Data Loading ──────────────────────────────────────────────────────── *)

let parse_peers (svc : Service.t) =
  let rec loop = function
    | "--peer" :: addr :: rest -> addr :: loop rest
    | _ :: rest -> loop rest
    | [] -> []
  in
  loop svc.extra_args

let load_baker_delegate_info instance =
  match Node_env.read ~inst:instance with
  | Error _ -> (0, [])
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_BAKER_DELEGATES_CSV" pairs with
      | None | Some "" -> (0, [])
      | Some csv ->
          let aliases =
            csv |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (fun s -> not (String.equal s ""))
          in
          (List.length aliases, aliases))

let load_sandboxes () =
  let service_states = Data.load_service_states () in
  let find_state instance =
    List.find_opt
      (fun (st : Data.Service_state.t) ->
        String.equal st.service.Service.instance instance)
      service_states
  in
  match Group_registry.list_sandboxes () with
  | Error _ -> []
  | Ok groups ->
      List.map
        (fun (g : Group.t) ->
          let services =
            match Lifecycle.group_services ~group_name:g.name () with
            | Ok l -> l
            | Error _ -> []
          in
          let nodes =
            services
            |> List.filter (fun (s : Service.t) -> String.equal s.role "node")
            |> List.map (fun svc ->
                {svc; state = find_state svc.instance; peers = parse_peers svc})
          in
          let bakers =
            services
            |> List.filter (fun (s : Service.t) -> String.equal s.role "baker")
            |> List.map (fun (svc : Service.t) ->
                let delegate_count, baker_ck_aliases =
                  load_baker_delegate_info svc.instance
                in
                {
                  svc;
                  state = find_state svc.instance;
                  delegate_count;
                  baker_ck_aliases;
                })
          in
          let accusers =
            services
            |> List.filter (fun (s : Service.t) ->
                String.equal s.role "accuser")
            |> List.map (fun svc -> {svc; state = find_state svc.instance})
          in
          {group = g; nodes; bakers; accusers})
        groups

let clamp_cursor sandboxes cursor =
  (* +1 for the synthetic "New sandbox" item always at position 0 *)
  let n = 1 + List.length sandboxes in
  max 0 (min cursor (n - 1))

(** Returns the sandbox at the cursor, or [None] when cursor is on the
    "New sandbox" create-item (position 0). *)
let selected_sandbox s =
  if s.cursor = 0 then None else List.nth_opt s.sandboxes (s.cursor - 1)

(* ─── Init / Lifecycle ──────────────────────────────────────────────────── *)

let init () =
  let sandboxes = load_sandboxes () in
  Navigation.make {sandboxes; cursor = 0}

let update ps _ = ps

let maybe_schedule_stake_fetch s =
  match selected_sandbox s with
  | None -> ()
  | Some sb -> (
      match sb.nodes with
      | [] -> ()
      | first :: _ ->
          let endpoint = Rpc_addr.to_endpoint first.svc.rpc_addr in
          let wallet = Sandbox.wallet_dir ~sandbox_name:sb.group.name in
          let baker_ck_aliases =
            List.concat_map (fun bi -> bi.baker_ck_aliases) sb.bakers
          in
          schedule_stake_fetch
            ~group_name:sb.group.name
            ~endpoint
            ~wallet_dir:wallet
            ~baker_ck_aliases)

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      let ps' =
        if Context.consume_instances_dirty () then begin
          let sandboxes = load_sandboxes () in
          let cursor = clamp_cursor sandboxes ps.Navigation.s.cursor in
          (* Only invalidate stake cache for sandboxes whose baker allocation
             changed (avoids re-fetching on unrelated dirty signals). *)
          let old_sandboxes = ps.Navigation.s.sandboxes in
          let baker_aliases (sb : sandbox_info) =
            List.concat_map (fun bi -> bi.baker_ck_aliases) sb.bakers
            |> List.sort String.compare
          in
          Mutex.protect stake_lock (fun () ->
              List.iter
                (fun (sb : sandbox_info) ->
                  let old_aliases =
                    match
                      List.find_opt
                        (fun (o : sandbox_info) ->
                          String.equal o.group.name sb.group.name)
                        old_sandboxes
                    with
                    | Some o -> baker_aliases o
                    | None -> []
                  in
                  if
                    not (List.equal String.equal old_aliases (baker_aliases sb))
                  then (
                    Hashtbl.remove stake_results sb.group.name ;
                    Hashtbl.remove stake_fetching sb.group.name))
                sandboxes) ;
          Navigation.update (fun _s -> {sandboxes; cursor}) ps
        end
        else ps
      in
      maybe_schedule_stake_fetch ps'.Navigation.s ;
      ps'

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps =
  Context.set_pending_tab Context.Tab_instances ;
  Navigation.back ps

(* ─── Page Layout ───────────────────────────────────────────────────────── *)

(** key_hint_pairs re-exported from Sandbox_view for use in keymap/key_hints *)
let key_hint_pairs = Sandbox_view.key_hint_pairs

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  Context.tick_spinner () ;
  Context.tick_toasts () ;
  let cols = size.LTerm_geom.cols in
  let toast = Context.render_toasts ~cols in
  (* Pre-compute node metrics (cache reads — not Eio) for the view layer. *)
  let node_metrics =
    List.concat_map
      (fun (sb : sandbox_info) ->
        List.map
          (fun (ni : node_info) ->
            (ni.svc.instance, Rpc_metrics.get ~instance:ni.svc.instance))
          sb.nodes)
      s.sandboxes
  in
  let stake_pct_for group_name = get_stake_pct group_name in
  Sandbox_view.view s ~toast ~node_metrics ~stake_pct_for ~focus ~size

(* ─── Actions ───────────────────────────────────────────────────────────── *)

let run_background desc f =
  Context.toast_info (T.text "%s..." desc) ;
  Background_runner.submit_blocking
    ~on_complete:(fun () -> Context.mark_instances_dirty ())
    (fun () ->
      match f () with
      | Ok () -> Context.toast_success (T.text "%s: done" desc)
      | Error (`Msg msg) ->
          Context.toast_error (T.text "%s failed: %s" desc msg))

type action =
  | Start
  | Stop
  | Destroy
  | Open_rpc
  | Add_account
  | Add_node
  | Add_baker
  | Reallocate_stake

let action_to_string = function
  | Start -> "Start"
  | Stop -> "Stop"
  | Destroy -> "Destroy"
  | Open_rpc -> "Open RPC Browser"
  | Add_account -> "Add Account"
  | Add_node -> "Add Node"
  | Add_baker -> "Add Baker"
  | Reallocate_stake -> "Reallocate Stake"

let do_start ps group_name =
  run_background (Printf.sprintf "Starting %s" group_name) (fun () ->
      let* _started = Lifecycle.start_group ~quiet:true ~group_name () in
      Ok ()) ;
  ps

let do_stop ps group_name =
  run_background (Printf.sprintf "Stopping %s" group_name) (fun () ->
      let* _stopped = Lifecycle.stop_group ~quiet:true ~group_name () in
      Ok ()) ;
  ps

let do_destroy ps group_name =
  let confirm () =
    run_background (Printf.sprintf "Destroying %s" group_name) (fun () ->
        Sandbox.destroy ~on_log:(fun _ -> ()) ~group_name ())
  in
  Modal_helpers.confirm_modal
    ~title:(Printf.sprintf "Destroy Sandbox '%s'?" group_name)
    ~message:
      (Printf.sprintf
         "Permanently delete sandbox '%s'?\n\
          This removes all services, wallet files, and data and cannot be \
          undone."
         group_name)
    ~on_result:(fun yes -> if yes then confirm ())
    () ;
  ps

let do_open_rpc ps (sb : sandbox_info) =
  (match sb.nodes with
  | svc :: _ ->
      Context.set_pending_instance_detail svc.svc.instance ;
      Context.navigate "rpc-browser"
  | [] -> Context.toast_warn "No node found for this sandbox") ;
  ps

let do_add_account ps group_name =
  let wallet = Sandbox.wallet_dir ~sandbox_name:group_name in
  Modal_helpers.prompt_text_modal
    ~title:"Add Account"
    ~placeholder:(Some "tz1... or tz2... or tz3... or tz4...")
    ~initial:""
    ~on_submit:(fun address ->
      let address = String.trim address in
      match Yes_wallet.curve_of_address address with
      | None ->
          Context.toast_error
            (T.text "Invalid address: must start with tz1/tz2/tz3/tz4")
      | Some _ ->
          run_background
            (Printf.sprintf "Adding account to %s" group_name)
            (fun () ->
              Result.map
                ignore
                (Yes_wallet_io.add_account ~wallet_dir:wallet ~address ())))
    () ;
  ps

let do_add_node ps group_name =
  let desc = Printf.sprintf "Adding node to %s" group_name in
  Context.toast_info (T.text "%s..." desc) ;
  Job_manager.submit
    ~timeout:None
    ~description:desc
    ~on_complete:(fun _ -> Context.mark_instances_dirty ())
    (fun ~append_log () ->
      Sandbox.add_node
        ~on_log:(fun msg -> append_log (msg ^ "\n"))
        ~group_name
        ()
      |> Result.map ignore) ;
  ps

let do_add_baker ps group_name =
  Context.set_pending_sandbox_group group_name ;
  Context.navigate "sandbox-key-alloc" ;
  ps

let open_action_modal ps (sb : sandbox_info) =
  let group_name = sb.group.name in
  let has_node = not (List.is_empty sb.nodes) in
  let has_bakers = not (List.is_empty sb.bakers) in
  let items =
    [Start; Stop]
    @ (if has_node then [Open_rpc] else [])
    @ [Add_account; Add_node; Add_baker]
    @ (if has_bakers then [Reallocate_stake] else [])
    @ [Destroy]
  in
  Modal_helpers.open_choice_modal
    ~title:(Printf.sprintf "Actions · %s" group_name)
    ~items
    ~to_string:action_to_string
    ~on_select:(fun action ->
      match action with
      | Start -> ignore (do_start ps group_name)
      | Stop -> ignore (do_stop ps group_name)
      | Destroy -> ignore (do_destroy ps group_name)
      | Open_rpc -> ignore (do_open_rpc ps sb)
      | Add_account -> ignore (do_add_account ps group_name)
      | Add_node -> ignore (do_add_node ps group_name)
      | Add_baker -> ignore (do_add_baker ps group_name)
      | Reallocate_stake -> ignore (do_add_baker ps group_name))
    () ;
  ps

(* ─── Key Handling ──────────────────────────────────────────────────────── *)

let handle_sandbox_key ps sb key =
  let group_name = sb.group.name in
  match key with
  | "Return" | "Enter" -> open_action_modal ps sb
  | "s" -> do_start ps group_name
  | "S" -> do_stop ps group_name
  | "d" -> do_destroy ps group_name
  | "r" -> do_open_rpc ps sb
  | "a" -> do_add_account ps group_name
  | _ -> ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match key with
    | "Escape" | "q" -> Navigation.back ps
    | "j" | "Down" ->
        Navigation.update
          (fun s -> {s with cursor = clamp_cursor s.sandboxes (s.cursor + 1)})
          ps
    | "k" | "Up" ->
        Navigation.update
          (fun s -> {s with cursor = clamp_cursor s.sandboxes (s.cursor - 1)})
          ps
    | "c" ->
        Context.navigate "sandbox-create" ;
        ps
    | ("Return" | "Enter") when ps.Navigation.s.cursor = 0 ->
        Context.navigate "sandbox-create" ;
        ps
    | _ -> (
        match selected_sandbox ps.Navigation.s with
        | Some sb -> handle_sandbox_key ps sb key
        | None -> ps)

(* ─── PAGE_SIG ──────────────────────────────────────────────────────────── *)

let handled_keys () =
  Keys.
    [
      Escape;
      Char "q";
      Char "j";
      Char "k";
      Down;
      Up;
      Char "c";
      Enter;
      Char "s";
      Char "S";
      Char "d";
      Char "a";
      Char "r";
    ]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "c" "Create";
    kb "Enter" "Actions";
    kb "s" "Start";
    kb "S" "Stop";
    kb "d" "Destroy";
    kb "a" "Add account";
    kb "r" "RPC";
    kb "j/k" "Navigate";
    kb "Esc" "Back";
  ]

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let handled_keys = handled_keys

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps =
    List.map (fun (key, help) -> Miaou.Core.Tui_page.{key; help}) key_hint_pairs

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = name
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
