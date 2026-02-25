(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox management page — list sandboxes, create, start, stop, destroy.

    LAYOUT RULE: All layouts use Flex_layout / Grid_layout / Box_widget.
    No manual string alignment, no Pane_layout. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Flex = Miaou_widgets_layout.Flex_layout
module DL = Miaou_widgets_display.Description_list
module T = Themed_text
open Octez_manager_lib
open Rresult

let ( let* ) = Result.bind

let name = "sandbox"

(* ─── State ────────────────────────────────────────────────────────────── *)

type sandbox_info = {
  group : Group.t;
  node : Service.t option;
  baker : Service.t option;
  node_state : Data.Service_state.t option;
  baker_state : Data.Service_state.t option;
}

type state = {sandboxes : sandbox_info list; cursor : int}

type msg = unit

type pstate = state Navigation.t

(* ─── Data Loading ──────────────────────────────────────────────────────── *)

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
          let node =
            List.find_opt
              (fun (s : Service.t) -> String.equal s.role "node")
              services
          in
          let baker =
            List.find_opt
              (fun (s : Service.t) -> String.equal s.role "baker")
              services
          in
          {
            group = g;
            node;
            baker;
            node_state = Option.bind node (fun s -> find_state s.instance);
            baker_state = Option.bind baker (fun s -> find_state s.instance);
          })
        groups

let clamp_cursor sandboxes cursor =
  let n = List.length sandboxes in
  if n = 0 then 0 else max 0 (min cursor (n - 1))

let selected_sandbox s = List.nth_opt s.sandboxes s.cursor

(* ─── Init / Lifecycle ──────────────────────────────────────────────────── *)

let init () =
  let sandboxes = load_sandboxes () in
  Navigation.make {sandboxes; cursor = 0}

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      if Context.consume_instances_dirty () then
        let sandboxes = load_sandboxes () in
        let cursor = clamp_cursor sandboxes ps.Navigation.s.cursor in
        Navigation.update (fun _s -> {sandboxes; cursor}) ps
      else ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

(* ─── Rendering ─────────────────────────────────────────────────────────── *)

let status_badge (st : Data.Service_state.t option) =
  match st with
  | None -> T.muted "?"
  | Some {status = Running; _} -> T.success "●"
  | Some {status = Stopped; _} -> T.error "○"
  | Some _ -> T.muted "?"

let render_list_item ~selected ~sb =
  let node_dot = status_badge sb.node_state in
  let baker_dot = status_badge sb.baker_state in
  let arrow = if selected then T.warning "▶" else " " in
  let label =
    T.concat
      [arrow; " "; node_dot; " "; baker_dot; "  "; T.text "%s" sb.group.name]
  in
  let net = T.muted "  %s" sb.group.network in
  T.concat [label; "\n"; net]

let render_list ~sandboxes ~cursor ~size =
  if sandboxes = [] then
    Flex.create
      ~direction:Flex.Column
      ~padding:{Flex.left = 2; right = 1; top = 1; bottom = 0}
      [
        {
          Flex.render =
            (fun ~size:_ -> T.muted "No sandboxes. Press [c] to create.");
          basis = Flex.Px 1;
          cross = None;
        };
      ]
    |> fun f -> Flex.render f ~size
  else
    let rows_per_item = 2 in
    let items =
      List.mapi
        (fun i sb ->
          let selected = i = cursor in
          {
            Flex.render = (fun ~size:_ -> render_list_item ~selected ~sb);
            basis = Flex.Px rows_per_item;
            cross = None;
          })
        sandboxes
    in
    Flex.create
      ~direction:Flex.Column
      ~padding:{Flex.left = 1; right = 1; top = 0; bottom = 0}
      items
    |> fun f -> Flex.render f ~size

let rpc_metrics_for (sb : sandbox_info) =
  match sb.node with
  | None -> None
  | Some svc -> Rpc_metrics.get ~instance:svc.instance

let render_detail ~sb ~size =
  let cols = size.LTerm_geom.cols in
  let g = sb.group in
  let metrics = rpc_metrics_for sb in
  let head_level =
    match metrics with
    | Some {head_level = Some n; _} -> string_of_int n
    | _ -> "–"
  in
  let synced =
    match metrics with
    | Some {bootstrapped = Some true; _} -> T.success "Yes"
    | Some {bootstrapped = Some false; _} -> T.warning "Syncing"
    | _ -> T.muted "–"
  in
  let node_ep =
    match sb.node with
    | None -> "–"
    | Some svc -> Rpc_addr.to_endpoint svc.rpc_addr
  in
  let items =
    [
      ("Name", g.name);
      ("Network", g.network);
      ( "Node",
        Option.fold ~none:"–" ~some:(fun (s : Service.t) -> s.instance) sb.node
      );
      ( "Baker",
        Option.fold ~none:"–" ~some:(fun (s : Service.t) -> s.instance) sb.baker
      );
      ("RPC Endpoint", node_ep);
      ("Head Level", head_level);
      ("Synced", synced);
      ("Created", g.created_at);
    ]
  in
  let dl = DL.create ~title:"Sandbox Details" ~key_width:14 ~items () in
  DL.render ~cols ~wrap:true dl ~focus:false

let render_empty_detail ~size =
  Flex.create
    ~direction:Flex.Column
    ~padding:{Flex.left = 2; right = 1; top = 2; bottom = 0}
    [
      {
        Flex.render =
          (fun ~size:_ ->
            T.muted "Select a sandbox or press [c] to create one.");
        basis = Flex.Px 1;
        cross = None;
      };
    ]
  |> fun f -> Flex.render f ~size

let header = ["  Sandboxes"; ""]

let key_hint_pairs =
  [
    ("c", "create");
    ("Enter", "actions");
    ("s", "start");
    ("S", "stop");
    ("d", "destroy");
    ("a", "add account");
    ("r", "RPC");
    ("j/k", "nav");
    ("Esc", "back");
  ]

let list_width total_cols = max 22 (total_cols / 3)

let render_content s ~size =
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let lw = list_width cols in
  let layout =
    Flex.create
      ~direction:Flex.Row
      [
        {
          Flex.render =
            (fun ~size ->
              render_list ~sandboxes:s.sandboxes ~cursor:s.cursor ~size);
          basis = Flex.Px lw;
          cross = None;
        };
        {
          Flex.render =
            (fun ~size ->
              let h = size.LTerm_geom.rows in
              String.concat "\n" (List.init h (fun _ -> T.muted "|")));
          basis = Flex.Px 1;
          cross = None;
        };
        {
          Flex.render =
            (fun ~size ->
              match selected_sandbox s with
              | Some sb -> render_detail ~sb ~size
              | None -> render_empty_detail ~size);
          basis = Flex.Fill;
          cross = None;
        };
      ]
  in
  Flex.render layout ~size:{LTerm_geom.rows; cols}

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  Context.tick_spinner () ;
  Context.tick_toasts () ;
  let cols = size.LTerm_geom.cols in
  let toast = Context.render_toasts ~cols in
  let footer =
    let hints = Themed_page.render_themed_footer ~cols key_hint_pairs in
    if String.length toast > 0 then [toast] @ hints else hints
  in
  Themed_page.render_layout ~size ~header ~footer ~child:(fun avail ->
      render_content s ~size:avail)

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

let action_to_string = function
  | Start -> "Start"
  | Stop -> "Stop"
  | Destroy -> "Destroy"
  | Open_rpc -> "Open RPC Browser"
  | Add_account -> "Add Account"
  | Add_node -> "Add Node"
  | Add_baker -> "Add Baker"

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

let do_open_rpc ps sb =
  (match sb.node with
  | Some svc ->
      Context.set_pending_instance_detail svc.instance ;
      Context.navigate "rpc-browser"
  | None -> Context.toast_warn "No node found for this sandbox") ;
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
  let has_node = Option.is_some sb.node in
  let items =
    [Start; Stop]
    @ (if has_node then [Open_rpc] else [])
    @ [Add_account; Add_node; Add_baker; Destroy]
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
      | Add_baker -> ignore (do_add_baker ps group_name))
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
