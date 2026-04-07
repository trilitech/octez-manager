(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Navigation = Miaou.Core.Navigation
module Keys = Miaou.Core.Keys

let name = "main_shell"

(* Tab IDs match page names for transparent nav routing *)
let tab_instances = Instances.name

let tab_wallets = Wallets_page.name

let tab_binaries = Binaries.name

let tab_rpcs = Rpc_node_selection.name

let tab_diagnostics = Diagnostics.name

let tab_topology = Topology_page.name

let tab_sandbox = Sandbox_page.name

let tab_experimental = "experimental"

type state = {
  tabs : Responsive_tabs_widget.t;
  instances_ps : Instances.Page.pstate;
  wallets_ps : Wallets_page.Page.pstate;
  binaries_ps : Binaries.Page.pstate;
  rpc_ps : Rpc_node_selection.Page.pstate;
  diagnostics_ps : Diagnostics.Page.pstate;
  topology_ps : Topology_page.Page.pstate;
  sandbox_ps : Sandbox_page.Page.pstate;
}

type msg = unit

type pstate = state Navigation.t

let initial_tabs =
  Responsive_tabs_widget.make
    [
      Responsive_tabs_widget.tab
        ~id:tab_instances
        ~labels:["1 Instances"; "1 Inst"];
      Responsive_tabs_widget.tab ~id:tab_wallets ~labels:["2 Wallets"; "2 Wall"];
      Responsive_tabs_widget.tab
        ~id:tab_binaries
        ~labels:["3 Binaries"; "3 Bins"];
      Responsive_tabs_widget.tab ~id:tab_rpcs ~labels:["4 RPCs"];
      Responsive_tabs_widget.tab
        ~id:tab_diagnostics
        ~labels:["5 Diagnostics"; "5 Diag"];
      Responsive_tabs_widget.tab
        ~id:tab_topology
        ~labels:["6 Topology"; "6 Topo"];
      Responsive_tabs_widget.tab
        ~id:tab_experimental
        ~labels:["7 Experimental"; "7 Exp"];
      Responsive_tabs_widget.tab ~id:tab_sandbox ~labels:["8 Sandbox"; "8 Sand"];
    ]

let init () =
  Navigation.make
    {
      tabs = initial_tabs;
      instances_ps = Instances.Page.init ();
      wallets_ps = Wallets_page.Page.init ();
      binaries_ps = Binaries.Page.init ();
      rpc_ps = Rpc_node_selection.Page.init ();
      diagnostics_ps = Diagnostics.Page.init ();
      topology_ps = Topology_page.Page.init ();
      sandbox_ps = Sandbox_page.Page.init ();
    }

let update ps _ = ps

let tab_id_of_context_tab = function
  | Context.Tab_instances -> tab_instances
  | Context.Tab_wallets -> tab_wallets
  | Context.Tab_binaries -> tab_binaries
  | Context.Tab_rpcs -> tab_rpcs
  | Context.Tab_diagnostics -> tab_diagnostics
  | Context.Tab_topology -> tab_topology
  | Context.Tab_sandboxes -> tab_sandbox

let is_tab_target t =
  String.equal t tab_instances
  || String.equal t tab_wallets
  || String.equal t tab_binaries
  || String.equal t tab_rpcs
  || String.equal t tab_diagnostics
  || String.equal t tab_topology
  || String.equal t tab_sandbox
  || String.equal t tab_experimental

(** Route navigation from a sub-page: tab targets become tab switches;
    other targets propagate as [Navigation.goto] on the shell. *)
let route_nav ~shell_ps ~shell_s target =
  let ps = {shell_ps with Navigation.s = shell_s} in
  if is_tab_target target then
    let tabs' = Responsive_tabs_widget.select shell_s.tabs ~id:target in
    {ps with Navigation.s = {shell_s with tabs = tabs'}}
  else Navigation.goto target ps

(** After calling a sub-page function, store the updated sub-pstate (with nav
    cleared) and propagate any navigation signal to the shell level. *)
let apply_sub_nav ~shell_ps ~shell_s nav_result =
  let ps = {shell_ps with Navigation.s = shell_s} in
  match nav_result with
  | None -> ps
  | Some Navigation.Back -> Navigation.back ps
  | Some Navigation.Quit -> Navigation.quit ps
  | Some (Navigation.Goto target) -> route_nav ~shell_ps:ps ~shell_s target

let current_tab_id s =
  match Responsive_tabs_widget.current s.tabs with
  | None -> ""
  | Some tab -> tab.Responsive_tabs_widget.id

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let tab_bar = Responsive_tabs_widget.render s.tabs ~focus ~cols in
  let tab_bar_size = {size with LTerm_geom.rows = 1} in
  let tab_bar_themed =
    Themed_page.apply_themed_background ~size:tab_bar_size tab_bar
  in
  let content_rows = max 1 (size.LTerm_geom.rows - 1) in
  let content_size = {size with LTerm_geom.rows = content_rows} in
  let content =
    match current_tab_id s with
    | id when String.equal id tab_instances ->
        Instances.Page.view s.instances_ps ~focus ~size:content_size
    | id when String.equal id tab_wallets ->
        Wallets_page.Page.view s.wallets_ps ~focus ~size:content_size
    | id when String.equal id tab_binaries ->
        Binaries.Page.view s.binaries_ps ~focus ~size:content_size
    | id when String.equal id tab_rpcs ->
        Rpc_node_selection.Page.view s.rpc_ps ~focus ~size:content_size
    | id when String.equal id tab_diagnostics ->
        Diagnostics.Page.view s.diagnostics_ps ~focus ~size:content_size
    | id when String.equal id tab_topology ->
        Topology_page.Page.view s.topology_ps ~focus ~size:content_size
    | id when String.equal id tab_sandbox ->
        Sandbox_page.Page.view s.sandbox_ps ~focus ~size:content_size
    | id when String.equal id tab_experimental ->
        (* Experimental tab has no content - modal opens on selection *)
        Themed_page.apply_themed_background ~size:content_size ""
    | _ -> Themed_page.apply_themed_background ~size:content_size ""
  in
  tab_bar_themed ^ "\n" ^ content

let refresh ps =
  let s = ps.Navigation.s in
  (* Apply any pending tab switch queued via Context *)
  let s =
    match Context.consume_tab_switch () with
    | None -> s
    | Some ctx_tab ->
        let id = tab_id_of_context_tab ctx_tab in
        {s with tabs = Responsive_tabs_widget.select s.tabs ~id}
  in
  let ps = {ps with Navigation.s} in
  let s = ps.Navigation.s in
  (* Refresh only the active tab to avoid consuming context signals
     intended for other sub-pages *)
  let tab = current_tab_id s in
  if String.equal tab tab_instances then
    let ip' = Instances.Page.refresh s.instances_ps in
    let shell_s = {s with instances_ps = Navigation.make ip'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending ip')
  else if String.equal tab tab_wallets then
    let wp' = Wallets_page.Page.refresh s.wallets_ps in
    let shell_s = {s with wallets_ps = Navigation.make wp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending wp')
  else if String.equal tab tab_binaries then
    let bp' = Binaries.Page.refresh s.binaries_ps in
    let shell_s = {s with binaries_ps = Navigation.make bp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending bp')
  else if String.equal tab tab_rpcs then
    let rp' = Rpc_node_selection.Page.refresh s.rpc_ps in
    let shell_s = {s with rpc_ps = Navigation.make rp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending rp')
  else if String.equal tab tab_diagnostics then
    let dp' = Diagnostics.Page.refresh s.diagnostics_ps in
    let shell_s = {s with diagnostics_ps = Navigation.make dp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending dp')
  else if String.equal tab tab_topology then
    let tp' = Topology_page.Page.refresh s.topology_ps in
    let shell_s = {s with topology_ps = Navigation.make tp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending tp')
  else if String.equal tab tab_sandbox then
    let sp' = Sandbox_page.Page.refresh s.sandbox_ps in
    let shell_s = {s with sandbox_ps = Navigation.make sp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending sp')
  else if String.equal tab tab_experimental then
    (* Experimental tab has no page state, but needs to handle navigation *)
    match Context.consume_navigation () with
    | Some (Context.Goto target) -> route_nav ~shell_ps:ps ~shell_s:s target
    | Some Context.Back -> Navigation.back ps
    | Some Context.Quit -> Navigation.quit ps
    | None -> ps
  else ps

let dispatch_key ps key ~size =
  let s = ps.Navigation.s in
  let tab = current_tab_id s in
  if String.equal tab tab_instances then
    let ip' = Instances.Page.handle_key s.instances_ps key ~size in
    let shell_s = {s with instances_ps = Navigation.make ip'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending ip')
  else if String.equal tab tab_wallets then
    let wp' = Wallets_page.Page.handle_key s.wallets_ps key ~size in
    let shell_s = {s with wallets_ps = Navigation.make wp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending wp')
  else if String.equal tab tab_binaries then
    let bp' = Binaries.Page.handle_key s.binaries_ps key ~size in
    let shell_s = {s with binaries_ps = Navigation.make bp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending bp')
  else if String.equal tab tab_rpcs then
    let rp' = Rpc_node_selection.Page.handle_key s.rpc_ps key ~size in
    let shell_s = {s with rpc_ps = Navigation.make rp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending rp')
  else if String.equal tab tab_diagnostics then
    let dp' = Diagnostics.Page.handle_key s.diagnostics_ps key ~size in
    let shell_s = {s with diagnostics_ps = Navigation.make dp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending dp')
  else if String.equal tab tab_topology then
    let tp' = Topology_page.Page.handle_key s.topology_ps key ~size in
    let shell_s = {s with topology_ps = Navigation.make tp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending tp')
  else if String.equal tab tab_sandbox then
    let sp' = Sandbox_page.Page.handle_key s.sandbox_ps key ~size in
    let shell_s = {s with sandbox_ps = Navigation.make sp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending sp')
  else ps

let dispatch_modal_key ps key ~size =
  let s = ps.Navigation.s in
  let tab = current_tab_id s in
  if String.equal tab tab_instances then
    let ip' = Instances.Page.handle_modal_key s.instances_ps key ~size in
    let shell_s = {s with instances_ps = Navigation.make ip'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending ip')
  else if String.equal tab tab_wallets then
    let wp' = Wallets_page.Page.handle_modal_key s.wallets_ps key ~size in
    let shell_s = {s with wallets_ps = Navigation.make wp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending wp')
  else if String.equal tab tab_binaries then
    let bp' = Binaries.Page.handle_modal_key s.binaries_ps key ~size in
    let shell_s = {s with binaries_ps = Navigation.make bp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending bp')
  else if String.equal tab tab_rpcs then
    let rp' = Rpc_node_selection.Page.handle_modal_key s.rpc_ps key ~size in
    let shell_s = {s with rpc_ps = Navigation.make rp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending rp')
  else if String.equal tab tab_diagnostics then
    let dp' = Diagnostics.Page.handle_modal_key s.diagnostics_ps key ~size in
    let shell_s = {s with diagnostics_ps = Navigation.make dp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending dp')
  else if String.equal tab tab_topology then
    let tp' = Topology_page.Page.handle_modal_key s.topology_ps key ~size in
    let shell_s = {s with topology_ps = Navigation.make tp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending tp')
  else if String.equal tab tab_sandbox then
    let sp' = Sandbox_page.Page.handle_modal_key s.sandbox_ps key ~size in
    let shell_s = {s with sandbox_ps = Navigation.make sp'.Navigation.s} in
    apply_sub_nav ~shell_ps:ps ~shell_s (Navigation.pending sp')
  else (
    (* Experimental tab and fallback: use global modal handler *)
    Miaou.Core.Modal_manager.handle_key key ;
    ps)

let switch_tab ps id =
  let s = ps.Navigation.s in
  {
    ps with
    Navigation.s = {s with tabs = Responsive_tabs_widget.select s.tabs ~id};
  }

(** Open the create-instance dropdown on the instances tab. *)
let open_instances_create_menu ps =
  let s = ps.Navigation.s in
  let instances_ps = Instances.For_tests.open_create_menu s.instances_ps in
  {ps with Navigation.s = {s with instances_ps}}

let handle_key ps key ~size =
  if Miaou.Core.Modal_manager.has_active () then dispatch_modal_key ps key ~size
  else
    match Global_shortcuts.handle key with
    | Global_shortcuts.Handled -> ps
    | Global_shortcuts.NotGlobal -> (
        let s = ps.Navigation.s in
        let current = current_tab_id s in
        match key with
        | "1" ->
            if String.equal current tab_instances then
              (* Pressing current tab number triggers create menu *)
              open_instances_create_menu ps
            else switch_tab ps tab_instances
        | "2" -> switch_tab ps tab_wallets
        | "3" -> switch_tab ps tab_binaries
        | "4" -> switch_tab ps tab_rpcs
        | "5" -> switch_tab ps tab_diagnostics
        | "6" -> switch_tab ps tab_topology
        | "7" ->
            (* Switch to experimental tab and open modal *)
            let ps' = switch_tab ps tab_experimental in
            Modal_helpers.open_choice_modal
              ~title:"Experimental Features"
              ~items:["sandbox"]
              ~to_string:(fun s ->
                Printf.sprintf "%s  [BETA]" (String.capitalize_ascii s))
              ~on_select:(fun _choice ->
                (* Close modal and navigate to sandbox tab *)
                Context.set_pending_tab Context.Tab_sandboxes)
              () ;
            ps'
        | "8" -> switch_tab ps tab_sandbox
        | _ -> dispatch_key ps key ~size)

let handle_modal_key ps key ~size = dispatch_modal_key ps key ~size

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

let handled_keys () = Keys.[Escape; Left; Right; Home; End]

let keymap _ps = []

let key_hints _ps = []

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let view = view

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let has_modal = has_modal

  let handled_keys = handled_keys

  let keymap = keymap

  let on_key ps key ~size =
    if Miaou.Core.Modal_manager.has_active () then
      let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
      (ps', Miaou_interfaces.Key_event.Handled)
    else
      let key_str = Miaou.Core.Keys.to_string key in
      (match Global_shortcuts.handle key_str with
        | Global_shortcuts.Handled -> ps
        | Global_shortcuts.NotGlobal -> handle_key ps key_str ~size)
      |> fun ps' -> (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints = key_hints
end

module Page = Page_Impl

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name (module Page_Impl)
