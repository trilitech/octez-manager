(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox Key Allocation page.

    Table-based UI for viewing and editing how the sandbox wallet's delegates
    are distributed across baker processes. Supports adding new baker rows and
    applying the allocation by installing new baker services.

    LAYOUT RULE: Grid_layout for the table, Flex_layout for the page. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Flex = Miaou_widgets_layout.Flex_layout
module Grid = Miaou_widgets_layout.Grid_layout
module T = Themed_text
open Octez_manager_lib
open Rresult

let ( let* ) = Result.bind

let name = "sandbox-key-alloc"

(* ─── State ────────────────────────────────────────────────────────────── *)

type baker_row = {instance : string; key_count : int; is_new : bool}

type state = {
  group_name : string;
  rows : baker_row list;
  total_delegates : int;
  cursor : int;
  editing : (int * string) option;
      (** (row_idx, text_being_typed) when inline editing *)
}

type msg = unit

type pstate = state Navigation.t

(* ─── Data Loading ──────────────────────────────────────────────────────── *)

(** Count baker delegates from the sandbox wallet.
    Each delegate = 3 wallet entries (addr + consensus_key + companion_key).
    Returns 0 if wallet not found. *)
let count_baker_delegates ~group_name =
  let wallet = Sandbox.wallet_dir ~sandbox_name:group_name in
  match Yes_wallet_io.read_wallet_pkhs ~wallet_dir:wallet with
  | Ok entries ->
      (* Entries with alias "delegate-K" where K ≡ 1 (mod 3) are baker (consensus key) *)
      List.length entries / 3
  | Error _ -> 0

(** Load existing baker delegate counts from their env files. *)
let load_baker_key_count (svc : Service.t) =
  match Node_env.read ~inst:svc.Service.instance with
  | Error _ -> 0
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_BAKER_DELEGATES_CSV" pairs with
      | None | Some "" -> 0
      | Some csv ->
          csv |> String.split_on_char ',' |> List.map String.trim
          |> List.filter (fun s -> s <> "")
          |> List.length)

let load_rows ~group_name =
  match Sandbox.find_sandbox_bakers ~group_name with
  | Error _ -> []
  | Ok bakers ->
      List.map
        (fun (svc : Service.t) ->
          {
            instance = svc.Service.instance;
            key_count = load_baker_key_count svc;
            is_new = false;
          })
        bakers

let init () =
  let group_name =
    Option.value ~default:"" (Context.take_pending_sandbox_group ())
  in
  let rows = load_rows ~group_name in
  let total_delegates = count_baker_delegates ~group_name in
  Navigation.make
    {group_name; rows; total_delegates; cursor = 0; editing = None}

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      if Context.consume_instances_dirty () then
        let s = ps.Navigation.s in
        let rows = load_rows ~group_name:s.group_name in
        let total_delegates = count_baker_delegates ~group_name:s.group_name in
        Navigation.update (fun _s -> {s with rows; total_delegates}) ps
      else ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

(* ─── Helpers ───────────────────────────────────────────────────────────── *)

let clamp_cursor rows cursor =
  let n = List.length rows in
  if n = 0 then 0 else max 0 (min cursor (n - 1))

let allocated_keys rows = List.fold_left (fun acc r -> acc + r.key_count) 0 rows

let unallocated rows total = total - allocated_keys rows

(** Find the primary node for this sandbox (for new baker installs). *)
let primary_node_instance ~group_name =
  match Sandbox.find_sandbox_nodes ~group_name with
  | Ok (first :: _) -> first.Service.instance
  | Ok [] | Error _ ->
      (* Fallback: construct expected name *)
      Printf.sprintf "%s-node" group_name

(** Build delegate aliases for a baker: slice [start, start+count) of consensus
    key aliases (delegate-1, delegate-4, delegate-7, …). *)
let delegate_aliases_for_slice ~start ~count =
  List.init count (fun i ->
      Printf.sprintf "delegate-%d" (((start + i) * 3) + 1))

(* ─── Redistribution ─────────────────────────────────────────────────────── *)

(** Redistribute all delegates evenly across all rows.
    Ceiling allocation goes to the first rows. Resets all key counts. *)
let redistribute s =
  let rows = s.rows in
  let n = List.length rows in
  if n = 0 then s
  else
    let total = s.total_delegates in
    let base = total / n in
    let extra = total mod n in
    let new_rows =
      List.mapi
        (fun i r -> {r with key_count = (base + if i < extra then 1 else 0)})
        rows
    in
    {s with rows = new_rows}

(* ─── Apply Allocation ───────────────────────────────────────────────────── *)

(** Update an existing baker's delegate list in its env file and restart it. *)
let update_baker_delegates ~on_log ~instance ~delegates =
  on_log (Printf.sprintf "[%s] Updating delegate list...\n" instance) ;
  let* pairs = Node_env.read ~inst:instance in
  let new_pairs =
    List.map
      (fun (k, v) ->
        if String.equal k "OCTEZ_BAKER_DELEGATES_CSV" then
          (k, String.concat "," delegates)
        else (k, v))
      pairs
  in
  let* () = Node_env.write_pairs ~inst:instance new_pairs in
  on_log (Printf.sprintf "[%s] Restarting baker...\n" instance) ;
  Systemd.restart ~role:"baker" ~instance ()

let apply_allocation s =
  let group_name = s.group_name in
  let node_instance = primary_node_instance ~group_name in
  (* Compute delegate slices for ALL bakers in order, then:
     - existing bakers: update env + restart
     - new bakers: install via Sandbox.add_baker *)
  let _, all_work =
    List.fold_left
      (fun (offset, acc) r ->
        (offset + r.key_count, (r, offset, r.key_count) :: acc))
      (0, [])
      s.rows
  in
  let all_work = List.rev all_work in
  let desc = Printf.sprintf "Apply key allocation for %s" group_name in
  Context.toast_info (T.text "%s..." desc) ;
  Job_manager.submit
    ~timeout:None
    ~description:desc
    ~on_complete:(fun _ -> Context.mark_instances_dirty ())
    (fun ~append_log () ->
      List.fold_left
        (fun acc (row, start, count) ->
          let* () = acc in
          let delegates = delegate_aliases_for_slice ~start ~count in
          if row.is_new then
            Sandbox.add_baker
              ~on_log:(fun msg -> append_log (msg ^ "\n"))
              ~group_name
              ~node_instance
              ~delegates
              ()
            |> Result.map ignore
          else
            update_baker_delegates
              ~on_log:append_log
              ~instance:row.instance
              ~delegates)
        (Ok ())
        all_work) ;
  Context.navigate_back ()

(* ─── Rendering ─────────────────────────────────────────────────────────── *)

let render_baker_row s ~row_idx row ~total_allocated =
  let selected = row_idx = s.cursor in
  let editing_text =
    match s.editing with
    | Some (idx, txt) when idx = row_idx -> Some txt
    | _ -> None
  in
  let cursor_str = if selected then T.warning "▶ " else "  " in
  let instance_str =
    if row.is_new then cursor_str ^ T.muted "[+ New Baker]"
    else cursor_str ^ T.text "%s" row.instance
  in
  let key_str =
    match editing_text with
    | Some txt -> T.warning "[%s]" txt
    | None ->
        if row.key_count = 0 then T.muted "0" else T.text "%d" row.key_count
  in
  (* Share of total allocated keys — sums to 100% across all bakers *)
  let share_str =
    if total_allocated <= 0 || row.key_count = 0 then T.muted "–"
    else
      let pct =
        float_of_int row.key_count /. float_of_int total_allocated *. 100.0
      in
      T.text "%.1f%%" pct
  in
  (instance_str, key_str, share_str)

let render_baker_table s ~size =
  let nrows = List.length s.rows in
  if nrows = 0 then T.muted "No bakers. Press [a] to add one."
  else
    let total_allocated = allocated_keys s.rows in
    let header_row =
      [
        Grid.cell ~row:0 ~col:0 (fun ~size:_ -> T.muted "Baker");
        Grid.cell ~row:0 ~col:1 (fun ~size:_ -> T.muted "Keys");
        Grid.cell ~row:0 ~col:2 (fun ~size:_ -> T.muted "Share%%");
      ]
    in
    let data_children =
      List.concat
        (List.mapi
           (fun i row ->
             let r = i + 1 in
             (* +1 for header row *)
             let inst, key, share =
               render_baker_row s ~row_idx:i row ~total_allocated
             in
             [
               Grid.cell ~row:r ~col:0 (fun ~size:_ -> inst);
               Grid.cell ~row:r ~col:1 (fun ~size:_ -> key);
               Grid.cell ~row:r ~col:2 (fun ~size:_ -> share);
             ])
           s.rows)
    in
    let grid =
      Grid.create
        ~rows:(Grid.Px 1 :: List.init nrows (fun _ -> Grid.Px 1))
        ~cols:[Grid.Fr 1.; Grid.Px 6; Grid.Px 9]
        ~col_gap:2
        (header_row @ data_children)
    in
    Grid.render grid ~size

let header = ["  Key Allocation"; ""]

let render_content s ~size =
  let rows = size.LTerm_geom.rows in
  let cols = size.LTerm_geom.cols in
  let unalloc = unallocated s.rows s.total_delegates in
  let summary =
    T.concat
      [
        T.muted "Sandbox: ";
        T.text "%s" s.group_name;
        T.muted "  Delegates: ";
        T.text "%d total" s.total_delegates;
        T.muted "  Unallocated: ";
        (if unalloc < 0 then T.error "%d" unalloc
         else if unalloc = 0 then T.success "%d" unalloc
         else T.warning "%d" unalloc);
      ]
  in
  let hint =
    T.muted
      "j/k nav  a add  d del  Enter edit  Tab redistribute  c apply  Esc back"
  in
  let layout =
    Flex.create
      ~direction:Flex.Column
      ~padding:{Flex.left = 1; right = 1; top = 1; bottom = 0}
      [
        {Flex.render = (fun ~size:_ -> summary); basis = Flex.Px 1; cross = None};
        {Flex.render = (fun ~size:_ -> ""); basis = Flex.Px 1; cross = None};
        {
          Flex.render = (fun ~size -> render_baker_table s ~size);
          basis = Flex.Fill;
          cross = None;
        };
        {Flex.render = (fun ~size:_ -> hint); basis = Flex.Px 1; cross = None};
      ]
  in
  Flex.render layout ~size:{LTerm_geom.rows; cols}

let key_hint_pairs =
  [
    ("a", "add");
    ("d", "delete");
    ("Tab", "redistribute");
    ("c", "apply");
    ("Esc", "back");
  ]

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

(* ─── Key Handling ──────────────────────────────────────────────────────── *)

let commit_edit s idx txt =
  let key_count =
    match int_of_string_opt (String.trim txt) with
    | Some n when n >= 0 -> n
    | _ -> (List.nth s.rows idx).key_count
  in
  let rows =
    List.mapi (fun i r -> if i = idx then {r with key_count} else r) s.rows
  in
  {s with rows; editing = None}

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    let s = ps.Navigation.s in
    match s.editing with
    | Some (idx, txt) ->
        (* Inline editing mode: route characters to the text buffer *)
        let new_ps =
          match key with
          | "Return" | "Enter" ->
              Navigation.update (fun s -> commit_edit s idx txt) ps
          | "Escape" -> Navigation.update (fun s -> {s with editing = None}) ps
          | "BackSpace" ->
              let new_txt =
                if String.length txt = 0 then ""
                else String.sub txt 0 (String.length txt - 1)
              in
              Navigation.update
                (fun s -> {s with editing = Some (idx, new_txt)})
                ps
          | c when String.length c = 1 && c.[0] >= '0' && c.[0] <= '9' ->
              Navigation.update
                (fun s -> {s with editing = Some (idx, txt ^ c)})
                ps
          | _ -> ps
        in
        new_ps
    | None -> (
        match key with
        | "Escape" | "q" -> Navigation.back ps
        | "j" | "Down" ->
            Navigation.update
              (fun s -> {s with cursor = clamp_cursor s.rows (s.cursor + 1)})
              ps
        | "k" | "Up" ->
            Navigation.update
              (fun s -> {s with cursor = clamp_cursor s.rows (s.cursor - 1)})
              ps
        | "Return" | "Enter" ->
            (* Start editing: empty buffer so user types a fresh value *)
            let cur = s.cursor in
            if cur < List.length s.rows then
              Navigation.update (fun s -> {s with editing = Some (cur, "")}) ps
            else ps
        | "a" ->
            (* Append new baker row *)
            let new_row =
              {
                instance =
                  Printf.sprintf "(new baker %d)" (List.length s.rows + 1);
                key_count = 0;
                is_new = true;
              }
            in
            Navigation.update
              (fun s ->
                let rows = s.rows @ [new_row] in
                {s with rows; cursor = clamp_cursor rows (List.length rows - 1)})
              ps
        | "d" -> (
            (* Delete selected row only if is_new *)
            let cur = s.cursor in
            let selected_row = List.nth_opt s.rows cur in
            match selected_row with
            | Some {is_new = true; _} ->
                Navigation.update
                  (fun s ->
                    let rows = List.filteri (fun i _ -> i <> cur) s.rows in
                    {s with rows; cursor = clamp_cursor rows cur})
                  ps
            | _ -> ps)
        | "\t" | "Tab" ->
            (* Redistribute all delegates evenly across all baker rows *)
            Navigation.update redistribute ps
        | "c" ->
            (* Validate then show confirmation before applying *)
            let allocated = allocated_keys s.rows in
            let total = s.total_delegates in
            if allocated > total then (
              Context.toast_error
                (T.text
                   "Overallocated by %d key(s) — use Tab to redistribute"
                   (allocated - total)) ;
              ps)
            else
              let change_lines =
                List.map
                  (fun r ->
                    if r.is_new then
                      Printf.sprintf
                        "  install %s: %d keys"
                        r.instance
                        r.key_count
                    else
                      Printf.sprintf
                        "  update %s: %d keys (restart)"
                        r.instance
                        r.key_count)
                  s.rows
              in
              let unalloc = total - allocated in
              let summary =
                Printf.sprintf
                  "\n%d / %d delegates allocated, %d unallocated"
                  allocated
                  total
                  unalloc
              in
              let message = String.concat "\n" (change_lines @ [summary]) in
              Modal_helpers.confirm_modal
                ~title:"Apply Key Allocation?"
                ~message
                ~on_result:(fun yes -> if yes then apply_allocation s)
                () ;
              ps
        | _ -> ps)

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
      Enter;
      Char "a";
      Char "d";
      Char "c";
      Tab;
    ]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "a" "Add baker";
    kb "d" "Delete";
    kb "Enter" "Edit count";
    kb "Tab" "Redistribute";
    kb "c" "Apply";
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
