(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox Key Allocation page.

    Single-table UI: Baker | Keys | Stake%.

    Users edit Keys or Stake% inline for each baker. Tab redistributes
    remaining unallocated keys evenly. [c] applies: stops all bakers,
    reallocates wallet slices (updates env CSV), clears highwatermarks,
    and restarts.

    LAYOUT RULE: Grid_layout for the table, Flex_layout for the page. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module T = Themed_text
open Octez_manager_lib

let name = "sandbox-key-alloc"

(* ─── Types (re-exported from Sandbox_key_alloc_view) ───────────────────── *)

type edit_field = Sandbox_key_alloc_view.edit_field = EKeys | EPct

type baker_row = Sandbox_key_alloc_view.baker_row = {
  instance : string;
  keys : int;
  is_new : bool;
  to_delete : bool;
}

type state = Sandbox_key_alloc_view.state = {
  group_name : string;
  rows : baker_row list;
  total_delegates : int;
  delegate_balances : (float array * float) option;
  cursor : int;
  editing : (int * edit_field * string) option;
  user_modified : bool;
      (** True when user has edited values; prevents refresh from overwriting. *)
}

type msg = unit

type pstate = state Navigation.t

(* ─── Async Stake Fetch ──────────────────────────────────────────────────── *)

let kap_bal_fetching : (string, unit) Hashtbl.t = Hashtbl.create 2

let kap_bal_results : (string, float array * float) Hashtbl.t = Hashtbl.create 2

let kap_bal_lock = Mutex.create ()

let get_cached_balances ~group_name =
  Mutex.protect kap_bal_lock (fun () ->
      Hashtbl.find_opt kap_bal_results group_name)

(** Signal only the balance update, not a full data reload. *)
let kap_bal_arrived : (string, unit) Hashtbl.t = Hashtbl.create 2

let consume_bal_arrived ~group_name =
  Mutex.protect kap_bal_lock (fun () ->
      let had = Hashtbl.mem kap_bal_arrived group_name in
      Hashtbl.remove kap_bal_arrived group_name ;
      had)

let maybe_fetch_balances ~group_name =
  let already =
    Mutex.protect kap_bal_lock (fun () ->
        Hashtbl.mem kap_bal_fetching group_name)
  in
  if not already then begin
    Mutex.protect kap_bal_lock (fun () ->
        Hashtbl.replace kap_bal_fetching group_name ()) ;
    Background_runner.submit_blocking
      ~on_complete:(fun () ->
        Mutex.protect kap_bal_lock (fun () ->
            Hashtbl.replace kap_bal_arrived group_name ()) ;
        Context.mark_instances_dirty ())
      (fun () ->
        let wallet_dir = Sandbox.wallet_dir ~sandbox_name:group_name in
        let endpoint =
          match Sandbox.find_sandbox_node ~group_name with
          | Ok (Some svc) ->
              Printf.sprintf "http://%s" (Rpc_addr.to_string svc.rpc_addr)
          | _ -> "http://127.0.0.1:18732"
        in
        match Yes_wallet_io.fetch_delegate_balances ~endpoint ~wallet_dir with
        | Ok result ->
            Mutex.protect kap_bal_lock (fun () ->
                Hashtbl.replace kap_bal_results group_name result)
        | Error _ -> ())
  end

(* ─── Data Loading ──────────────────────────────────────────────────────── *)

(** Count base delegate entries (alias index mod 3 = 0) in the wallet. *)
let count_baker_delegates ~group_name =
  let wallet = Sandbox.wallet_dir ~sandbox_name:group_name in
  match Yes_wallet_io.read_wallet_pkhs ~wallet_dir:wallet with
  | Ok entries ->
      List.fold_left
        (fun acc (alias, _) ->
          match String.split_on_char '-' alias with
          | ["delegate"; ns] -> (
              match int_of_string_opt ns with
              | Some n when n mod 3 = 0 -> acc + 1
              | _ -> acc)
          | _ -> acc)
        0
        entries
  | Error _ -> 0

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
            keys = load_baker_key_count svc;
            is_new = false;
            to_delete = false;
          })
        bakers

let init () =
  let group_name =
    Option.value ~default:"" (Context.take_pending_sandbox_group ())
  in
  let rows = load_rows ~group_name in
  let total_delegates = count_baker_delegates ~group_name in
  Navigation.make
    {
      group_name;
      rows;
      total_delegates;
      delegate_balances = None;
      cursor = 0;
      editing = None;
      user_modified = false;
    }

let update ps _ = ps

let refresh ps =
  let s = ps.Navigation.s in
  maybe_fetch_balances ~group_name:s.group_name ;
  let new_bal = get_cached_balances ~group_name:s.group_name in
  let bal_arrived = consume_bal_arrived ~group_name:s.group_name in
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      (* Only reload rows from disk when not user-modified. *)
      let data_changed =
        Context.consume_instances_dirty () && not s.user_modified
      in
      if data_changed then
        let rows = load_rows ~group_name:s.group_name in
        let total_delegates = count_baker_delegates ~group_name:s.group_name in
        Navigation.update
          (fun _s ->
            {
              s with
              rows;
              total_delegates;
              delegate_balances = new_bal;
              user_modified = false;
            })
          ps
      else if bal_arrived then
        Navigation.update (fun s -> {s with delegate_balances = new_bal}) ps
      else ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

(* ─── Helpers ───────────────────────────────────────────────────────────── *)

let clamp_cursor rows cursor =
  let n = List.length rows in
  if n = 0 then 0 else max 0 (min cursor (n - 1))

let allocated_keys rows =
  List.fold_left (fun acc r -> if r.to_delete then acc else acc + r.keys) 0 rows

(** Build consensus-key delegate aliases for slice [start, start+count). *)
let delegate_aliases_for_slice ~start ~count =
  List.init count (fun i ->
      Printf.sprintf "delegate-%d" (((start + i) * 3) + 1))


(* ─── Redistribution ─────────────────────────────────────────────────────── *)

(** Redistribute total_delegates evenly across non-deleted rows. *)
let redistribute s =
  let active = List.filter (fun r -> not r.to_delete) s.rows in
  let n = List.length active in
  if n = 0 then s
  else
    let total = s.total_delegates in
    let base = total / n in
    let extra = total mod n in
    let active_idx = ref 0 in
    let rows =
      List.map
        (fun r ->
          if r.to_delete then r
          else
            let i = !active_idx in
            incr active_idx ;
            {r with keys = (base + if i < extra then 1 else 0)})
        s.rows
    in
    {s with rows; user_modified = true}

(* ─── Apply Allocation ───────────────────────────────────────────────────── *)

let clear_highwatermarks ~sandbox_name ~baker_instance =
  let base = Sandbox.baker_base_dir ~sandbox_name ~baker_instance in
  let try_remove f = try Unix.unlink f with Unix.Unix_error _ -> () in
  (* Highwatermark files are chain-ID-prefixed, e.g. NetXsqzbfFenS_highwatermarks.
     Delete all matching files in the base dir. *)
  try
    Sys.readdir base
    |> Array.iter (fun fname ->
        if
          String.length fname > 16
          &&
          let suffix = "_highwatermarks" in
          let sl = String.length suffix in
          let fl = String.length fname in
          fl >= sl && String.equal (String.sub fname (fl - sl) sl) suffix
        then try_remove (Filename.concat base fname))
  with Sys_error _ -> ()

let apply_allocation s =
  let group_name = s.group_name in
  let node_instance =
    match Sandbox.find_sandbox_nodes ~group_name with
    | Ok (first :: _) -> first.Service.instance
    | Ok [] | Error _ -> Printf.sprintf "%s-node" group_name
  in
  let desc = Printf.sprintf "Apply key allocation for %s" group_name in
  Context.toast_info (T.text "%s..." desc) ;
  Job_manager.submit
    ~timeout:None
    ~description:desc
    ~on_complete:(fun _ -> Context.mark_instances_dirty ())
    (fun ~append_log () ->
      let log msg = append_log (msg ^ "\n") in
      let to_delete = List.filter (fun r -> r.to_delete) s.rows in
      let existing =
        List.filter (fun r -> (not r.is_new) && not r.to_delete) s.rows
      in
      let to_add = List.filter (fun r -> r.is_new && r.keys > 0) s.rows in
      (* Stop + remove bakers marked for deletion *)
      let result =
        List.fold_left
          (fun acc r ->
            match acc with
            | Error _ -> acc
            | Ok () ->
                log (Printf.sprintf "  deleting %s" r.instance) ;
                let _ = Systemd.stop ~role:"baker" ~instance:r.instance () in
                Removal.remove_service
                  ~quiet:true
                  ~delete_data_dir:true
                  ~instance:r.instance
                  ())
          (Ok ())
          to_delete
      in
      (* Update env + restart each baker using patch_keys to avoid double-encoding *)
      let offset = ref 0 in
      let result =
        match result with
        | Error _ -> result
        | Ok () ->
            List.fold_left
              (fun acc r ->
                match acc with
                | Error _ -> acc
                | Ok () -> (
                    let delegates =
                      delegate_aliases_for_slice ~start:!offset ~count:r.keys
                    in
                    offset := !offset + r.keys ;
                    log
                      (Printf.sprintf
                         "  updating %s: %d keys"
                         r.instance
                         r.keys) ;
                    let updates =
                      [
                        ( "OCTEZ_BAKER_DELEGATES_CSV",
                          String.concat "," delegates );
                        ( "OCTEZ_BAKER_DELEGATES_ARGS",
                          String.concat " " delegates );
                      ]
                    in
                    match Node_env.patch_keys ~inst:r.instance ~updates with
                    | Error _ as e -> e
                    | Ok () ->
                        clear_highwatermarks
                          ~sandbox_name:group_name
                          ~baker_instance:r.instance ;
                        log (Printf.sprintf "  restarting %s" r.instance) ;
                        Systemd.restart ~role:"baker" ~instance:r.instance ()))
              (Ok ())
              existing
      in
      match result with
      | Error _ -> result
      | Ok () ->
          (* Install new bakers (offset continues from where existing left off) *)
          List.fold_left
            (fun acc r ->
              match acc with
              | Error _ -> acc
              | Ok () ->
                  let delegates =
                    delegate_aliases_for_slice ~start:!offset ~count:r.keys
                  in
                  offset := !offset + r.keys ;
                  log
                    (Printf.sprintf
                       "  installing %s: %d keys"
                       r.instance
                       r.keys) ;
                  Sandbox.add_baker
                    ~on_log:log
                    ~group_name
                    ~node_instance
                    ~delegates
                    ()
                  |> Result.map ignore)
            (Ok ())
            to_add) ;
  Context.navigate_back ()

(* ─── View ──────────────────────────────────────────────────────────────── *)

let key_hint_pairs = Sandbox_key_alloc_view.key_hint_pairs

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  Context.tick_spinner () ;
  Context.tick_toasts () ;
  let cols = size.LTerm_geom.cols in
  let toast = Context.render_toasts ~cols in
  Sandbox_key_alloc_view.view s ~toast ~focus ~size

(* ─── Key Handling ──────────────────────────────────────────────────────── *)

let commit_edit s idx field txt =
  let rows =
    List.mapi
      (fun i r ->
        if i <> idx then r
        else
          let keys =
            match field with
            | EKeys -> (
                match int_of_string_opt (String.trim txt) with
                | Some n when n >= 0 -> n
                | _ -> r.keys)
            | EPct -> (
                let clean =
                  String.trim txt |> fun t ->
                  if String.length t > 0 && t.[String.length t - 1] = '%' then
                    String.sub t 0 (String.length t - 1)
                  else t
                in
                match float_of_string_opt clean with
                | Some p when p >= 0.0 && p <= 100.0 ->
                    max
                      0
                      (Float.to_int
                         (Float.round
                            (p /. 100.0 *. float_of_int s.total_delegates)))
                | _ -> r.keys)
          in
          {r with keys})
      s.rows
  in
  {s with rows; editing = None; user_modified = true}

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    let s = ps.Navigation.s in
    let nav f = Navigation.update f ps in
    match s.editing with
    | Some (idx, field, txt) ->
        nav (fun s ->
            match key with
            | "Return" | "Enter" -> commit_edit s idx field txt
            | "Escape" -> {s with editing = None}
            | "BackSpace" ->
                let new_txt =
                  if String.length txt = 0 then ""
                  else String.sub txt 0 (String.length txt - 1)
                in
                {s with editing = Some (idx, field, new_txt)}
            | c
              when String.length c = 1
                   && ((c.[0] >= '0' && c.[0] <= '9')
                      || (c.[0] = '.' && field = EPct)) ->
                {s with editing = Some (idx, field, txt ^ c)}
            | _ -> s)
    | None -> (
        match key with
        | "Escape" | "q" -> Navigation.back ps
        | "j" | "Down" ->
            nav (fun s -> {s with cursor = clamp_cursor s.rows (s.cursor + 1)})
        | "k" | "Up" ->
            nav (fun s -> {s with cursor = clamp_cursor s.rows (s.cursor - 1)})
        | "Return" | "Enter" ->
            let cur = s.cursor in
            if cur < List.length s.rows then
              nav (fun s -> {s with editing = Some (cur, EKeys, "")})
            else ps
        | "p" ->
            let cur = s.cursor in
            if cur < List.length s.rows then
              nav (fun s -> {s with editing = Some (cur, EPct, "")})
            else ps
        | "a" ->
            let new_row =
              {
                instance =
                  Printf.sprintf
                    "%s-baker-%d"
                    s.group_name
                    (List.length s.rows + 1);
                keys = 0;
                is_new = true;
                to_delete = false;
              }
            in
            nav (fun s ->
                let rows = s.rows @ [new_row] in
                {
                  s with
                  rows;
                  user_modified = true;
                  cursor = clamp_cursor rows (List.length rows - 1);
                })
        | "d" -> (
            let cur = s.cursor in
            match List.nth_opt s.rows cur with
            | None -> ps
            | Some row ->
                if row.is_new then
                  nav (fun s ->
                      let rows = List.filteri (fun i _ -> i <> cur) s.rows in
                      {
                        s with
                        rows;
                        user_modified = true;
                        cursor = clamp_cursor rows cur;
                      })
                else
                  nav (fun s ->
                      let rows =
                        List.mapi
                          (fun i r ->
                            if i = cur then {r with to_delete = not r.to_delete}
                            else r)
                          s.rows
                      in
                      {s with rows; user_modified = true}))
        | "\t" | "Tab" -> nav redistribute
        | "c" ->
            let alloc = allocated_keys s.rows in
            let total = s.total_delegates in
            if alloc > total then (
              Context.toast_error
                (T.text
                   "Over-allocated by %d — use Tab to redistribute"
                   (alloc - total)) ;
              ps)
            else
              let lines =
                List.filter_map
                  (fun r ->
                    if r.to_delete then
                      Some (Printf.sprintf "  delete %s" r.instance)
                    else if r.is_new then
                      Some
                        (Printf.sprintf
                           "  install %s: %d keys"
                           r.instance
                           r.keys)
                    else
                      Some
                        (Printf.sprintf
                           "  restart %s: %d keys"
                           r.instance
                           r.keys))
                  s.rows
              in
              let unalloc = total - alloc in
              let summary =
                if unalloc = 0 then
                  Printf.sprintf "\n%d / %d delegates allocated" alloc total
                else
                  Printf.sprintf
                    "\n%d / %d allocated, %d unallocated (will be unused)"
                    alloc
                    total
                    unalloc
              in
              Modal_helpers.confirm_modal
                ~title:"Apply Reallocation?"
                ~message:(String.concat "\n" (lines @ [summary]))
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
      Char "p";
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
    kb "Enter" "Edit keys";
    kb "p" "Edit stake %";
    kb "a" "Add baker";
    kb "d" "Delete/toggle";
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
