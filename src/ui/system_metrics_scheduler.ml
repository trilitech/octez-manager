(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for collecting system metrics.

    Polls CPU, memory, disk usage, and version information at
    configurable intervals. Uses sparklines to display history. *)

open Octez_manager_lib
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context

let shutdown_requested = Atomic.make false

(** Metrics storage per instance *)
type instance_state = {
  mutable version : string option;
  mutable last_version_check : float option;
      (** Last version check timestamp *)
  mutable pids : int list;
  mutable last_pid_check : float;
  cpu_samples : (int, System_metrics.cpu_sample) Hashtbl.t;
  mutable cpu_history : float list;  (** Recent CPU values for line chart *)
  mem_sparkline : Miaou_widgets_display.Sparkline_widget.t;
  mutable memory_rss : int64;
  mutable data_dir_size : int64 option;
  mutable last_cpu_poll : float;
  mutable last_mem_poll : float;
  mutable last_disk_poll : float;
}

(** Chart configuration *)
let cpu_chart_width = 20

let cpu_chart_height = 3

let cpu_max_points = 40

let mem_sparkline_width = 10

let mem_max_points = 20

(** Create a new instance state *)
let make_instance_state () =
  {
    version = None;
    last_version_check = None;
    pids = [];
    last_pid_check = 0.0;
    cpu_samples = Hashtbl.create 17;
    cpu_history = [];
    mem_sparkline =
      Miaou_widgets_display.Sparkline_widget.create
        ~width:mem_sparkline_width
        ~max_points:mem_max_points
        ~min_value:0.0
        ();
    memory_rss = 0L;
    data_dir_size = None;
    last_cpu_poll = 0.0;
    last_mem_poll = 0.0;
    last_disk_poll = 0.0;
  }

(** Storage and lock *)
let table : (string, instance_state) Hashtbl.t = Hashtbl.create 17

let lock = Mutex.create ()

(** Track which instances we've already warned about version *)
let version_warned : (string, unit) Hashtbl.t = Hashtbl.create 17

(** Forward ref for version toast check (defined later after version parsing) *)
let check_version_toast_ref :
    (key:string -> instance:string -> version:string -> unit) ref =
  ref (fun ~key:_ ~instance:_ ~version:_ -> ())

(** Poll intervals *)
let cpu_interval = 0.5 (* 500ms *)

let mem_interval = 1.0 (* 1s *)

let disk_interval = 5.0 (* 5s *)

let pid_check_interval = 5.0 (* Check PIDs every 5s *)

(** Multiplier for hidden instances (folded or off-screen) *)
let hidden_interval_multiplier = 4.0

(** Track which instances are currently visible (unfolded and on-screen) *)
let visible_instances : (string, unit) Hashtbl.t = Hashtbl.create 17

let visible_lock = Mutex.create ()

(** Mark an instance as visible (called from UI during render) *)
let mark_visible ~role ~instance =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect visible_lock (fun () ->
      Hashtbl.replace visible_instances key ())

(** Mark an instance as hidden.
    Note: Currently unused as clear_visibility is called each render pass,
    but kept for API completeness if explicit hiding is needed in future. *)
let mark_hidden ~role ~instance =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect visible_lock (fun () -> Hashtbl.remove visible_instances key)

(** Clear all visibility (called at start of render pass) *)
let clear_visibility () =
  Mutex.protect visible_lock (fun () -> Hashtbl.clear visible_instances)

(** Check if an instance is visible *)
let is_visible key =
  Mutex.protect visible_lock (fun () -> Hashtbl.mem visible_instances key)

(** Get effective interval based on visibility *)
let effective_interval ~key ~base_interval =
  if is_visible key then base_interval
  else base_interval *. hidden_interval_multiplier

(** Get or create instance state *)
let get_state key =
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt table key with
      | Some s -> s
      | None ->
          let s = make_instance_state () in
          Hashtbl.replace table key s ;
          s)

(** Update PIDs and version if changed *)
let update_pids_and_version ~key ~role ~instance ~binary ~data_dir ?unit_name
    state now =
  let interval = effective_interval ~key ~base_interval:pid_check_interval in
  if now -. state.last_pid_check < interval then ()
  else
    let new_pids =
      match unit_name with
      | Some unit -> System_metrics.get_pids_by_unit ~unit_name:unit
      | None -> System_metrics.get_service_pids ~role ~instance
    in
    let pids_changed =
      List.length new_pids <> List.length state.pids
      || List.exists2
           ( <> )
           (List.sort compare new_pids)
           (List.sort compare state.pids)
    in
    state.pids <- new_pids ;
    (* Refresh version when PIDs change (service restarted) OR periodically *)
    let old_version = state.version in
    let time_since_version_check =
      match state.last_version_check with
      | None -> Float.infinity
      | Some t -> now -. t
    in
    let should_refresh_version =
      pids_changed
      || Option.is_none state.version
      || time_since_version_check > 300.0 (* Re-check every 5 minutes *)
    in
    if should_refresh_version then (
      state.version <-
        (if String.equal role "index" then
           (* octez-index --version reports "Octez 0.0+dev" regardless of
              release; read the actual version from the versioned install
              directory (e.g. ".../v0.1.0/octez-index" → "0.1.0"). *)
           let dir_name = Filename.basename (Filename.dirname binary) in
           if String.length dir_name > 1 && dir_name.[0] = 'v' then
             Some (String.sub dir_name 1 (String.length dir_name - 1))
           else System_metrics.get_version ~binary
         else System_metrics.get_version ~binary) ;
      state.last_version_check <- Some now) ;
    (* Check version and show toast if outdated (on first detection or change) *)
    (match (old_version, state.version) with
    | _, None -> ()
    | old, Some v when old <> Some v ->
        (* Version changed or first detection - check and toast *)
        Hashtbl.remove version_warned key ;
        !check_version_toast_ref ~key ~instance ~version:v
    | _ -> ()) ;
    (* Update disk size on PID check *)
    state.data_dir_size <- File_ops.get_dir_size data_dir ;
    (* Update timestamp at completion to spread load across instances *)
    state.last_pid_check <- Unix.gettimeofday ()

(** Poll CPU for all tracked PIDs *)
let poll_cpu ~key state now =
  let interval = effective_interval ~key ~base_interval:cpu_interval in
  if now -. state.last_cpu_poll < interval then ()
  else
    let total_cpu = ref 0.0 in
    List.iter
      (fun pid ->
        let prev = Hashtbl.find_opt state.cpu_samples pid in
        match System_metrics.get_process_stats ~pid ~prev_sample:prev with
        | None -> Hashtbl.remove state.cpu_samples pid
        | Some (stats, new_sample) ->
            Hashtbl.replace state.cpu_samples pid new_sample ;
            total_cpu := !total_cpu +. stats.cpu_percent)
      state.pids ;
    let cpu = !total_cpu in
    (* Add to history, keep last N points *)
    let history = state.cpu_history @ [cpu] in
    let len = List.length history in
    state.cpu_history <-
      (if len > cpu_max_points then
         List.filteri (fun i _ -> i >= len - cpu_max_points) history
       else history) ;
    (* Update timestamp at completion to spread load across instances *)
    state.last_cpu_poll <- Unix.gettimeofday ()

(** Poll memory for all tracked PIDs *)
let poll_mem ~key state now =
  let interval = effective_interval ~key ~base_interval:mem_interval in
  if now -. state.last_mem_poll < interval then ()
  else
    let total_rss = ref 0L in
    List.iter
      (fun pid ->
        match System_metrics.read_proc_stat ~pid with
        | None -> ()
        | Some (_, _, rss) -> total_rss := Int64.add !total_rss rss)
      state.pids ;
    state.memory_rss <- !total_rss ;
    let mem_mb = Int64.to_float !total_rss /. 1048576.0 in
    Miaou_widgets_display.Sparkline_widget.push state.mem_sparkline mem_mb ;
    (* Update timestamp at completion to spread load across instances *)
    state.last_mem_poll <- Unix.gettimeofday ()

(** Poll disk size *)
let poll_disk ~key ~data_dir state now =
  let interval = effective_interval ~key ~base_interval:disk_interval in
  if now -. state.last_disk_poll < interval then ()
  else (
    state.data_dir_size <- File_ops.get_dir_size data_dir ;
    (* Update timestamp at completion to spread load across instances *)
    state.last_disk_poll <- Unix.gettimeofday ())

(** Poll all metrics for an instance *)
let poll ~role ~instance ~binary ~data_dir ?unit_name () =
  let key = Printf.sprintf "%s/%s" role instance in
  let state = get_state key in
  let now = Unix.gettimeofday () in
  update_pids_and_version
    ~key
    ~role
    ~instance
    ~binary
    ~data_dir
    ?unit_name
    state
    now ;
  poll_cpu ~key state now ;
  poll_mem ~key state now ;
  poll_disk ~key ~data_dir state now

(** Render CPU line chart (multi-row braille) *)
let render_cpu_chart ~role ~instance ~focus:_ =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt table key with
      | None -> None
      | Some state ->
          if state.cpu_history = [] then None
          else
            let text_color =
              let resolved = Style.to_resolved (Style_context.text ()) in
              if resolved.r_fg < 0 then None
              else Some ("38;5;" ^ string_of_int resolved.r_fg)
            in
            let warn_color =
              let resolved = Style.to_resolved (Style_context.warning ()) in
              if resolved.r_fg < 0 then "33"
              else "38;5;" ^ string_of_int resolved.r_fg
            in
            let error_color =
              let resolved = Style.to_resolved (Style_context.error ()) in
              if resolved.r_fg < 0 then "31"
              else "38;5;" ^ string_of_int resolved.r_fg
            in
            (* Convert history to points *)
            let points =
              List.mapi
                (fun i y ->
                  Miaou_widgets_display.Line_chart_widget.
                    {x = Float.of_int i; y; color = None})
                state.cpu_history
            in
            let series =
              Miaou_widgets_display.Line_chart_widget.
                [{label = "cpu"; points; color = text_color}]
            in
            let chart =
              Miaou_widgets_display.Line_chart_widget.create
                ~width:cpu_chart_width
                ~height:cpu_chart_height
                ~series
                ()
            in
            let thresholds =
              Miaou_widgets_display.Line_chart_widget.
                [
                  {value = 90.0; color = error_color};
                  {value = 75.0; color = warn_color};
                ]
            in
            let rendered =
              Miaou_widgets_display.Line_chart_widget.render
                chart
                ~show_axes:false
                ~show_grid:false
                ~thresholds
                ~mode:Braille
                ()
            in
            (* Calculate average *)
            let avg =
              let sum = List.fold_left ( +. ) 0.0 state.cpu_history in
              sum /. Float.of_int (List.length state.cpu_history)
            in
            Some (rendered, avg))

(** Render memory sparkline *)
let render_mem_sparkline ~role ~instance ~focus =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt table key with
      | None -> ""
      | Some state ->
          if Miaou_widgets_display.Sparkline_widget.is_empty state.mem_sparkline
          then ""
          else
            let spark_color =
              let resolved = Style.to_resolved (Style_context.text ()) in
              if resolved.r_fg < 0 then None
              else Some ("38;5;" ^ string_of_int resolved.r_fg)
            in
            let value_str = System_metrics.format_bytes state.memory_rss in
            let chart =
              Miaou_widgets_display.Sparkline_widget.render
                state.mem_sparkline
                ~focus
                ~show_value:false
                ?color:spark_color
                ()
            in
            Printf.sprintf "%s %s" chart value_str)

(** Get current version *)
let get_version ~role ~instance =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt table key with
      | None -> None
      | Some state -> state.version)

(** Invalidate version cache - forces refresh on next poll *)
let invalidate_version ~role ~instance =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt table key with
      | None ->
          (* Not in cache yet, nothing to do *)
          ()
      | Some state ->
          state.version <- None ;
          state.last_pid_check <- 0.0)

(** Get disk size *)
let get_disk_size ~role ~instance =
  let key = Printf.sprintf "%s/%s" role instance in
  Mutex.protect lock (fun () ->
      match Hashtbl.find_opt table key with
      | None -> None
      | Some state -> state.data_dir_size)

(** Worker queue for processing poll requests one at a time with dedup *)
let worker : unit Worker_queue.t = Worker_queue.create ~name:"system_metrics" ()

(** Submit a poll request to the worker queue *)
let submit_poll ~role ~instance ~binary ~data_dir ?unit_name () =
  let key = Printf.sprintf "poll:%s/%s" role instance in
  Worker_queue.submit_unit worker ~key ~work:(fun () ->
      try poll ~role ~instance ~binary ~data_dir ?unit_name () with _ -> ())

(** Submit a DAL health check to the worker queue *)
let submit_dal_health ~instance ~rpc_endpoint =
  let key = Printf.sprintf "dal-health:%s" instance in
  Worker_queue.submit_unit worker ~key ~work:(fun () ->
      try
        match Dal_health.fetch ~rpc_endpoint with
        | Some health -> Dal_health.set ~instance health
        | None -> ()
      with _ -> ())

(** Tick - submit poll requests for all instances *)
let tick () =
  let states = Data.load_service_states () in
  (* Submit poll requests for nodes *)
  states
  |> List.iter (fun (st : Data.Service_state.t) ->
      let svc = st.service in
      match svc.Service.role with
      | "node" ->
          let binary = Filename.concat svc.Service.app_bin_dir "octez-node" in
          submit_poll
            ~role:svc.Service.role
            ~instance:svc.Service.instance
            ~binary
            ~data_dir:svc.Service.data_dir
            ()
      | "baker" ->
          let binary = Filename.concat svc.Service.app_bin_dir "octez-baker" in
          submit_poll
            ~role:svc.Service.role
            ~instance:svc.Service.instance
            ~binary
            ~data_dir:""
            ()
      | "dal-node" ->
          let binary =
            Filename.concat svc.Service.app_bin_dir "octez-dal-node"
          in
          submit_poll
            ~role:svc.Service.role
            ~instance:svc.Service.instance
            ~binary
            ~data_dir:svc.Service.data_dir
            () ;
          (* Also submit DAL health check *)
          let rpc_endpoint = Rpc_addr.to_endpoint svc.Service.rpc_addr in
          submit_dal_health ~instance:svc.Service.instance ~rpc_endpoint
      | "accuser" ->
          (* Accuser runs via octez-baker run accuser *)
          let binary = Filename.concat svc.Service.app_bin_dir "octez-baker" in
          submit_poll
            ~role:svc.Service.role
            ~instance:svc.Service.instance
            ~binary
            ~data_dir:""
            ()
      | "index" ->
          let binary =
            Filename.concat svc.Service.app_bin_dir "octez-index"
          in
          (* Read OCTEZ_INDEXER_DIR for disk usage tracking *)
          let data_dir =
            match Node_env.read ~inst:svc.Service.instance with
            | Error _ -> ""
            | Ok pairs -> (
                match List.assoc_opt "OCTEZ_INDEXER_DIR" pairs with
                | None | Some "" -> ""
                | Some d -> d)
          in
          submit_poll
            ~role:svc.Service.role
            ~instance:svc.Service.instance
            ~binary
            ~data_dir
            ()
      | _ -> ())

let started = ref false

(** Latest stable Octez version string from feed (e.g., "23.3") *)
let latest_stable_version : string option ref = ref None

(** Latest stable octez-index version from GitLab releases (e.g., "1.0.0") *)
let latest_stable_index_version : string option ref = ref None

(** Fetch and parse latest stable version from octez releases JSON *)
let fetch_latest_version () =
  let url = "https://octez.tezos.com/releases/versions.json" in
  match Cmd_runner.run_out ["curl"; "-sfL"; "--max-time"; "10"; url] with
  | Error _ -> ()
  | Ok json -> (
      try
        let data = Yojson.Safe.from_string json in
        (* Find entry with "latest": true and no "rc" field *)
        match data with
        | `List versions -> (
            let latest =
              List.find_opt
                (fun v ->
                  let is_latest =
                    match Yojson.Safe.Util.member "latest" v with
                    | `Bool true -> true
                    | _ -> false
                  in
                  let is_rc =
                    match Yojson.Safe.Util.member "rc" v with
                    | `Null -> false
                    | _ -> true
                  in
                  is_latest && not is_rc)
                versions
            in
            match latest with
            | Some v -> (
                let major =
                  Yojson.Safe.Util.member "major" v
                  |> Yojson.Safe.Util.to_int_option
                in
                let minor =
                  Yojson.Safe.Util.member "minor" v
                  |> Yojson.Safe.Util.to_int_option
                in
                match (major, minor) with
                | Some maj, Some min ->
                    latest_stable_version :=
                      Some (Printf.sprintf "%d.%d" maj min)
                | _ -> ())
            | None -> ())
        | _ -> ()
      with _ -> ())

(** Fetch latest octez-index version from GitLab releases *)
let fetch_latest_index_version () =
  match Octez_index_downloader.fetch_versions ~include_prerelease:false () with
  | Error _ -> ()
  | Ok [] -> ()
  | Ok (latest :: _) ->
      latest_stable_index_version :=
        Some latest.Octez_index_downloader.version

(** Version status for coloring *)
type version_status =
  | Latest  (** Running latest stable *)
  | MinorBehind  (** Same major, older minor *)
  | MajorBehind  (** Older major version *)
  | DevOrRC  (** Running dev or RC version *)
  | Unknown  (** Can't determine *)

(** Compare running version against an explicit latest version *)
let check_version_status ~running ~latest =
  if Version_utils.is_rc running then DevOrRC
  else
    match latest with
    | None -> Unknown
    | Some latest -> (
        (* Check if the running version can be parsed at all *)
        match Version_utils.parse_version running with
        | [] -> Unknown
        | _ ->
            let cmp = Version_utils.compare_versions running latest in
            if cmp >= 0 then Latest
            else
              (* Running is older; determine if major or minor behind *)
              let running_parts = Version_utils.parse_version running in
              let latest_parts = Version_utils.parse_version latest in
              let running_major =
                match running_parts with x :: _ -> Some x | [] -> None
              in
              let latest_major =
                match latest_parts with x :: _ -> Some x | [] -> None
              in
              if running_major <> latest_major then MajorBehind else MinorBehind
        )

(** Get ANSI color for version status *)
let version_color status =
  match status with
  | Latest -> Some (Miaou_style.Style_context.success ())
  | MinorBehind -> Some (Miaou_style.Style_context.warning ())
  | MajorBehind -> Some (Miaou_style.Style_context.error ())
  | DevOrRC -> Some (Miaou_style.Style_context.info ())
  | Unknown -> None

(** Format version with color based on status *)
let format_version_colored version =
  let status =
    check_version_status ~running:version ~latest:!latest_stable_version
  in
  match version_color status with
  | None -> Printf.sprintf "v%s" version
  | Some style -> Miaou_style.Style.render style (Printf.sprintf "v%s" version)

(** Format octez-index version using its own GitLab release feed *)
let format_index_version_colored version =
  let status =
    check_version_status
      ~running:version
      ~latest:!latest_stable_index_version
  in
  match version_color status with
  | None -> Printf.sprintf "v%s" version
  | Some style -> Miaou_style.Style.render style (Printf.sprintf "v%s" version)

(** Check version for a single instance and show toast if outdated *)
let check_version_toast_for ~key ~instance ~version =
  (* Determine the right feed based on role (key = "role/instance") *)
  let role =
    match String.index_opt key '/' with
    | Some i -> String.sub key 0 i
    | None -> ""
  in
  let latest_ver_opt =
    if String.equal role "index" then !latest_stable_index_version
    else !latest_stable_version
  in
  match latest_ver_opt with
  | None -> ()
  | Some latest_ver -> (
      if Hashtbl.mem version_warned key then ()
      else
        let status = check_version_status ~running:version ~latest:latest_ver_opt in
        match status with
        | MinorBehind ->
            Hashtbl.replace version_warned key () ;
            Context.toast_warn
              (Printf.sprintf
                 "%s: v%s is outdated (latest: %s)"
                 instance
                 version
                 latest_ver)
        | MajorBehind ->
            Hashtbl.replace version_warned key () ;
            Context.toast_error
              (Printf.sprintf
                 "%s: v%s is deprecated (latest: %s)"
                 instance
                 version
                 latest_ver)
        | _ -> ())

(* Wire up the forward reference *)
let () = check_version_toast_ref := check_version_toast_for

let start () =
  if not !started then (
    started := true ;
    (* Start the worker that processes poll requests *)
    Worker_queue.start worker ;
    (* Fetch latest version in pool fiber - don't block worker queue *)
    Domain_pool.submit (fun () -> try fetch_latest_version () with _ -> ()) ;
    Domain_pool.submit (fun () ->
        try fetch_latest_index_version () with _ -> ()) ;
    (* Submit scheduler fiber to domain pool.
       Runs first tick immediately (no initial delay) to start populating
       metrics as early as possible. *)
    Domain_pool.submit (fun () ->
        Metrics.record_scheduler_tick ~scheduler:"system_metrics" tick ;
        while not (Atomic.get shutdown_requested) do
          Eio_unix.sleep 0.5 ;
          Metrics.record_scheduler_tick ~scheduler:"system_metrics" tick
        done))

(** Clear all state *)
let clear () = Mutex.protect lock (fun () -> Hashtbl.clear table)

let shutdown () =
  Atomic.set shutdown_requested true ;
  Worker_queue.stop worker

(** Get worker queue stats *)
let get_worker_stats () = Worker_queue.get_stats worker

(** {2 Testing Interface} *)

module For_test = struct
  type version_status_t = version_status =
    | Latest
    | MinorBehind
    | MajorBehind
    | DevOrRC
    | Unknown

  let parse_version s =
    let s = String.trim s in
    match Version_utils.parse_version s with
    | maj :: min :: _ -> Some (maj, min)
    | [maj] -> Some (maj, 0)
    | [] -> None

  let is_rc_or_dev = Version_utils.is_rc

  let check_version_status ~running =
    check_version_status ~running ~latest:!latest_stable_version

  let version_color = version_color

  (** Set the latest stable version for testing *)
  let set_latest_version version = latest_stable_version := version

  (** Get the latest stable version *)
  let get_latest_version () = !latest_stable_version

  (** Clear all state for test isolation *)
  let clear_all () =
    Mutex.protect lock (fun () -> Hashtbl.clear table) ;
    Mutex.lock visible_lock ;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock visible_lock)
      (fun () -> Hashtbl.clear visible_instances) ;
    Hashtbl.clear version_warned ;
    latest_stable_version := None

  (** Direct access to visibility check *)
  let is_visible = is_visible

  (** Direct access to interval calculation *)
  let effective_interval = effective_interval

  (** Initialize an instance state for testing *)
  let init_instance ~role ~instance =
    let key = Printf.sprintf "%s/%s" role instance in
    Mutex.protect lock (fun () ->
        if not (Hashtbl.mem table key) then
          Hashtbl.replace table key (make_instance_state ()))

  (** Set version and last_pid_check for an instance (for testing) *)
  let set_instance_state ~role ~instance ~version ~last_pid_check =
    let key = Printf.sprintf "%s/%s" role instance in
    Mutex.protect lock (fun () ->
        match Hashtbl.find_opt table key with
        | None ->
            let state = make_instance_state () in
            state.version <- version ;
            state.last_pid_check <- last_pid_check ;
            Hashtbl.replace table key state
        | Some state ->
            state.version <- version ;
            state.last_pid_check <- last_pid_check)

  (** Get last_pid_check timestamp for an instance *)
  let get_last_pid_check ~role ~instance =
    let key = Printf.sprintf "%s/%s" role instance in
    Mutex.protect lock (fun () ->
        match Hashtbl.find_opt table key with
        | None -> None
        | Some state -> Some state.last_pid_check)
end
