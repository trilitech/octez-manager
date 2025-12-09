open Octez_manager_lib
module Metrics = Rpc_metrics
module Rpc = Rpc_client
module Bg = Background_runner

let submit_blocking_ref = ref Bg.submit_blocking

let now_ref = ref Unix.gettimeofday

let bg_inflight = ref 0

let bg_inflight_cap = ref 4

let bg_lock = Mutex.create ()

(* Polling cadence *)
let boot_pending_interval = 6.0

let boot_ok_interval = 10.0

(* Enforce a global pacing so different nodes do not fire at the exact same
  moment. *)
let min_spacing = 1.0

let last_boot_at : (string, float) Hashtbl.t = Hashtbl.create 17

let last_boot_state : (string, bool option) Hashtbl.t = Hashtbl.create 17

let last_global_rpc = ref 0.0

let head_monitors : (string, Rpc_client.monitor_handle) Hashtbl.t =
  Hashtbl.create 17

let schedule (f : unit -> unit) : bool =
  let allowed =
    Mutex.lock bg_lock ;
    let allowed = !bg_inflight < !bg_inflight_cap in
    if allowed then incr bg_inflight ;
    Mutex.unlock bg_lock ;
    allowed
  in
  if allowed then (
    !submit_blocking_ref
      ~on_complete:(fun () ->
        Mutex.lock bg_lock ;
        decr bg_inflight ;
        Mutex.unlock bg_lock)
      (fun () -> try f () with _ -> ()) ;
    true)
  else false

let poll_boot (svc : Service.t) now =
  let boot = Rpc.rpc_is_bootstrapped svc in
  let prev =
    Hashtbl.find_opt last_boot_state svc.Service.instance |> Option.join
  in
  (* Toast on bootstrap state change *)
  (match (prev, boot) with
  | Some false, Some true ->
      Context.toast_success
        (Printf.sprintf "%s is now synced" svc.Service.instance)
  | Some true, Some false ->
      Context.toast_warn (Printf.sprintf "%s lost sync" svc.Service.instance)
  | None, Some true ->
      Context.toast_success (Printf.sprintf "%s is synced" svc.Service.instance)
  | _ -> ()) ;
  Hashtbl.replace last_boot_state svc.Service.instance boot ;
  let existing = Metrics.get ~instance:svc.Service.instance in
  let node_version =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.node_version ->
        m.Rpc_metrics.node_version
    | _ -> Rpc.node_version svc
  in
  (* Fetch fresh head/proto/chain during sync when head monitor isn't streaming *)
  let head_level =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.head_level ->
        m.Rpc_metrics.head_level
    | _ -> Rpc.rpc_head_header svc
  in
  let proto =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.proto -> m.Rpc_metrics.proto
    | _ -> Rpc.rpc_protocol svc
  in
  let last_error = Rpc.rpc_last_error svc in
  let chain_id =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.chain_id ->
        m.Rpc_metrics.chain_id
    | _ -> Rpc.rpc_chain_id svc
  in
  Metrics.set
    ~instance:svc.Service.instance
    {
      Rpc_metrics.chain_id;
      head_level;
      bootstrapped = boot;
      last_rpc_refresh = Some now;
      node_version;
      data_size = None;
      proto;
      last_error;
    } ;
  Context.mark_instances_dirty ()

let start_head_monitor (svc : Service.t) =
  let instance = svc.Service.instance in
  (* Remove stale monitor if it's no longer alive *)
  (match Hashtbl.find_opt head_monitors instance with
  | Some h when not (h.Rpc_client.alive ()) ->
      Hashtbl.remove head_monitors instance
  | _ -> ()) ;
  if Hashtbl.mem head_monitors instance then ()
  else
    let on_disconnect () = Hashtbl.remove head_monitors instance in
    let handle =
      Rpc.start_head_monitor
        svc
        ~on_disconnect
        ~on_head:(fun ~level ~proto ~chain_id ->
          let now = Unix.gettimeofday () in
          let boot = Rpc.rpc_is_bootstrapped svc in
          Hashtbl.replace last_boot_state instance boot ;
          let existing = Metrics.get ~instance in
          let node_version =
            match existing with
            | Some m when Option.is_some m.Rpc_metrics.node_version ->
                m.Rpc_metrics.node_version
            | _ -> Rpc.node_version svc
          in
          let chain_id =
            match chain_id with
            | Some _ as v -> v
            | None -> (
                match
                  Option.bind existing (fun m -> m.Rpc_metrics.chain_id)
                with
                | Some _ as v -> v
                | None -> Rpc.rpc_chain_id svc)
          in
          let proto =
            match proto with
            | Some _ as v -> v
            | None -> (
                match Option.bind existing (fun m -> m.Rpc_metrics.proto) with
                | Some _ as v -> v
                | None -> Rpc.rpc_protocol svc)
          in
          let last_error = Rpc.rpc_last_error svc in
          Metrics.set
            ~instance
            {
              Rpc_metrics.chain_id;
              head_level = level;
              bootstrapped = boot;
              last_rpc_refresh = Some now;
              node_version;
              data_size = None;
              proto;
              last_error;
            } ;
          Context.mark_instances_dirty ())
    in
    Hashtbl.replace head_monitors instance handle

let stop_head_monitor instance =
  match Hashtbl.find_opt head_monitors instance with
  | None -> ()
  | Some h ->
      h.stop () ;
      Hashtbl.remove head_monitors instance

let poll_interval instance =
  match Hashtbl.find_opt last_boot_state instance |> Option.join with
  | Some true -> boot_ok_interval
  | _ -> boot_pending_interval

let due_polls now nodes =
  nodes
  |> List.filter (fun (svc : Service.t) ->
      match Hashtbl.find_opt last_boot_at svc.Service.instance with
      | None -> true
      | Some last -> now -. last >= poll_interval svc.Service.instance)

let poll_boot_ref = ref poll_boot

let dispatch polls =
  let rec loop = function
    | [] -> ()
    | svc :: rest ->
        let now = !now_ref () in
        let since_last = now -. !last_global_rpc in
        if since_last < min_spacing then loop rest
        else
          let submitted = schedule (fun () -> !poll_boot_ref svc now) in
          if submitted then (
            Hashtbl.replace last_boot_at svc.Service.instance now ;
            last_global_rpc := now) ;
          loop rest
  in
  loop polls

let tick () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Service_manager_capability.key
  with
  | Some cap ->
      let module SM =
        (val (cap : Manager_interfaces.Service_manager_capability.t))
      in
      let nodes = match SM.list () with Ok l -> l | Error _ -> [] in
      let nodes =
        nodes
        |> List.filter (fun (svc : Service.t) -> String.equal svc.role "node")
      in
      let now = !now_ref () in
      List.iter (fun (svc : Service.t) -> start_head_monitor svc) nodes ;
      dispatch (due_polls now nodes)
  | None ->
      let nodes =
        Data.load_service_states ()
        |> List.map (fun st -> st.Data.Service_state.service)
        |> List.filter (fun (svc : Service.t) -> String.equal svc.role "node")
      in
      let now = !now_ref () in
      List.iter (fun (svc : Service.t) -> start_head_monitor svc) nodes ;
      List.iter
        (fun svc ->
          let submitted = schedule (fun () -> !poll_boot_ref svc now) in
          if submitted then (
            Hashtbl.replace last_boot_at svc.Service.instance now ;
            last_global_rpc := now))
        nodes

let started = ref false

let rec loop () =
  tick () ;
  Unix.sleepf 1.0 ;
  loop ()

let start () =
  if not !started then (
    started := true ;
    Bg.submit_blocking (fun () -> loop ()))

module For_tests = struct
  let reset_state () =
    bg_inflight := 0 ;
    Hashtbl.reset last_boot_at ;
    Hashtbl.reset last_boot_state ;
    Hashtbl.reset head_monitors ;
    last_global_rpc := 0.0 ;
    bg_inflight_cap := 4 ;
    now_ref := Unix.gettimeofday ;
    submit_blocking_ref := Bg.submit_blocking ;
    poll_boot_ref := poll_boot

  let with_submit_blocking stub f =
    let original = !submit_blocking_ref in
    submit_blocking_ref := stub ;
    Fun.protect ~finally:(fun () -> submit_blocking_ref := original) f

  let with_now now f =
    let original = !now_ref in
    now_ref := now ;
    Fun.protect ~finally:(fun () -> now_ref := original) f

  let with_poll_boot stub f =
    let original = !poll_boot_ref in
    poll_boot_ref := stub ;
    Fun.protect ~finally:(fun () -> poll_boot_ref := original) f

  let set_bg_cap n = bg_inflight_cap := n

  let set_last_global_rpc t = last_global_rpc := t

  let dispatch = dispatch
end
