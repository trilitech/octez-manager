open Rresult
open Octez_manager_lib
open Manager_interfaces
module Bg = Background_runner

module Service_state = struct
  type status = Running | Stopped | Unknown of string

  type t = {
    service : Service.t;
    enabled : bool option;
    active : bool option;
    status : status;
    status_text : string option;
  }

  let status_label {status; _} =
    match status with
    | Running -> "running"
    | Stopped -> "stopped"
    | Unknown msg -> Printf.sprintf "unknown (%s)" msg
end

module Summary = struct
  type t = {total : int; running : int; stopped : int; unknown : int}
end

let parse_enabled_response resp =
  let trimmed = String.lowercase_ascii (String.trim resp) in
  match trimmed with
  | "enabled" | "static" -> Some true
  | "disabled" -> Some false
  | _ -> None

let classify_active result =
  match result with
  | Ok true -> (Some true, Service_state.Running)
  | Ok false -> (Some false, Service_state.Stopped)
  | Error (`Msg msg) -> (None, Service_state.Unknown msg)

let fetch_status ?(detail = false) service =
  let role = service.Service.role in
  let instance = service.Service.instance in
  let active_raw = Systemd.is_active ~role ~instance in
  let active, status = classify_active active_raw in
  let enabled =
    match Systemd.is_enabled ~role ~instance with
    | Ok resp -> parse_enabled_response resp
    | Error _ -> None
  in
  let status_text =
    if detail then
      match Systemd.status ~role ~instance with
      | Ok text when String.trim text <> "" -> Some text
      | Ok _ -> None
      | Error (`Msg msg) -> Some msg
    else None
  in
  Service_state.{service; enabled; active; status; status_text}

let fetch_statuses ?detail services =
  let safe_fetch ?detail svc =
    try fetch_status ?detail svc
    with exn ->
      Service_state.
        {
          service = svc;
          enabled = None;
          active = None;
          status = Unknown (Printexc.to_string exn);
          status_text = None;
        }
  in
  List.map (safe_fetch ?detail) services

let load_service_states ?detail () =
  let module SM =
    (val Miaou_interfaces.Capability.require Service_manager_capability.key)
  in
  match SM.list () with
  | Ok services ->
      let states = fetch_statuses ?detail services in
      let now = Unix.gettimeofday () in
      List.iter
        (fun st ->
          let svc = st.Service_state.service in
          if String.equal svc.Service.role "node" then
            match Rpc_metrics.get ~instance:svc.instance with
            | None ->
                Rpc_metrics.set
                  ~instance:svc.instance
                  {
                    Rpc_metrics.chain_id = None;
                    head_level = None;
                    bootstrapped = None;
                    last_rpc_refresh = Some now;
                    node_version = None;
                    data_size = None;
                    proto = None;
                    last_error = None;
                  }
            | Some _ -> ())
        states ;
      states
  | Error (`Msg msg) ->
      prerr_endline (Printf.sprintf "Failed to read registry: %s" msg) ;
      []

let summarize states =
  let open Service_state in
  List.fold_left
    (fun (acc : Summary.t) state ->
      let running =
        if state.status = Running then acc.running + 1 else acc.running
      in
      let stopped =
        if state.status = Stopped then acc.stopped + 1 else acc.stopped
      in
      let unknown =
        match state.status with
        | Unknown _ -> acc.unknown + 1
        | _ -> acc.unknown
      in
      Summary.{total = acc.total + 1; running; stopped; unknown})
    Summary.{total = 0; running = 0; stopped = 0; unknown = 0}
    states

let split_lines text = String.split_on_char '\n' text

let diagnostics_lines states =
  let header = ["Diagnostics"; "-----------"] in
  let per_service state =
    let svc = state.Service_state.service in
    let head =
      Printf.sprintf
        "- %s (%s, %s) → %s%s"
        svc.Service.instance
        svc.Service.role
        svc.Service.network
        (Service_state.status_label state)
        (match state.enabled with
        | Some true -> " [enabled]"
        | Some false -> " [disabled]"
        | None -> "")
    in
    let details =
      match state.status_text with
      | None -> []
      | Some text -> List.map (fun line -> "    " ^ line) (split_lines text)
    in
    head :: details
  in
  header @ List.concat_map per_service states

let activity_lines states =
  let per_service state =
    let svc = state.Service_state.service in
    Printf.sprintf
      "%s  •  %s (%s) history=%s data=%s"
      svc.Service.created_at
      svc.Service.instance
      svc.Service.network
      (History_mode.to_string svc.Service.history_mode)
      svc.Service.data_dir
  in
  "Recent activity" :: "----------------" :: List.map per_service states

let formatted_timestamp ts =
  let tm = Unix.localtime ts in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let rec take n acc = function
  | [] -> List.rev acc
  | _ when n = 0 -> List.rev acc
  | x :: xs -> take (n - 1) (x :: acc) xs

let spotlight_lines states ~limit =
  let sorted =
    List.sort
      (fun a b ->
        String.compare
          a.Service_state.service.instance
          b.Service_state.service.instance)
      states
  in
  let focus = if limit <= 0 then sorted else take limit [] sorted in
  List.map
    (fun state ->
      let svc = state.Service_state.service in
      let marker =
        match state.status with
        | Service_state.Running -> "●"
        | Service_state.Stopped -> "○"
        | Service_state.Unknown _ -> "?"
      in
      Printf.sprintf "%s %s (%s)" marker svc.Service.instance svc.Service.role)
    focus
