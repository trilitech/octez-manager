module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys

let name = "jobs"

type state = {
  jobs : Job_manager.job list;
  selected : int;
  next_page : string option;
}

type msg = unit

let init () = {jobs = Job_manager.list (); selected = 0; next_page = None}

let update s _ = s

let check_navigation s =
  match Context.consume_navigation () with
  | Some p -> {s with next_page = Some p}
  | None -> s

let refresh s =
  let s = check_navigation s in
  {s with jobs = Job_manager.list ()}

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = refresh s

let back s = {s with next_page = Some "__BACK__"}

let keymap _ = [("Esc", back, "Back")]

let header s =
  [
    Widgets.title_highlight " Background Jobs ";
    Widgets.dim (Printf.sprintf "%d jobs" (List.length s.jobs));
  ]

let footer = [Widgets.dim "c: clear finished  Esc: back"]

let status_str = function
  | Job_manager.Pending -> Widgets.dim "Pending"
  | Job_manager.Running -> Widgets.yellow "Running"
  | Job_manager.Succeeded -> Widgets.green "Succeeded"
  | Job_manager.Failed msg -> Widgets.red ("Failed: " ^ msg)

let view s ~focus:_ ~size =
  let body =
    if s.jobs = [] then ["No jobs."]
    else
      s.jobs
      |> List.mapi (fun i (job : Job_manager.job) ->
          let marker = if i = s.selected then Widgets.bold "➤" else " " in
          Printf.sprintf
            "%s %-30s %s"
            marker
            (Widgets.bold job.description)
            (status_str job.status))
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  check_navigation s

let move_selection s delta =
  let len = List.length s.jobs in
  if len = 0 then s
  else
    let selected = max 0 (min (len - 1) (s.selected + delta)) in
    {s with selected}

let clear_finished s =
  Job_manager.clear_finished () ;
  {s with jobs = Job_manager.list (); selected = 0}

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
        {s with next_page = Some "__BACK__"}
    | Some Keys.Up | Some (Keys.Char "k") -> move_selection s (-1)
    | Some Keys.Down | Some (Keys.Char "j") -> move_selection s 1
    | Some (Keys.Char "c") -> clear_finished s
    | _ -> s

let next_page s = s.next_page

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let enter = enter

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
