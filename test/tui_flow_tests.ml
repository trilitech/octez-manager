open Alcotest
module Instances = Octez_manager_ui.Instances

let setup () =
  let tmp_dir = Filename.get_temp_dir_name () ^ "/octez-manager-test-" ^ string_of_int (Unix.getpid ()) in
  if not (Sys.file_exists tmp_dir) then Unix.mkdir tmp_dir 0o700;
  Unix.putenv "XDG_CONFIG_HOME" (tmp_dir ^ "/config");
  Unix.putenv "XDG_DATA_HOME" (tmp_dir ^ "/data");
  Unix.putenv "XDG_STATE_HOME" (tmp_dir ^ "/state");
  Octez_manager_lib.Capabilities.register ();
  tmp_dir

let cleanup tmp_dir =
  let rec rm_rf path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    ) else
      Sys.remove path
  in
  if Sys.file_exists tmp_dir then rm_rf tmp_dir

let test_instances_init () =
  let tmp_dir = setup () in
  Fun.protect ~finally:(fun () -> cleanup tmp_dir) (fun () ->
    let state = Instances.Page.init () in
    let size = {LTerm_geom.rows = 24; cols = 80} in
    let _ = Instances.Page.view state ~focus:true ~size in
    check pass "Init and view" () ()
  )

let test_instances_navigation () =
  let tmp_dir = setup () in
  Fun.protect ~finally:(fun () -> cleanup tmp_dir) (fun () ->
    let state = Instances.Page.init () in
    let size = {LTerm_geom.rows = 24; cols = 80} in
    (* Simulate Down key *)
    let state = Instances.Page.handle_key state "Down" ~size in
    (* Simulate Esc key *)
    let state = Instances.Page.handle_key state "q" ~size in
    (* Check next_page *)
    match Instances.Page.next_page state with
    | Some "__BACK__" -> ()
    | Some other -> fail ("Expected __BACK__, got " ^ other)
    | None -> fail "Expected __BACK__, got None"
  )

let () =
  run
    "TUI Flows"
    [
      ( "Instances",
        [
          test_case "Init" `Quick test_instances_init;
          test_case "Navigation" `Quick test_instances_navigation;
        ] );
    ]
