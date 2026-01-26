(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Tests for cache invalidation to prevent issue #489:
    Import wizard causing instances to disappear due to stale cache. *)

let test_data_cache_registered () =
  (* Force Data module to load (triggers cache registration at module init time).
     Use summarize() with empty list - doesn't require systemd. *)
  let _ = Octez_manager_ui.Data.summarize [] in

  (* Check the Cache registry to verify Data cache was registered *)
  let stats = Octez_manager_ui.Cache.get_stats () in
  let data_cache_exists =
    List.exists
      (fun (name, _, _, _, _, _, _) -> name = "data_service_states")
      stats
  in
  Alcotest.(check bool)
    "Data cache is registered with Cache registry"
    true
    data_cache_exists

let test_cache_invalidate_all_clears_data_cache () =
  (* This test verifies that Cache.invalidate_all() properly clears the Data cache.
     
     Without this fix (issue #489):
     - Data cache would remain stale after Cache.invalidate_all()
     - Import wizard would cause instances to disappear
     
     With the fix:
     - Data cache is registered with Cache registry
     - Cache.invalidate_all() properly clears it
  *)

  (* Get initial stats *)
  let stats_before = Octez_manager_ui.Cache.get_stats () in
  let find_data_cache stats =
    List.find_opt
      (fun (name, _, _, _, _, _, _) -> name = "data_service_states")
      stats
  in

  match find_data_cache stats_before with
  | None ->
      Alcotest.fail "Data cache not found in registry (registration failed)"
  | Some (_, _, _, _age_before, _, _, _) -> (
      (* Invalidate all caches *)
      Octez_manager_ui.Cache.invalidate_all () ;

      (* Check that Data cache was invalidated *)
      let stats_after = Octez_manager_ui.Cache.get_stats () in
      match find_data_cache stats_after with
      | None -> Alcotest.fail "Data cache disappeared after invalidation"
      | Some (_, _, _, age_after, _, _, _) ->
          (* After invalidation, age should be None (cache empty) *)
          Alcotest.(check (option (float 0.01)))
            "Data cache age is None after invalidation (cache cleared)"
            None
            age_after)

let () =
  let open Alcotest in
  run
    "Cache Invalidation"
    [
      ( "registration",
        [test_case "Data cache is registered" `Quick test_data_cache_registered]
      );
      ( "invalidation",
        [
          test_case
            "Cache.invalidate_all() clears Data cache"
            `Quick
            test_cache_invalidate_all_clears_data_cache;
        ] );
    ]
