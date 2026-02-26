(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let fetch_json_from_rpc ~endpoint path =
  let url = endpoint ^ path in
  let cmd =
    Printf.sprintf
      "curl -fsL --max-time 30 --connect-timeout 5 %s 2>/dev/null"
      (Cmd_runner.sh_quote url)
  in
  let* out = Cmd_runner.run_out ["/bin/sh"; "-c"; cmd] in
  try Ok (Yojson.Safe.from_string out)
  with Yojson.Json_error msg ->
    Error (`Msg (Printf.sprintf "JSON parse error from %s: %s" url msg))

(** Parse a mutez-like field (String/Int/Intlit) from a JSON value. *)
let parse_mutez json_val =
  match json_val with
  | `String s -> ( try float_of_string s with _ -> 0.0)
  | `Int n -> float_of_int n
  | `Intlit s -> ( try float_of_string s with _ -> 0.0)
  | _ -> 0.0

(** Fetch consensus key and companion key pkhs for a delegate address.
    Returns additional (address, curve) pairs to include in the wallet.
    Keys already equal to [addr] are excluded. Returns [] on any fetch failure. *)
let fetch_extra_signing_keys ~endpoint addr =
  let path =
    Printf.sprintf "/chains/main/blocks/head/context/delegates/%s" addr
  in
  match fetch_json_from_rpc ~endpoint path with
  | Error _ -> []
  | Ok json ->
      let open Yojson.Safe.Util in
      let extract_pkh field =
        try
          let pkh =
            json |> member field |> member "active" |> member "pkh" |> to_string
          in
          if String.equal pkh addr then None
          else
            match Yes_wallet.curve_of_address pkh with
            | Some curve -> Some (pkh, curve)
            | None -> None
        with _ -> None
      in
      List.filter_map
        Fun.id
        [extract_pkh "consensus_key"; extract_pkh "companion_key"]

let fetch_delegates ~endpoint ~max_delegates =
  let* json =
    fetch_json_from_rpc
      ~endpoint
      "/chains/main/blocks/head/context/delegates?active=true&with_minimal_stake=true"
  in
  match json with
  | `List addresses ->
      let delegate_addrs_unsorted =
        addresses
        |> List.filter_map (fun j ->
            match j with
            | `String addr -> (
                match Yes_wallet.curve_of_address addr with
                | Some curve -> Some (addr, curve)
                | None -> None)
            | _ -> None)
        |> List.filteri (fun i _ -> i < max_delegates)
      in
      (* Sort delegates by staking balance descending so that equal key-count
         splits in the allocation page produce equal-stake splits.  One RPC
         call (stake_distribution); silently keeps original order on failure. *)
      let delegate_addrs =
        let stake_map =
          match
            fetch_json_from_rpc
              ~endpoint
              "/chains/main/blocks/head/context/stake_distribution"
          with
          | Ok (`List entries) ->
              let tbl = Hashtbl.create 64 in
              List.iter
                (fun entry ->
                  let open Yojson.Safe.Util in
                  try
                    let baker = entry |> member "baker" |> to_string in
                    let bal = parse_mutez (entry |> member "staking_balance") in
                    Hashtbl.replace tbl baker bal
                  with _ -> ())
                entries ;
              tbl
          | _ -> Hashtbl.create 0
        in
        let stake_of (addr, _) =
          Option.value ~default:0.0 (Hashtbl.find_opt stake_map addr)
        in
        List.sort
          (fun a b -> Float.compare (stake_of b) (stake_of a))
          delegate_addrs_unsorted
      in
      (* Collect consensus/companion keys for each delegate.
         Also tracks the consensus key address for each delegate, so the baker
         receives consensus key aliases rather than delegate address aliases.
         (The baker daemon identifies delegates by their consensus key hash, so
         passing delegate address aliases causes empty attestation rights.) *)
      let seen = Hashtbl.create 64 in
      let consensus_keys = Hashtbl.create 32 in
      let add_unique (addr, curve) acc =
        if Hashtbl.mem seen addr then acc
        else (
          Hashtbl.add seen addr () ;
          (addr, curve) :: acc)
      in
      let all_addrs =
        List.fold_left
          (fun acc ((addr, _) as entry) ->
            let extra = fetch_extra_signing_keys ~endpoint addr in
            (* First extra key (if any) is the consensus key for this delegate *)
            (match extra with
            | (ck_addr, _) :: _ -> Hashtbl.replace consensus_keys addr ck_addr
            | [] -> ()) ;
            add_unique entry acc |> fun acc ->
            List.fold_left (fun a e -> add_unique e a) acc extra)
          []
          delegate_addrs
        |> List.rev
      in
      (* baker_delegates: consensus key aliases for baker CLI args *)
      let baker_set = Hashtbl.create 32 in
      List.iter
        (fun (addr, _) ->
          let ck =
            Option.value ~default:addr (Hashtbl.find_opt consensus_keys addr)
          in
          Hashtbl.replace baker_set ck ())
        delegate_addrs ;
      (* All wallet entries indexed sequentially *)
      let all_entries =
        List.mapi
          (fun i (addr, curve) ->
            Yes_wallet.
              {alias = Printf.sprintf "delegate-%d" i; address = addr; curve})
          all_addrs
      in
      let baker_delegates =
        List.filter
          (fun (d : Yes_wallet.delegate) -> Hashtbl.mem baker_set d.address)
          all_entries
      in
      Ok (baker_delegates, all_entries)
  | _ -> Error (`Msg "Expected JSON array from delegates RPC endpoint")

let write_wallet ~wallet_dir delegates =
  let* () =
    let owner, group_name =
      if Paths.is_root () then ("root", "root")
      else Paths.current_user_group_names ()
    in
    File_ops.ensure_dir_path ~owner ~group:group_name ~mode:0o755 wallet_dir
  in
  let pkhs, pks, sks = Yes_wallet.generate_wallet_json delegates in
  let write_file name json =
    let path = Filename.concat wallet_dir name in
    let content = Yojson.Safe.pretty_to_string json in
    try
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc content) ;
      Ok ()
    with Sys_error msg -> Error (`Msg msg)
  in
  let* () = write_file "public_key_hashs" pkhs in
  let* () = write_file "public_keys" pks in
  write_file "secret_keys" sks

let read_wallet_pkhs ~wallet_dir =
  let path = Filename.concat wallet_dir "public_key_hashs" in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let json = Yojson.Safe.from_file path in
      match json with
      | `List entries ->
          Ok
            (List.filter_map
               (fun entry ->
                 let open Yojson.Safe.Util in
                 try
                   let name = entry |> member "name" |> to_string in
                   let value = entry |> member "value" |> to_string in
                   Some (name, value)
                 with _ -> None)
               entries)
      | _ -> Ok []
    with
    | Sys_error msg -> Error (`Msg msg)
    | Yojson.Json_error msg -> Error (`Msg msg)

(** Fetch what percentage of total network staking power our wallet delegates
    hold. Tries [/context/stake_distribution] (one call, exact) then falls back
    to a count-based approximation against the full active-delegate list.

    Our delegates are entries at alias indices divisible by 3 (the base
    addresses; indices 1 and 2 are consensus/companion keys). *)
let fetch_stake_pct ~endpoint ?(only_addrs = []) ~wallet_dir () =
  let* pkhs = read_wallet_pkhs ~wallet_dir in
  let our_delegates =
    List.filter_map
      (fun (alias, addr) ->
        match String.split_on_char '-' alias with
        | ["delegate"; ns] -> (
            match int_of_string_opt ns with
            | Some n when n mod 3 = 0 ->
                (* When only_addrs is provided, restrict to that set. *)
                if
                  only_addrs <> []
                  && not (List.exists (String.equal addr) only_addrs)
                then None
                else Some addr
            | _ -> None)
        | _ -> None)
      pkhs
  in
  let n_our = List.length our_delegates in
  if n_our = 0 then Ok 0.0
  else
    let our_set = Hashtbl.create 32 in
    List.iter (fun addr -> Hashtbl.replace our_set addr ()) our_delegates ;
    (* Attempt: stake_distribution (1 call, has per-baker staking balance) *)
    let try_stake_distribution () =
      match
        fetch_json_from_rpc
          ~endpoint
          "/chains/main/blocks/head/context/stake_distribution"
      with
      | Ok (`List entries) ->
          let total = ref 0.0 in
          let ours = ref 0.0 in
          List.iter
            (fun entry ->
              let open Yojson.Safe.Util in
              try
                let baker = entry |> member "baker" |> to_string in
                let bal =
                  match entry |> member "staking_balance" with
                  | `String s -> ( try float_of_string s with _ -> 0.0)
                  | `Int n -> float_of_int n
                  | `Intlit s -> ( try float_of_string s with _ -> 0.0)
                  | _ -> 0.0
                in
                total := !total +. bal ;
                if Hashtbl.mem our_set baker then ours := !ours +. bal
              with _ -> ())
            entries ;
          if !total > 0.0 then Some (!ours /. !total *. 100.0) else None
      | _ -> None
    in
    match try_stake_distribution () with
    | Some pct -> Ok pct
    | None ->
        (* Fallback: count-based approximation *)
        let* all_json =
          fetch_json_from_rpc
            ~endpoint
            "/chains/main/blocks/head/context/delegates?active=true&with_minimal_stake=true"
        in
        let n_total =
          match all_json with `List all -> List.length all | _ -> 0
        in
        if n_total = 0 then Ok 0.0
        else Ok (float_of_int n_our /. float_of_int n_total *. 100.0)

(** Fetch baking power for a single delegate via its individual RPC.
    Uses [baking_power] if available and non-zero, else [total_staked].
    Returns 0.0 on any failure. *)
let fetch_delegate_baking_power ~endpoint addr =
  let path =
    Printf.sprintf "/chains/main/blocks/head/context/delegates/%s" addr
  in
  match fetch_json_from_rpc ~endpoint path with
  | Error _ -> 0.0
  | Ok json ->
      let open Yojson.Safe.Util in
      let field name = try parse_mutez (json |> member name) with _ -> 0.0 in
      let bp = field "baking_power" in
      if bp > 0.0 then bp else field "total_staked"

(** Fetch individual baking power for each base delegate in the wallet.

    Returns [(powers, wallet_total)] where [powers.(i)] is the baking power
    of the i-th base delegate and [wallet_total] is the sum across all wallet
    delegates. Percentages are relative to the wallet total (so all bakers
    together sum to 100%).

    Tries [/context/stake_distribution] first (one call). Falls back to
    per-delegate [/context/delegates/{addr}] queries (one call per delegate).
    Last resort: unit weights (proportional to key count).
    Performs blocking HTTP calls — call from a background thread only.

    @param endpoint Node RPC endpoint (e.g. http://127.0.0.1:18732)
    @param wallet_dir Sandbox wallet directory *)
let fetch_delegate_balances ~endpoint ~wallet_dir =
  let* pkhs = read_wallet_pkhs ~wallet_dir in
  let base_addrs =
    List.filter_map
      (fun (alias, addr) ->
        match String.split_on_char '-' alias with
        | ["delegate"; ns] -> (
            match int_of_string_opt ns with
            | Some n when n mod 3 = 0 -> Some addr
            | _ -> None)
        | _ -> None)
      pkhs
    |> Array.of_list
  in
  let n = Array.length base_addrs in
  if n = 0 then Ok ([||], 0.0)
  else
    (* Try stake_distribution first (single RPC call) *)
    let from_stake_distribution () =
      match
        fetch_json_from_rpc
          ~endpoint
          "/chains/main/blocks/head/context/stake_distribution"
      with
      | Ok (`List entries) ->
          let stake_map = Hashtbl.create 64 in
          List.iter
            (fun entry ->
              let open Yojson.Safe.Util in
              try
                let baker = entry |> member "baker" |> to_string in
                let bal = parse_mutez (entry |> member "staking_balance") in
                Hashtbl.replace stake_map baker bal
              with _ -> ())
            entries ;
          let balances =
            Array.map
              (fun addr ->
                Option.value ~default:0.0 (Hashtbl.find_opt stake_map addr))
              base_addrs
          in
          let total = Array.fold_left ( +. ) 0.0 balances in
          if total > 0.0 then Some (balances, total) else None
      | _ -> None
    in
    (* Fallback: per-delegate baking_power queries (n HTTP calls) *)
    let from_individual_queries () =
      let balances =
        Array.map (fetch_delegate_baking_power ~endpoint) base_addrs
      in
      let total = Array.fold_left ( +. ) 0.0 balances in
      if total > 0.0 then Some (balances, total) else None
    in
    match from_stake_distribution () with
    | Some result -> Ok result
    | None -> (
        match from_individual_queries () with
        | Some result -> Ok result
        | None ->
            (* Last resort: equal weights *)
            Ok (Array.make n 1.0, float_of_int n))

let add_account ~wallet_dir ~address ?alias () =
  let* curve =
    match Yes_wallet.curve_of_address address with
    | Some c -> Ok c
    | None ->
        Error
          (`Msg
             (Printf.sprintf
                "Invalid address prefix: %S (expected tz1/tz2/tz3/tz4)"
                address))
  in
  let* existing = read_wallet_pkhs ~wallet_dir in
  if List.exists (fun (_, addr) -> String.equal addr address) existing then
    Error (`Msg (Printf.sprintf "Address %s already exists in wallet" address))
  else
    let next_idx = List.length existing in
    let alias =
      match alias with
      | Some a -> a
      | None -> Printf.sprintf "delegate-%d" next_idx
    in
    let delegate = Yes_wallet.{alias; address; curve} in
    let all_delegates =
      List.map
        (fun (name, addr) ->
          let curve =
            match Yes_wallet.curve_of_address addr with
            | Some c -> c
            | None -> Yes_wallet.Ed25519
          in
          Yes_wallet.{alias = name; address = addr; curve})
        existing
      @ [delegate]
    in
    let* () = write_wallet ~wallet_dir all_delegates in
    Ok alias
