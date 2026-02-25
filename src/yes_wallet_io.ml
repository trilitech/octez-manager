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
      let delegate_addrs =
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
