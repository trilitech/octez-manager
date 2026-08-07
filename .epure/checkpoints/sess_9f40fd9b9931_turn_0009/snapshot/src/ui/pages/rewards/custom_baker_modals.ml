(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Octez_manager_rewards

type endpoint_choice =
  | Endpoint_local of {label : string; url : string}
  | Endpoint_public of {label : string; url : string}
  | Endpoint_custom

type payout_key_choice = Payout_known of Keys_reader.key_info | Payout_custom

let prompt_endpoint ?(title = "RPC Endpoint") ~network ~on_submit () =
  (* Loose match: a service's stored network may be a slug ("mainnet") or a
     full URL ("https://teztnets.com/shadownet"); the [network] picked here is
     always a slug. Treat the picked slug as matching if either side equals
     the other lowercased OR if the slug appears as a substring of the
     service's network string. *)
  let net_eq a b =
    let la = String.lowercase_ascii a and lb = String.lowercase_ascii b in
    let contains_substring haystack needle =
      let lh = String.length haystack and ln = String.length needle in
      let rec check i =
        if i + ln > lh then false
        else if String.equal (String.sub haystack i ln) needle then true
        else check (i + 1)
      in
      ln > 0 && check 0
    in
    String.equal la lb || contains_substring la lb || contains_substring lb la
  in
  let local_nodes =
    Data.load_service_states ()
    |> List.filter_map (fun (st : Data.Service_state.t) ->
        let svc = st.service in
        let url = Rpc_addr.to_string svc.Service.rpc_addr in
        if
          String.equal svc.Service.role "node"
          && net_eq svc.Service.network network
          && String.length url > 0
        then
          Some
            (Endpoint_local
               {label = Printf.sprintf "%s — %s" svc.Service.instance url; url})
        else None)
  in
  let public_nodes =
    Public_nodes_cache.get_nodes ()
    |> List.filter_map (fun (n : Public_nodes_cache.node_info) ->
        match n.network with
        | Some net when net_eq net network ->
            Some
              (Endpoint_public
                 {
                   label = Printf.sprintf "%s — %s" n.label n.rpc_addr;
                   url = n.rpc_addr;
                 })
        | _ -> None)
  in
  let items = local_nodes @ public_nodes @ [Endpoint_custom] in
  let to_string = function
    | Endpoint_local {label; _} -> "[local]  " ^ label
    | Endpoint_public {label; _} -> "[public] " ^ label
    | Endpoint_custom -> "[ Custom… ]"
  in
  Modal_helpers.open_choice_modal
    ~title:(Printf.sprintf "%s (%s)" title network)
    ~items
    ~to_string
    ~on_select:(function
      | Endpoint_local {url; _} | Endpoint_public {url; _} -> on_submit url
      | Endpoint_custom ->
          Modal_helpers.prompt_validated_text_modal
            ~title:(Printf.sprintf "%s (custom)" title)
            ~placeholder:(Some "host:8732")
            ~validator:Custom_baker_registry.validate_endpoint
            ~on_submit
            ())
    ()

let prompt_payout_key ?(title = "Payout Key Alias") ~base_dir ~on_submit () =
  let keys =
    match Keys_reader.read_public_key_hashes ~base_dir with
    | Ok ks -> ks
    | Error _ -> []
  in
  let custom_prompt () =
    Modal_helpers.prompt_validated_text_modal
      ~title
      ~placeholder:(Some "payout-key")
      ~validator:(fun s ->
        if String.length s > 0 then Ok ()
        else Error "payout key alias must not be empty")
      ~on_submit
      ()
  in
  if keys = [] then custom_prompt ()
  else
    let items = List.map (fun k -> Payout_known k) keys @ [Payout_custom] in
    let to_string = function
      | Payout_known k ->
          Printf.sprintf "%s — %s" k.Keys_reader.name k.Keys_reader.value
      | Payout_custom -> "[ Type alias directly… ]"
    in
    Modal_helpers.open_choice_modal
      ~title:
        (Printf.sprintf "%s (%d known in %s)" title (List.length keys) base_dir)
      ~items
      ~to_string
      ~on_select:(function
        | Payout_known k -> on_submit k.Keys_reader.name
        | Payout_custom -> custom_prompt ())
      ()
