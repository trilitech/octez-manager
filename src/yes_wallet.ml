(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type curve = Ed25519 | Secp256k1 | P256 | BLS

type delegate = {alias : string; address : string; curve : curve}

let curve_of_address addr =
  if String.length addr < 3 then None
  else
    let prefix = String.sub addr 0 3 in
    match prefix with
    | "tz1" -> Some Ed25519
    | "tz2" -> Some Secp256k1
    | "tz3" -> Some P256
    | "tz4" -> Some BLS
    | _ -> None

let test_keys_for_curve = function
  | Ed25519 ->
      ( "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
        "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" )
  | Secp256k1 ->
      ( "spsk2oCGdj7yZFmAWr1wqopU512kDtCfWfs7N4FoRH6bkHhgXk5Tu5",
        "sppk7aFpmJLm3ZxMd3F4osvGBq77R1NeSpiJVnof3Wiv4mt84EBE9aX" )
  | P256 ->
      ( "p2sk2g5Btw8MK7fnhQg9d7DaE8QYX8A89BLWaqWTQcHuATYY3ABjD9",
        "p2pk66Y3178eHvgWFA4CDsgGPhbkWtwrYCVUKJnsAi41eGP7MbHKv24" )
  | BLS ->
      ( "BLsk1hKAHyGqY9qRbgoSVnjiSmDWpKGjFF3WNQ7BaiaMUA6RMA6Pfq",
        "BLpk1yUiLJ7RezbyViD5ZvWTfQndM3TRRYmvYWkUfH2EJqsLFnzzvpJss6pbuz3U1DDMpk8v16nV"
      )

let dedup_delegates delegates =
  let seen = Hashtbl.create 64 in
  List.filter
    (fun d ->
      if Hashtbl.mem seen d.address then false
      else (
        Hashtbl.replace seen d.address () ;
        true))
    delegates

let generate_wallet_json delegates =
  let delegates = dedup_delegates delegates in
  let pkhs =
    `List
      (List.map
         (fun d ->
           `Assoc [("name", `String d.alias); ("value", `String d.address)])
         delegates)
  in
  let pks =
    `List
      (List.map
         (fun d ->
           let _sk, pk = test_keys_for_curve d.curve in
           `Assoc
             [
               ("name", `String d.alias);
               ( "value",
                 `Assoc
                   [
                     ("locator", `String ("unencrypted:" ^ pk));
                     ("key", `String pk);
                   ] );
             ])
         delegates)
  in
  let sks =
    `List
      (List.map
         (fun d ->
           let sk, _pk = test_keys_for_curve d.curve in
           `Assoc
             [
               ("name", `String d.alias);
               ("value", `String ("unencrypted:" ^ sk));
             ])
         delegates)
  in
  (pkhs, pks, sks)

module Internal_for_tests = struct
  let test_keys_for_curve = test_keys_for_curve
end
