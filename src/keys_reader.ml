(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type key_info = {
  name : string;
  value : string; (* public key hash: tz1/tz2/tz3/tz4... *)
}

let key_info_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let value = json |> member "value" |> to_string in
    Ok {name; value}
  with
  | Type_error (msg, _) -> Error (`Msg msg)
  | Undefined (msg, _) -> Error (`Msg msg)

let read_public_key_hashes ~base_dir =
  let path = Filename.concat base_dir "public_key_hashs" in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let json = Yojson.Safe.from_file path in
      match json with
      | `List entries ->
          let results = List.map key_info_of_yojson entries in
          (* Collect all results or return first error *)
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | Ok key :: rest -> collect (key :: acc) rest
            | Error msg :: _ -> Error msg
          in
          collect [] results
      | _ -> Error (`Msg "Invalid public_key_hashs format: expected JSON array")
    with
    | Sys_error msg -> Error (`Msg msg)
    | Yojson.Json_error msg -> Error (`Msg msg)

module For_tests = struct
  let key_info_of_yojson = key_info_of_yojson
end
