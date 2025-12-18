type t = Journald | File of {path : string; rotate : bool}

let default_for ~instance ~role =
  if Common.is_root () then
    File
      {
        path =
          Printf.sprintf
            "/var/log/octez/%s-%s.log"
            (Role.to_string role)
            instance;
        rotate = true;
      }
  else
    let base = Filename.concat (Common.xdg_state_home ()) "octez/logs" in
    File
      {
        path =
          Filename.concat
            base
            (Printf.sprintf "%s-%s.log" (Role.to_string role) instance);
        rotate = true;
      }

let to_string = function Journald -> "journald" | File {path; _} -> path

let to_yojson = function
  | Journald -> `Assoc [("type", `String "journald")]
  | File {path; rotate} ->
      `Assoc
        [
          ("type", `String "file");
          ("path", `String path);
          ("rotate", `Bool rotate);
        ]

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    match member "type" json with
    | `String "journald" -> Ok Journald
    | `String "file" ->
        let path = member "path" json |> to_string in
        let rotate =
          member "rotate" json |> to_bool_option |> Option.value ~default:false
        in
        Ok (File {path; rotate})
    | _ -> Error "Invalid logging mode JSON"
  with Type_error (msg, _) -> Error msg
