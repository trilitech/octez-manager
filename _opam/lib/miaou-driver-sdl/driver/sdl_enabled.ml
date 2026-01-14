[@@@warning "-32-34-37-69"]

let enabled =
  match Sys.getenv_opt "MIAOU_WITH_SDL" with
  | Some v when String.lowercase_ascii (String.trim v) = "0" -> false
  | Some v when String.lowercase_ascii (String.trim v) = "false" -> false
  | _ -> true
