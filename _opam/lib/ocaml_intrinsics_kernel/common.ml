let available =
  match Sys.backend_type with
  | Native ->
    (* (match Sys.unix with
     *  | false -> false
     *  | true ->
     *    (match Sys.architecture with
     *     | "amd64" | "arm64" -> true
     *     | _ -> false)) *)
    Sys.unix
  | Bytecode | Other _ -> false
;;
