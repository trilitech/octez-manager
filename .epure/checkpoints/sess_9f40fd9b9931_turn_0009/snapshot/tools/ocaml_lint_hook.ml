(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type hook_error = Fmt_check_failed of int | Build_failed of int

let run_hook ~run_command () =
  let fmt_exit = run_command "dune fmt --check" in
  if fmt_exit <> 0 then Error (Fmt_check_failed fmt_exit)
  else
    let build_exit = run_command "dune build" in
    if build_exit <> 0 then Error (Build_failed build_exit) else Ok ()

let exit_code_of_result = function Ok () -> 0 | Error _ -> 1
