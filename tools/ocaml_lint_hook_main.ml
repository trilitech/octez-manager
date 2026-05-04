(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let () =
  let result = Ocaml_lint_hook.run_hook ~run_command:Sys.command () in
  exit (Ocaml_lint_hook.exit_code_of_result result)
