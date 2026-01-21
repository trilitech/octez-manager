(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Escape a value for safe use in shell environment files.
    Wraps values containing special characters in double quotes and escapes
    characters that could cause shell expansion or command substitution.
    
    This prevents issues like glob expansion (asterisk, question, brackets),
    variable expansion (dollar sign), and command substitution (backticks).
    
    @param v The value to escape
    @return Escaped value, quoted if necessary *)
val escape_env_value : string -> string

(** Write environment variable pairs to the instance's node.env file.
    If [with_comments] is true, includes documentation comments for each variable. *)
val write_pairs :
  ?with_comments:bool ->
  inst:string ->
  (string * string) list ->
  (unit, Rresult.R.msg) result

(** Read environment variable pairs from the instance's node.env file *)
val read : inst:string -> ((string * string) list, Rresult.R.msg) result

(** Write node environment file with DATA_DIR, NODE_ARGS, and extra variables.
    If [with_comments] is true, includes documentation comments for each variable. *)
val write :
  inst:string ->
  data_dir:string ->
  run_args:string ->
  extra_env:(string * string) list ->
  ?with_comments:bool ->
  unit ->
  (unit, Rresult.R.msg) result
