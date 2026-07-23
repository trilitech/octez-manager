(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
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

(** Unescape a value read from a shell environment file.
    This is the inverse of [escape_env_value]. Removes surrounding double quotes
    and unescapes backslash sequences.
    
    Values without surrounding quotes are returned unchanged.
    
    @param v The escaped value from the env file
    @return Unescaped plain value *)
val unescape_env_value : string -> string

(** Write environment variable pairs to the instance's node.env file.
    If [with_comments] is true, includes documentation comments for each variable. *)
val write_pairs :
  ?with_comments:bool ->
  inst:string ->
  (string * string) list ->
  (unit, Rresult.R.msg) result

(** Read environment variable pairs from the instance's node.env file *)
val read : inst:string -> ((string * string) list, Rresult.R.msg) result

(** Update specific key-value pairs in an instance's node.env file in-place.

    Only lines whose key matches an entry in [updates] are replaced; all other
    lines (comments, blank lines, unchanged keys) are preserved verbatim.
    Keys in [updates] that are not found in the file are appended at the end.

    This avoids the double-encoding problem that would arise from a [read] +
    [write_pairs] round-trip: [read] returns raw encoded values (with any
    surrounding quotes intact), and [write_pairs] would encode them again.
    [patch_keys] encodes only the new values supplied by the caller.

    @param inst  Instance name
    @param updates List of [(key, plain_value)] pairs to write *)
val patch_keys :
  inst:string -> updates:(string * string) list -> (unit, Rresult.R.msg) result

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
