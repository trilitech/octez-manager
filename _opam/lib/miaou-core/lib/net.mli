(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type t

(** Network capability: minimal HTTP fetch helpers.

		The core uses this capability for simple, synchronous HTTP GET-like
		operations. Each function receives the [rpc_addr] and [app_bin_dir]
		of the target service as passed-through context; implementations may
		use them to prefer local IPC (e.g., call a bundled client binary) or
		to construct requests. Calls are expected to run inside a shared
		[Eio_main.run] loop; the global fiber runtime must be initialized via
		[Miaou_helpers.Fiber_runtime.init].

		Conventions
		- Both functions return [(Ok body) | Error msg]. The [Error] text is
			intended for direct display to users when a fetch fails.
		- [http_get_string] is intended for small textual responses where the
			caller may further parse or decode the string.
		- [http_get_url] is the more general helper; callers should prefer
			it for fetching arbitrary URLs (the core uses it for RPC-style
			endpoints).

		Example implementation (using curl/wget fallback):
		{[
			let http_get_url ~rpc_addr:_ ~app_bin_dir:_ url =
				match Common.run_out ["curl"; "-sfm"; "2"; "--connect-timeout"; "0.8"; url] with
				| Ok s when s <> "" -> Ok s
				| _ -> Error (Printf.sprintf "Failed to fetch %s" url)

			let http_get_string = http_get_url
			Miaou.Net.register (Miaou.Net.create ~http_get_string ~http_get_url)
		]}
*)

val create :
  http_get_string:
    (env:Eio_unix.Stdenv.base ->
    rpc_addr:string ->
    app_bin_dir:string ->
    string ->
    (string, string) result) ->
  http_get_url:
    (env:Eio_unix.Stdenv.base ->
    rpc_addr:string ->
    app_bin_dir:string ->
    string ->
    (string, string) result) ->
  t

val register : t -> unit

val get : unit -> t option

val require : unit -> t

(** [http_get_string t ~rpc_addr ~app_bin_dir path] fetches [path] relative to
		the service identified by [rpc_addr] and returns its body.
		@param path path or URL to fetch
		@return [Ok body] or [Error msg]. *)
val http_get_string :
  t ->
  rpc_addr:string ->
  app_bin_dir:string ->
  string ->
  (string, string) result

(** [http_get_url t ~rpc_addr ~app_bin_dir url] fetches the provided [url]
		and returns its body. Implementations may use [rpc_addr] or
		[app_bin_dir] to select an appropriate transport (local client vs
		HTTP). *)
val http_get_url :
  t ->
  rpc_addr:string ->
  app_bin_dir:string ->
  string ->
  (string, string) result
