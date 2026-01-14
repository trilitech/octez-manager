(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Eio.Std

module type RUNTIME = sig
  val env : Eio_unix.Stdenv.base

  val sw : Eio.Switch.t
end

let runtime : (module RUNTIME) option ref = ref None

let page_switch : Eio.Switch.t option ref = ref None

exception Page_switch_cancel

(* Global shutdown flag checked by all spawned fibers *)
let shutdown_flag = Atomic.make false

let init ~env ~sw =
  Atomic.set shutdown_flag false ;
  page_switch := None ;
  runtime :=
    Some
      (module struct
        let env = env

        let sw = sw
      end)

let env_opt () =
  match !runtime with Some (module R) -> Some R.env | None -> None

let switch_opt () =
  match !runtime with Some (module R) -> Some R.sw | None -> None

let page_switch_opt () = !page_switch

let require_runtime () =
  match !runtime with
  | Some (module R) -> (R.env, R.sw)
  | None ->
      invalid_arg
        "Fiber runtime not initialized; call Fiber_runtime.init from \
         Eio_main.run"

let require_current_switch () =
  match !page_switch with
  | Some sw -> sw
  | None ->
      let _, sw = require_runtime () in
      sw

let require_env_and_switch () =
  let env, _ = require_runtime () in
  (env, require_current_switch ())

let with_page_switch f =
  let env, _ = require_runtime () in
  let prev = !page_switch in
  let result = ref None in
  (try
     Eio.Switch.run (fun page_sw ->
         page_switch := Some page_sw ;
         Fun.protect
           ~finally:(fun () -> page_switch := prev)
           (fun () ->
             let r = f env page_sw in
             result := Some r ;
             (* Cancel all page-scoped fibers before leaving the switch so we
                don't block on long-running tails. *)
             Eio.Switch.fail page_sw Page_switch_cancel))
   with Page_switch_cancel -> ()) ;
  match !result with
  | Some r -> r
  | None ->
      invalid_arg
        "with_page_switch: page callback raised before producing a result"

let with_page_scope f =
  match env_opt () with
  | Some _ -> with_page_switch (fun _env _sw -> f ())
  | None -> f ()

let with_env f =
  match env_opt () with
  | Some env -> f env
  | None ->
      invalid_arg
        "Fiber runtime not initialized; call Fiber_runtime.init from \
         Eio_main.run"

let is_shutdown () = Atomic.get shutdown_flag

let shutdown () = Atomic.set shutdown_flag true

let spawn f =
  let env, sw = require_env_and_switch () in
  Fiber.fork ~sw (fun () -> if not (is_shutdown ()) then f env) |> ignore

let sleep seconds =
  if is_shutdown () then ()
  else with_env (fun env -> Eio.Time.sleep env#clock seconds)
