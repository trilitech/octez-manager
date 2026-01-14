(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Page state wrapper with navigation support.

    Instead of pages manually managing a [next_page] field, the framework
    wraps page state in ['a t] and provides navigation helpers.

    {2 Usage in pages}

    {[
      let handle_key ps key ~size =
        match key with
        | "Esc" -> Navigation.back ps
        | "q" -> Navigation.quit ps
        | "Enter" -> Navigation.goto "other_page" ps
        | "j" -> Navigation.update (fun s -> {s with cursor = s.cursor + 1}) ps
        | _ -> ps
    ]}

    {2 Benefits}
    - No [next_page] field in page state
    - No [next_page] accessor function required
    - Clear, named navigation functions
    - Pure functional style (no side effects)
*)

type 'a t = {
  s : 'a;  (** The page's own state *)
  nav : string option;  (** Pending navigation target, if any *)
}

(** [make s] wraps a page state with no pending navigation. *)
val make : 'a -> 'a t

(** [goto page ps] sets navigation to the named page. *)
val goto : string -> 'a t -> 'a t

(** [back ps] navigates to the previous page in the stack. *)
val back : 'a t -> 'a t

(** [quit ps] exits the application. *)
val quit : 'a t -> 'a t

(** [pending ps] returns the pending navigation target, if any.
    Used by the framework after handlers return. *)
val pending : 'a t -> string option

(** [update f ps] applies [f] to the inner state.
    Shorthand for [{ps with s = f ps.s}]. *)
val update : ('a -> 'a) -> 'a t -> 'a t
