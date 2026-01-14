(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Sdl = Tsdl.Sdl

type next_action = Refresh | Quit | Key of string

let string_of_event_text e =
  match Sdl.Event.(get e typ |> enum) with
  | `Text_input -> Some Sdl.Event.(get e text_input_text)
  | _ -> None

let keyname_of_scancode sc =
  match Sdl.Scancode.enum sc with
  | `Return -> Some "Enter"
  | `Up -> Some "Up"
  | `Down -> Some "Down"
  | `Left -> Some "Left"
  | `Right -> Some "Right"
  | `Tab -> Some "NextPage"
  | `Backspace -> Some "Backspace"
  | `Escape -> Some "Esc"
  | `Delete -> Some "Delete"
  | `Space -> Some " "
  | `A -> Some "a"
  | `B -> Some "b"
  | `C -> Some "c"
  | `D -> Some "d"
  | `E -> Some "e"
  | `F -> Some "f"
  | `G -> Some "g"
  | `H -> Some "h"
  | `I -> Some "i"
  | `J -> Some "j"
  | `K -> Some "k"
  | `L -> Some "l"
  | `M -> Some "m"
  | `N -> Some "n"
  | `O -> Some "o"
  | `P -> Some "p"
  | `Q -> Some "q"
  | `R -> Some "r"
  | `S -> Some "s"
  | `T -> Some "t"
  | `U -> Some "u"
  | `V -> Some "v"
  | `W -> Some "w"
  | `X -> Some "x"
  | `Y -> Some "y"
  | `Z -> Some "z"
  | _ -> None

let poll_event ~timeout_ms ~on_resize =
  let e = Sdl.Event.create () in
  let start = Sdl.get_ticks () in
  let rec loop () =
    match Sdl.poll_event (Some e) with
    | true -> (
        match Sdl.Event.(get e typ |> enum) with
        | `Quit -> Quit
        | `Window_event ->
            on_resize () ;
            Refresh
        | `Key_down -> (
            let repeat = Sdl.Event.get e Sdl.Event.keyboard_repeat <> 0 in
            match
              Sdl.Event.(get e keyboard_scancode |> keyname_of_scancode)
            with
            | Some k ->
                let is_nav =
                  match k with
                  | "Up" | "Down" | "Left" | "Right" | "Tab" | "NextPage" ->
                      true
                  | _ -> false
                in
                if repeat && not is_nav then loop () else Key k
            | None -> loop ())
        | `Text_input -> (
            match string_of_event_text e with
            | Some " " -> loop ()
            | Some txt -> Key txt
            | None -> loop ())
        | _ -> loop ())
    | false ->
        let elapsed_ms = Int32.(to_int (sub (Sdl.get_ticks ()) start)) in
        if elapsed_ms > timeout_ms then Refresh
        else (
          Sdl.delay 12l ;
          loop ())
  in
  loop ()
