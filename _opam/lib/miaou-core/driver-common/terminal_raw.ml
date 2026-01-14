(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Raw terminal operations shared between drivers.
    Based on term_terminal_setup.ml (lambda-term driver). *)

type t = {
  fd : Unix.file_descr;
  tty_out_fd : Unix.file_descr;
  orig_termios : Unix.terminal_io option;
  mutable cached_size : (int * int) option;
  mutable cleanup_done : bool;
  resize_pending : bool Atomic.t;
  (* Screen content to dump on exit for debugging *)
  mutable exit_screen_dump : string option;
}

let setup () =
  let fd = Unix.descr_of_in_channel stdin in
  if not (try Unix.isatty fd with _ -> false) then
    failwith "interactive TUI requires a terminal" ;
  let orig_termios = try Some (Unix.tcgetattr fd) with _ -> None in
  (* Open /dev/tty directly for writing escape sequences - this is more reliable
     than stdout during cleanup/signal handling because stdout might be redirected *)
  let tty_out_fd =
    try Unix.openfile "/dev/tty" [Unix.O_WRONLY] 0
    with _ -> Unix.descr_of_out_channel stdout
  in
  {
    fd;
    tty_out_fd;
    orig_termios;
    cached_size = None;
    cleanup_done = false;
    resize_pending = Atomic.make false;
    exit_screen_dump = None;
  }

let set_exit_screen_dump t dump = t.exit_screen_dump <- Some dump

let fd t = t.fd

let enter_raw t =
  match t.orig_termios with
  | None -> ()
  | Some orig -> (
      let raw =
        {
          orig with
          Unix.c_icanon = false;
          Unix.c_echo = false;
          Unix.c_vmin = 1;
          Unix.c_vtime = 0;
        }
      in
      Unix.tcsetattr t.fd Unix.TCSANOW raw ;
      (* Enter alternate screen mode first *)
      try
        let alt_seq = "\027[?1049h" in
        ignore
          (Unix.write
             t.tty_out_fd
             (Bytes.of_string alt_seq)
             0
             (String.length alt_seq))
      with _ -> ())

let leave_raw t =
  match t.orig_termios with
  | None -> ()
  | Some orig -> ( try Unix.tcsetattr t.fd Unix.TCSANOW orig with _ -> ())

let disable_mouse t =
  (* ALWAYS run mouse cleanup - no flag check!
     Mouse tracking must be disabled reliably on exit.
     The escape sequences are idempotent (safe to send multiple times). *)
  let disable_seq =
    "\027[?1006l\027[?1015l\027[?1005l\027[?1003l\027[?1002l\027[?1000l"
  in
  (* Method 1: Write to /dev/tty *)
  (try
     for _ = 1 to 2 do
       let _ =
         Unix.write
           t.tty_out_fd
           (Bytes.of_string disable_seq)
           0
           (String.length disable_seq)
       in
       ()
     done ;
     Unix.tcdrain t.tty_out_fd
   with _ -> ()) ;
  (* Method 2: Write to stdout *)
  (try
     print_string disable_seq ;
     Stdlib.flush stdout
   with _ -> ()) ;
  (* Method 3: Write to stderr as last resort *)
  (try Printf.eprintf "%s%!" disable_seq with _ -> ()) ;
  (* Give terminal time to process escape sequences *)
  Unix.sleepf 0.05

let enable_mouse _t =
  try
    print_string "\027[?1000h\027[?1006h" ;
    Stdlib.flush stdout
  with _ -> ()

let cleanup t =
  if not t.cleanup_done then begin
    t.cleanup_done <- true ;
    (* Step 1: Exit alternate screen mode (restores main buffer) *)
    let exit_alt_seq = "\027[?1049l" in
    (try
       ignore
         (Unix.write
            t.tty_out_fd
            (Bytes.of_string exit_alt_seq)
            0
            (String.length exit_alt_seq)) ;
       Unix.tcdrain t.tty_out_fd
     with _ -> ()) ;
    (* Step 2: Restore terminal settings *)
    leave_raw t ;
    (* Step 3: Print saved screen content (if any) for debugging *)
    (* Write directly to /dev/tty to avoid shell interference (ble.sh, etc.) *)
    (match t.exit_screen_dump with
    | Some dump -> (
        try
          (* Clear screen and move cursor home before printing dump *)
          let clear_seq = "\027[2J\027[H" in
          ignore
            (Unix.write
               t.tty_out_fd
               (Bytes.of_string clear_seq)
               0
               (String.length clear_seq)) ;
          ignore
            (Unix.write
               t.tty_out_fd
               (Bytes.of_string dump)
               0
               (String.length dump)) ;
          ignore (Unix.write t.tty_out_fd (Bytes.of_string "\n") 0 1) ;
          Unix.tcdrain t.tty_out_fd
        with _ -> ())
    | None -> ()) ;
    (* Step 4: Show cursor, reset style *)
    let final_seq = "\027[?25h\027[0m" in
    try
      print_string final_seq ;
      Stdlib.flush stdout
    with _ -> ()
  end ;
  (* THEN disable mouse tracking - terminal is now in normal mode *)
  disable_mouse t

let write t s =
  try
    let _ = Unix.write t.tty_out_fd (Bytes.of_string s) 0 (String.length s) in
    ()
  with _ -> (
    try
      print_string s ;
      Stdlib.flush stdout
    with _ -> ())

(* Size detection using stty - direct method without System capability *)
let detect_size_direct () =
  try
    let pipe_read, pipe_write = Unix.pipe () in
    let tty_fd = Unix.openfile "/dev/tty" [Unix.O_RDONLY] 0 in
    let pid =
      Unix.create_process
        "stty"
        [|"stty"; "size"|]
        tty_fd
        pipe_write
        Unix.stderr
    in
    Unix.close tty_fd ;
    Unix.close pipe_write ;
    let buf = Buffer.create 32 in
    let tmp = Bytes.create 64 in
    let rec read_all () =
      match Unix.read pipe_read tmp 0 64 with
      | 0 -> ()
      | n ->
          Buffer.add_subbytes buf tmp 0 n ;
          read_all ()
    in
    read_all () ;
    Unix.close pipe_read ;
    let _ = Unix.waitpid [] pid in
    let output = String.trim (Buffer.contents buf) in
    match String.split_on_char ' ' output with
    | [r; c] ->
        let rows = int_of_string r in
        let cols = int_of_string c in
        Some (rows, cols)
    | _ -> None
  with _ -> None

let detect_size_env () =
  match (Sys.getenv_opt "MIAOU_TUI_ROWS", Sys.getenv_opt "MIAOU_TUI_COLS") with
  | Some r, Some c -> (
      try
        let rows = int_of_string (String.trim r) in
        let cols = int_of_string (String.trim c) in
        Some (rows, cols)
      with _ -> None)
  | _ -> None

let detect_size_uncached () =
  match detect_size_env () with
  | Some s -> s
  | None -> ( match detect_size_direct () with Some s -> s | None -> (24, 80))

let size t =
  match t.cached_size with
  | Some s -> s
  | None ->
      let s = detect_size_uncached () in
      t.cached_size <- Some s ;
      s

let invalidate_size_cache t = t.cached_size <- None

let install_signals t ~on_resize ~on_exit =
  let exit_flag = Atomic.make false in
  let sigwinch = 28 in
  (* Install resize handler *)
  (try
     Sys.set_signal
       sigwinch
       (Sys.Signal_handle
          (fun _ ->
            invalidate_size_cache t ;
            Atomic.set t.resize_pending true ;
            on_resize ()))
   with _ -> ()) ;
  (* Install exit handlers *)
  let set_exit_handler sigv =
    try
      Sys.set_signal
        sigv
        (Sys.Signal_handle
           (fun _ ->
             (* Run cleanup synchronously in signal handler *)
             (try on_exit () with _ -> ()) ;
             (* Set flag so main loop can exit gracefully *)
             Atomic.set exit_flag true ;
             (* Exit immediately *)
             exit 130))
    with _ -> ()
  in
  set_exit_handler Sys.sigint ;
  set_exit_handler Sys.sigterm ;
  (try set_exit_handler Sys.sighup with _ -> ()) ;
  (try set_exit_handler Sys.sigquit with _ -> ()) ;
  exit_flag

let resize_pending t = Atomic.get t.resize_pending

let clear_resize_pending t = Atomic.set t.resize_pending false
