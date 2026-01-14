(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let run page =
  let term_backend =
    {
      Miaou_runner_common.Tui_driver_common.available =
        Miaou_driver_term.Lambda_term_driver.available;
      run = Miaou_driver_term.Lambda_term_driver.run;
    }
  in
  let sdl_backend =
    {
      Miaou_runner_common.Tui_driver_common.available =
        Miaou_driver_sdl.Sdl_driver.available;
      run = (fun page -> Miaou_driver_sdl.Sdl_driver.run page);
    }
  in
  let matrix_backend =
    {
      Miaou_runner_common.Tui_driver_common.available =
        Miaou_driver_matrix.Matrix_driver.available;
      run = Miaou_driver_matrix.Matrix_driver.run;
    }
  in
  Miaou_runner_common.Tui_driver_common.run
    ~term_backend
    ~sdl_backend
    ~matrix_backend
    page
