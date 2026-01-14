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
      Miaou_runner_common.Tui_driver_common.available = false;
      run = (fun _ -> `Quit);
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
