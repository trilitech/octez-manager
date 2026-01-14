open! Base
open! Stdio
module I = Ocaml_intrinsics_kernel.Float

let%expect_test "min and max" =
  let args =
    [ 0., 1.
    ; 1., 0.
    ; Float.neg_infinity, Float.infinity
    ; Float.infinity, Float.neg_infinity
    ; -0., 0.
    ; 0., -0.
    ; Float.nan, 0.
    ; 0., Float.nan
    ]
  in
  List.iter args ~f:(fun (x, y) -> printf "min %.19g %.19g = %.19g\n" x y (I.min x y));
  [%expect
    {|
    min 0 1 = 0
    min 1 0 = 0
    min -inf inf = -inf
    min inf -inf = -inf
    min -0 0 = 0
    min 0 -0 = -0
    min nan 0 = 0
    min 0 nan = nan
    |}];
  List.iter args ~f:(fun (x, y) -> printf "max %.19g %.19g = %.19g\n" x y (I.max x y));
  [%expect
    {|
    max 0 1 = 1
    max 1 0 = 1
    max -inf inf = inf
    max inf -inf = inf
    max -0 0 = 0
    max 0 -0 = -0
    max nan 0 = 0
    max 0 nan = nan
    |}]
;;
