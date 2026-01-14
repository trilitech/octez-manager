open! Base
module I = Ocaml_intrinsics_kernel

let%bench_module "Overheads" =
  (module struct
    (* Using [%bench_fun] to bind the input outside the benchmarked code actually has less
       overhead then using [%bench] naively. *)
    let%bench_fun "int overhead" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Fn.id n
    ;;

    let%bench_fun "int64 overhead" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Fn.id n
    ;;

    let%bench_fun "int32 overhead" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Fn.id n
    ;;

    let%bench_fun "nativeint overhead" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Fn.id n
    ;;
  end)
;;

let%bench_module "Clz" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "int_clz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> I.Int.count_leading_zeros n
    ;;

    let%bench_fun "int_clz2" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> I.Int.count_leading_zeros2 n
    ;;

    let%bench_fun "int64_clz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> I.Int64.count_leading_zeros n
    ;;

    let%bench_fun "nativeint_clz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> I.Nativeint.count_leading_zeros n
    ;;

    let%bench_fun "int32_clz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> I.Int32.count_leading_zeros n
    ;;

    (* Base *)
    let%bench_fun "base int_clz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Base.Int.clz n
    ;;

    let%bench_fun "base int64_clz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Base.Int64.clz n
    ;;

    let%bench_fun "base nativeint_clz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Base.Nativeint.clz n
    ;;

    let%bench_fun "base int32_clz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Base.Int32.clz n
    ;;
  end)
;;

let%bench_module "Ctz" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "int_ctz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> I.Int.count_trailing_zeros n
    ;;

    let%bench_fun "int64_ctz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> I.Int64.count_trailing_zeros n
    ;;

    let%bench_fun "nativeint_ctz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> I.Nativeint.count_trailing_zeros n
    ;;

    let%bench_fun "int32_ctz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> I.Int32.count_trailing_zeros n
    ;;

    (* Base *)
    let%bench_fun "base int_ctz" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Base.Int.ctz n
    ;;

    let%bench_fun "base int64_ctz" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Base.Int64.ctz n
    ;;

    let%bench_fun "base nativeint_ctz" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Base.Nativeint.ctz n
    ;;

    let%bench_fun "base int32_ctz" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Base.Int32.ctz n
    ;;
  end)
;;

let%bench_module "Popcnt" =
  (module struct
    (* ocaml_intrinsics library *)
    let%bench_fun "int_popcount" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> I.Int.count_set_bits n
    ;;

    let%bench_fun "int_popcount2" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> I.Int.count_set_bits2 n
    ;;

    let%bench_fun "int64_popcount" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> I.Int64.count_set_bits n
    ;;

    let%bench_fun "nativeint_popcount" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> I.Nativeint.count_set_bits n
    ;;

    let%bench_fun "int32_popcount" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> I.Int32.count_set_bits n
    ;;

    (* Base  *)
    let%bench_fun "base int_popcount" =
      let n = Sys.opaque_identity (Random.int Int.max_value) in
      fun () -> Base.Int.popcount n
    ;;

    let%bench_fun "base int64_popcount" =
      let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
      fun () -> Base.Int64.popcount n
    ;;

    let%bench_fun "base nativeint_popcount" =
      let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
      fun () -> Base.Nativeint.popcount n
    ;;

    let%bench_fun "base int32_popcount" =
      let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
      fun () -> Base.Int32.popcount n
    ;;
  end)
;;
