ocaml_intrinsics_kernel - a library of intrinsics for OCaml
===========================================================


The ocaml_intrinsics_kernel library provides an OCaml interface to operations
that have dedicated hardware instructions on some micro-architectures.
Currently, it provides the following operations:

* conditional select

See ocaml_intrinsics for details. Unlike ocaml_intrinsics, ocaml_intrinsics_kernel
can be used by programs compiled to javascript.
