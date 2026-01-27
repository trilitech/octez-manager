(lang dune 3.13)
(name octez-manager)
(generate_opam_files true)

(package
 (name octez-manager)
 (synopsis "Minimal Octez service installer")
 (description
  "Small CLI that installs octez-node and companion systemd services without the full Octez Setup UI.")
 (depends
  (ocaml (>= 5.1))
  cmdliner
  rresult
  yojson
  bos
  (miaou-core (>= 0.1.4))
  (miaou-driver-term (>= 0.1.4))
  (miaou-driver-matrix (>= 0.1.4))
  (miaou-runner (>= 0.1.4))
  eio
  eio_main
  eio_posix
  cohttp-eio
  tls-eio
  tls
  ca-certs
  domain-name
  mirage-crypto-rng
  uri
  lambda-term
  linenoise
  (ppx_deriving :build)
  (alcotest :with-test)
  (bisect_ppx :with-test))
 (license MIT)
 (authors "Nomadic Labs <contact@nomadic-labs.com>")
 (maintainers "Nomadic Labs <contact@nomadic-labs.com>")
 (homepage "https://gitlab.com/mbourgoin/octez-manager")
 (bug_reports "https://gitlab.com/mbourgoin/octez-manager/-/issues"))
