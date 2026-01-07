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
  miaou-core
  miaou-driver-term
  miaou-runner
  eio
  eio_main
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
