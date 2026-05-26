(lang dune 3.13)
(name octez-manager)
(version 1.0.0-rc3)
(generate_opam_files true)
(license MIT)
(source (github trilitech/octez-manager))

(package
 (name octez-manager)
 (synopsis "Minimal Octez service installer")
 (description
  "Small CLI that installs octez-node and companion systemd services without the full Octez Setup UI.")
 (depends
  (ocaml (>= 5.3))
  cmdliner
  rresult
  yojson
  yaml
  bos
   (miaou-core (>= 0.5.2))
   (miaou-driver-term (>= 0.5.2))
   (miaou-driver-matrix (>= 0.5.2))
   (miaou-runner (>= 0.5.2))
   (miaou-driver-web (>= 0.5.2))
   (miaou-registry (>= 0.5.2))
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
   (ppx_forbid (and :build (>= 0.1.0)))
   (ppx_enforce (and :build (>= 0.1.0)))
  (ppxlib (>= 0.28.0))
  (sqlite3 :with-dev-setup)
  (alcotest :with-test)
  (qcheck-alcotest :with-test)
  (bisect_ppx :with-test))
 (authors "Nomadic Labs <contact@nomadic-labs.com>")
 (maintainers "Nomadic Labs <contact@nomadic-labs.com>"))
