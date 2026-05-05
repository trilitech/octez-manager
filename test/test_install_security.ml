(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> In_channel.input_all ic)

let contains_substring s sub =
  let sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= String.length s
    && (String.equal (String.sub s i sub_len) sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

let assert_not_contains path needle =
  let content = read_file path in
  Alcotest.(check bool)
    (path ^ " must not contain " ^ needle)
    false
    (contains_substring content needle)

let assert_contains path needle =
  let content = read_file path in
  Alcotest.(check bool)
    (path ^ " must contain " ^ needle)
    true
    (contains_substring content needle)

let index_of_substring content needle =
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > String.length content then None
    else if String.equal (String.sub content i needle_len) needle then Some i
    else loop (i + 1)
  in
  loop 0

let test_docs_do_not_recommend_main_pipe () =
  List.iter
    (fun path ->
      assert_not_contains
        path
        "raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh")
    [
      "README.md";
      "docs/src/content/docs/index.mdx";
      "docs/src/content/docs/getting-started/installation.md";
    ]

let test_docs_show_checksum_verification () =
  assert_contains
    "docs/src/content/docs/getting-started/installation.md"
    "sha256sum -c" ;
  assert_contains
    "docs/src/content/docs/getting-started/installation.md"
    {|grep "  ${ASSET}$" sha256sums.txt|} ;
  assert_not_contains
    "docs/src/content/docs/getting-started/installation.md"
    "--ignore-missing" ;
  List.iter
    (fun path -> assert_contains path "checksum verification")
    ["README.md"; "docs/src/content/docs/index.mdx"]

let test_install_script_verifies_before_install () =
  let content = read_file "install.sh" in
  assert_contains "install.sh" "sha256sum -c" ;
  let checksum_pos =
    index_of_substring content "sha256sum -c"
    |> Option.value ~default:(String.length content)
  in
  let chmod_pos =
    index_of_substring content "chmod +x" |> Option.value ~default:0
  in
  Alcotest.(check bool)
    "checksum verification appears before chmod"
    true
    (checksum_pos < chmod_pos)

let test_deb_release_assets_have_checksums () =
  assert_contains ".github/workflows/ci.yml" "octez-manager-linux-x86_64" ;
  assert_contains ".github/workflows/ci.yml" "ubuntu_amd64.deb.sha256" ;
  assert_contains ".github/workflows/ci.yml" "debian_amd64.deb.sha256"

let () =
  Alcotest.run
    "install_security"
    [
      ( "docs",
        [
          Alcotest.test_case
            "no recommended main pipe"
            `Quick
            test_docs_do_not_recommend_main_pipe;
          Alcotest.test_case
            "checksum verification documented"
            `Quick
            test_docs_show_checksum_verification;
        ] );
      ( "installer",
        [
          Alcotest.test_case
            "verifies before install"
            `Quick
            test_install_script_verifies_before_install;
        ] );
      ( "release",
        [
          Alcotest.test_case
            "deb assets have checksums"
            `Quick
            test_deb_release_assets_have_checksums;
        ] );
    ]
