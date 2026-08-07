# Story

Story #13 [accepted]
Title: Register OCaml lint hook for Épure validation pipeline
As a developer
I want Register OCaml lint hook for Épure validation pipeline
So that it is available in the project

## Acceptance Criteria

- Given given the project has OCaml files
  When when epure build runs a pre-build lint hook
  Then then dune fmt --check and dune build are executed and failures block the build

## Constraints

(none)