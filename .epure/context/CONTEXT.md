# Build Context: Register OCaml lint hook for Épure validation pipeline

Reference files in `.epure/context/`:

| File | Purpose | Status |
|------|---------|--------|
| story.md | Story spec, acceptance criteria, constraints | Required |
| rules.md | Project rules and architectural conventions | Required |
| code_context.md | Relevant code pointers and design context | Optional |
| parent_epic.md | Parent epic description and criteria | Optional |
| rejections.md | Feedback from previous build attempt | **NEW — read first** |

Build log: `.epure/build_output_1_2.log`

When all tests pass, redirect output:

  (dune build 2>&1 && dune runtest 2>&1) > .epure/build_output_1_2.log
