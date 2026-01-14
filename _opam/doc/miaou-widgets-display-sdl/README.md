🐱 MIAOU

MIAOU is a playful TUI library for OCaml with a state-view-handlers architecture.

Why the name?

It's an acronym: Model, Interface, Application, OCaml, UI.

It’s also the French way of writing a cat’s meow—a nod to OCaml’s French roots.

And like a cat, it’s light, curious, and perfectly at home on your terminal. 🐾

## Project status & ownership

- **Owner / maintainer:** Nomadic Labs (<contact@nomadic-labs.com>)
- **Repository:** https://github.com/trilitech/miaou
- **License:** MIT (SPDX: MIT)

MIAOU is a high-quality, easy-to-use TUI foundation for OCaml applications (installers, dashboards, service consoles, etc.).

See [CONTRIBUTING.md](./CONTRIBUTING.md) for contribution guidelines and [SECURITY.md](./SECURITY.md) for security policy.

Features at a glance
--------------------
- State-view-handlers page lifecycle with modal support and capability injection
- Ready-to-use widgets: tables, file browsers, pagers, modal forms, panes, text boxes, palette helpers, charts (sparkline, line, bar), image viewer, QR code generator, etc.
- Three rendering backends: Matrix (default, high-performance), Lambda-Term (stable), SDL2 (experimental graphics)
- Headless driver for tests/CI
- Debug overlay for performance monitoring (`MIAOU_OVERLAY=1`)
- Example demo with system monitoring, chart visualization, and image display capabilities

Backends
--------

MIAOU supports three rendering backends:

1. **Matrix (Default)**: High-performance terminal driver with cell-based double buffering and diff-based rendering. Uses OCaml 5 Domains for true parallelism: render domain runs at 60 FPS while main domain handles input at 30 TPS. Only changed cells are written to terminal, eliminating flicker. Pure ANSI output with no lambda-term dependency.

2. **Lambda-Term (TUI)**: Character-based terminal rendering using Unicode box-drawing and block characters. Excellent compatibility across terminals. Chart widgets now support **Unicode Braille mode** for higher resolution rendering (2×4 dots per cell vs 1 character per cell).

3. **SDL2 (Experimental)**: Hardware-accelerated graphics rendering with anti-aliased lines, smooth curves, pixel-perfect rendering, and full RGB color support. Provides superior visual quality for charts, sparklines, images, and QR codes.

**Backend selection** (priority: Matrix > SDL > Lambda-Term):
```sh
MIAOU_DRIVER=matrix  # Default - high-performance diff rendering
MIAOU_DRIVER=term    # Lambda-Term fallback
MIAOU_DRIVER=sdl     # SDL2 graphics (requires tsdl packages)
```

The SDL2 backend (`miaou-driver-sdl` package) requires Tsdl + Tsdl_ttf + Tsdl_image. Run with `MIAOU_DRIVER=sdl dune exec -- miaou.demo-sdl`. Provide a monospaced font via `MIAOU_SDL_FONT=/path/to/font.ttf` if auto-detection fails.

**SDL-Enhanced Widgets**: Chart widgets (sparkline, line chart), image viewer, and QR code widgets automatically use native SDL rendering when the SDL backend is active, providing smooth anti-aliased graphics instead of text approximation. See [`src/miaou_widgets_display/SDL_CHARTS_README.md`](./src/miaou_widgets_display/SDL_CHARTS_README.md) for details.

**Braille Charts**: Terminal chart widgets support Unicode Braille patterns for higher resolution. Each terminal cell becomes a 2×4 dot grid, providing smoother curves and denser plots. See the braille demo (`example/demos/braille/`) for usage examples.

Quick start — build & depend
----------------------------

Prerequisites

- OCaml (5.3.x recommended) and opam
- dune (>= 3.12)
- the runtime dependencies below (opam will install them)

Build in this repository (preferred):

```sh
# Install deps, build, test, format
make deps
make build
make test          # runs repo tests
# optional: make fmt
```

Alternative (direct dune):

```sh
eval $(opam env)
dune build @all
dune runtest
```

Using from another project
--------------------------

### Package Structure

MIAOU is split into multiple opam packages to allow flexible installation:

| Package | Description | SDL Required |
|---------|-------------|--------------|
| `miaou-tui` | Terminal-only (recommended for most users) | No |
| `miaou` | Full install with SDL support | Yes |
| `miaou-core` | Core library and widgets | No |
| `miaou-driver-matrix` | High-performance Matrix driver (default) | No |
| `miaou-driver-term` | Lambda-Term driver | No |
| `miaou-driver-sdl` | SDL2 driver | Yes |
| `miaou-runner` | Runner with backend selection | No (SDL optional) |

**For terminal-only applications** (no SDL2 dependency):
```bash
opam install miaou-tui
```

**For full SDL2 support**:
```bash
opam install miaou
```

### Library Names

The libraries use package-prefixed public names:

- `miaou-core.lib` → `Miaou` module (umbrella re-exporting Core, Widgets, Net)
- `miaou-core.core` → `Miaou_core`
- `miaou-core.widgets.display` → `Miaou_widgets_display`
- `miaou-core.widgets.layout` → `Miaou_widgets_layout`
- `miaou-core.widgets.input` → `Miaou_widgets_input`
- `miaou-core.internals` → `Miaou_internals`
- `miaou-driver-matrix.driver` → `Miaou_driver_matrix`
- `miaou-driver-term.driver` → `Miaou_driver_term`
- `miaou-driver-sdl.driver` → `Miaou_driver_sdl`

### Example dune stanza

```lisp
(library
 (name my_app)
 ;; For terminal-only: use miaou-core.lib
 (libraries miaou-core.lib miaou-driver-term.driver))
```

Or with the full package:

```lisp
(library
 (name my_app)
 ;; Full install: the miaou package brings everything
 (libraries miaou-core.lib miaou-driver-term.driver miaou-driver-sdl.driver))
```

Then in OCaml:

```ocaml
(* Use the umbrella module for clean imports *)
open Miaou

(* Example: use the layout Pane splitter and the display Widgets helpers *)
module Pane = Miaou.Widgets.Layout.Pane
module W    = Miaou.Widgets.Display.Widgets
```

Dependencies
------------

### Core (`miaou-core`)

Runtime dependencies (no SDL required):

- cohttp, cohttp-eio (HTTP client)
- eio, eio_main (effects-based I/O)
- lambda-term (terminal handling)
- rresult, uri, yojson
- imagelib (image loading)
- qrc (QR code generation)
- alcotest, bisect_ppx (test dependencies)

### SDL Backend (`miaou-driver-sdl`)

Additional dependencies for SDL2 support:

- tsdl (SDL2 bindings)
- tsdl-ttf (TrueType font rendering)
- tsdl-image (image loading for SDL)

### Install via opam

```sh
# Terminal-only (no SDL)
opam install miaou-tui

# Full install with SDL
opam install miaou

# Development (install from source)
opam install --deps-only -y .
```

Minimal usage example
---------------------

This repository ships an `example/` directory with mocked capabilities plus a driver bridge demonstrating the public API. Build it with dune and run `dune exec -- miaou.demo` (TUI-only) or `dune exec -- miaou.demo-sdl` (SDL with terminal fallback) to see the widgets in action. For your own app, create a tiny program that registers a `Tui_page` and invoke the driver — see the library modules under `miaou_core` for the public API.

### Chart Rendering Modes

Chart widgets support multiple rendering modes for different use cases:

```ocaml
open Miaou.Widgets.Display

(* ASCII mode - one character per data point, maximum compatibility *)
let sparkline = Sparkline_widget.create ~width:30 ~max_points:30 () in
for i = 0 to 29 do
  Sparkline_widget.push sparkline (Random.float 100.)
done;
let ascii_output = 
  Sparkline_widget.render sparkline ~focus:false ~show_value:true ~mode:ASCII () in
(* Output: ▂▃▄▅▆▇█▇▆▅▄▃▂ ... *)

(* Braille mode - 2×4 dots per cell, higher resolution for smoother curves *)
let braille_output = 
  Sparkline_widget.render sparkline ~focus:false ~show_value:true ~mode:Braille () in
(* Output: ⠀⠁⠃⠇⡇⣇⣧⣷⣿⣷⣧⣇⡇⠇⠃⠁ ... *)

(* Line charts also support braille mode *)
let points = List.init 50 (fun i ->
  let x = float_of_int i in
  let y = sin (x /. 5.0) *. 10.0 in
  { Line_chart_widget.x; y; color = None }
) in
let chart = Line_chart_widget.create
  ~width:60 ~height:20
  ~series:[{ label = "Sine Wave"; points; color = None }]
  () in
let chart_output = 
  Line_chart_widget.render chart 
    ~show_axes:false ~show_grid:false ~mode:Braille () in
```

See the braille demo (`example/demos/braille/`) for more examples of braille rendering.

Examples
--------

```sh
dune exec -- miaou.demo             # TUI-only demo (lambda-term)
dune exec -- miaou.demo-sdl         # SDL demo with enhanced graphics
dune exec -- miaou-runner-tui       # generic runner forcing TUI
dune exec -- miaou-runner-native    # generic runner preferring SDL
```

For the best SDL experience with transitions:
```sh
MIAOU_SDL_TRANSITION=explode MIAOU_DRIVER=sdl \
  MIAOU_SDL_FONT=/usr/share/fonts/TTF/FiraCode-Regular.ttf \
  dune exec -- miaou.demo-sdl
```

### Demo Pages

The demo includes several example pages showcasing different widgets:

- **System Monitor**: Real-time system metrics (CPU, memory, network) with live sparkline and line chart visualization
- **Chart Demo**: Interactive demonstrations of sparkline, line chart, and bar chart widgets
- **Image Viewer**: Display PNG images with terminal (Unicode block) or SDL (pixel-perfect) rendering
- **QR Code Generator**: Generate QR codes with URL encoding
- **File Browser**: Navigate filesystem with path editing
- **Text Editor**: Multi-line text input with cursor control
- **Table Demo**: Sortable, paginated data tables
- **Modal Forms**: Input dialogs and confirmation prompts

The demo registers mock System/Logger/Service_lifecycle implementations so you can inspect how capabilities are wired before integrating Miaou into your own driver.

Recording & replay
--------------------

Set `MIAOU_DEBUG_KEYSTROKE_CAPTURE=1` (and optionally `MIAOU_DEBUG_KEYSTROKE_CAPTURE_PATH`) to write a JSONL stream of every key processed by the Lambda-Term driver.
Set `MIAOU_DEBUG_FRAME_CAPTURE=1` (and `MIAOU_DEBUG_FRAME_CAPTURE_PATH`) to persist rendered frames.
If no overrides are provided the files are created in `MIAOU_DEBUG_CAPTURE_DIR` (defaults to the current working directory) under names such as `miaou_capture_keystrokes_<timestamp>.jsonl`.

The [`recordings/`](./recordings/README.md) directory contains canonical JSONL and asciicast files you can replay or regenerate. See
[`docs/CAPTURE_HELPER.md`](./docs/CAPTURE_HELPER.md) for a deeper walkthrough of the capture workflow.

Helper scripts:

- `./tools/capture_helper.sh --dir recordings -- dune exec -- miaou.demo` &ndash; wraps any command with the capture environment variables and prints the artifact locations.
- `./tools/replay_tui.py --keystrokes path/to/file --cmd "dune exec -- miaou.demo"` &ndash; replays a capture by feeding the recorded keys through a pseudo-TTY (optionally emitting an asciicast v3 file with `--write-cast`).
- `./tools/replay_all_captures.sh [--dir recordings] [-- ...extra args...]` &ndash; replays every keystroke capture in a directory.
- `./tools/replay_screencast.sh recordings/miaou_logging_switch_frames.jsonl` &ndash; replays the frame-only screencast in your terminal.
- `./tools/convert_cast_to_gif.sh recordings/miaou_logging_switch.cast recordings/miaou_logging_switch.gif` &ndash; converts an asciinema cast into a GIF (see also `Dockerfile.cast2gif` + `tools/docker_convert_cast_entrypoint.sh`).
- `./tools/run_capture_helpers_smoke.sh` &ndash; CI-friendly smoke test that verifies helpers/docs/sample artifacts are present.
- `./tools/upload_gifs_to_mr.py` &ndash; GitLab helper that uploads `recordings/*.gif` to the current Merge Request (requires `requests`; see `tools/requirements.txt`).

Each keystroke JSONL line looks like `{"timestamp": <float>, "key": <string>}`. Frame captures add terminal geometry: `{"timestamp": <float>, "size": {"rows": <int>, "cols": <int>}, "frame": <string>}`.

Running tests
-------------

From the repo root:

```sh
make test
# or: eval $(opam env) && dune runtest
```

The `test/` directory contains widget/layout regression tests plus a synthetic adaptive page exercised through the headless driver shipped in `src/lib_miaou_internal`. You can also run the install rule (`dune build @install`) to ensure opam packaging stays healthy.

Capabilities
------------

Miaou relies on a capability system so the driver (or host application) can decide how to perform side-effects. Before launching the UI you should register implementations for the following interfaces (see `src/miaou_interfaces/`):

- `Miaou_interfaces.System` — file-system and process helpers (required by file browsers, log viewers, etc.).
- `Miaou_interfaces.Logger` — sink for structured log output from widgets and the driver (optional but recommended).
- `Miaou_interfaces.Service_lifecycle` — used by the service manager widgets; provide stubs if your app does not manage OS services.
- `Miaou.Net` — HTTP capability used by network-aware widgets; the repo ships a `cohttp-eio` provider (`src/cohttp_net.ml`).

Register your implementations via `Miaou_interfaces.Capability.set` (or the helper `register` functions exposed by each interface) before calling `Miaou.Core.Tui_driver.start`. Tests use the mock implementations in `example/` for reference.

Contextual Help System
----------------------

MIAOU includes a built-in contextual help system that intercepts the `?` key globally. When users press `?`, the driver displays a help overlay with context-sensitive information.

**Important:** The driver intercepts `?` before your page's or modal's `handle_key` function receives it. You cannot handle `?` in custom key handlers.

To provide contextual help for your page or modal, use the `Help_hint` module:

```ocaml
(* In your page's view function *)
let view state ~focus ~size =
  Help_hint.set (Some "Press Space to toggle, Enter to confirm, Esc to cancel") ;
  (* ... render your page ... *)
```

For nested modals, use the stack-based API:

```ocaml
(* When opening a modal *)
let init () =
  Help_hint.push 
    ~short:"?" 
    ~long:"Press Esc to close this modal, Space to toggle options" 
    () ;
  (* ... *)

(* When closing the modal *)
let on_close () =
  Help_hint.pop () ;
  (* ... *)
```

The `Help_hint` module automatically handles responsive help text: it provides separate `short` and `long` variants, with the driver selecting the appropriate one based on terminal width.

See `src/miaou_core/help_hint.mli` for the complete API documentation.

Working with Modals
-------------------

MIAOU provides a flexible modal system through the `Modal_manager` module. Modals are stacked overlays that can be used for forms, dialogs, and confirmations.

### Basic Modal Usage

The simplest way to open a modal is with `push_default`:

```ocaml
module My_modal : Miaou.Core.Tui_page.PAGE_SIG = struct
  type state = Miaou_widgets_input.Textbox_widget.t

  let init () =
    Miaou_widgets_input.Textbox_widget.open_centered
      ~title:"Enter your name"
      ~width:40
      ()

  let view state ~focus ~size =
    Miaou_widgets_input.Textbox_widget.render state ~focus:true

  let handle_key state ~key ~size =
    Miaou_widgets_input.Textbox_widget.handle_key state ~key

  (* ... other PAGE_SIG methods ... *)
end

(* Open the modal *)
Modal_manager.push_default
  (module My_modal)
  ~init:(My_modal.init ())
  ~ui:{title = "Input"; left = None; max_width = None; dim_background = true}
  ~on_close:(fun state outcome ->
    match outcome with
    | `Commit -> Printf.printf "Got: %s\n" (Textbox_widget.get_text state)
    | `Cancel -> Printf.printf "Cancelled\n")
```

`push_default` automatically:
- Closes the modal with `Commit` when the user presses **Enter**
- Closes the modal with `Cancel` when the user presses **Esc**

### Nested Modals

**⚠ Important**: If your modal needs to open another modal when the user presses Enter (or handle Enter internally for any reason), **do NOT use `push_default`**. Instead, use `push` with empty `commit_on` and `cancel_on` lists:

```ocaml
(* Parent modal that opens a child modal on Enter *)
let handle_key state ~key ~size =
  match key with
  | "Enter" ->
      (* Open a child modal *)
      Modal_manager.push
        (module Child_modal)
        ~init:(Child_modal.init ())
        ~ui:{title = "Child"; ...}
        ~commit_on:[]   (* Don't auto-close on Enter! *)
        ~cancel_on:[]   (* Handle Esc manually *)
        ~on_close:(fun child_state outcome -> ...) ;
      state
  | "Esc" ->
      (* Close this modal manually *)
      Modal_manager.set_consume_next_key () ;
      Modal_manager.close_top `Cancel ;
      state
  | _ -> (* handle other keys *) state

(* Push the parent modal *)
Modal_manager.push
  (module Parent_modal)
  ~init:(Parent_modal.init ())
  ~ui:{title = "Parent"; ...}
  ~commit_on:[]   (* Empty: we handle Enter ourselves *)
  ~cancel_on:[]   (* Empty: we handle Esc ourselves *)
  ~on_close:(fun state outcome -> ...)
```

**Why this matters**: The modal manager checks `commit_on`/`cancel_on` **after** calling your `handle_key` function. If you use `push_default` (which sets `commit_on:["Enter"]`), when you press Enter:

1. Your `handle_key` processes the Enter and opens the child modal
2. The modal manager sees Enter is in `commit_on` and immediately closes the parent
3. Result: The parent modal disappears right after the child opens!

### Preventing Key Propagation

When you programmatically close a modal from within `handle_key`, call `set_consume_next_key()` **before** `close_top` to prevent the key from propagating to the underlying page or parent modal:

```ocaml
let handle_key state ~key ~size =
  match key with
  | "Enter" ->
      Modal_manager.set_consume_next_key () ;  (* Consume the Enter key *)
      Modal_manager.close_top `Commit ;
      state
  | "Esc" ->
      Modal_manager.set_consume_next_key () ;  (* Consume the Esc key *)
      Modal_manager.close_top `Cancel ;
      state
  | _ -> state
```

Without `set_consume_next_key()`, the Enter or Esc key that closed your modal would be passed to the parent modal or underlying page, potentially triggering unintended behavior.

### Complete Nested Modal Example

Here's a full working example of a parent modal that opens a confirmation dialog:

```ocaml
(* Confirmation dialog (child modal) *)
module Confirm_modal : Miaou.Core.Tui_page.PAGE_SIG = struct
  type state = string  (* The confirmation message *)

  let init message = message

  let view message ~focus ~size =
    message ^ "\n\nPress Enter to confirm, Esc to cancel"

  let handle_key state ~key ~size =
    match key with
    | "Enter" ->
        Modal_manager.set_consume_next_key () ;
        Modal_manager.close_top `Commit ;
        state
    | "Esc" ->
        Modal_manager.set_consume_next_key () ;
        Modal_manager.close_top `Cancel ;
        state
    | _ -> state

  (* ... other PAGE_SIG methods ... *)
end

(* Form modal (parent modal) *)
module Form_modal : Miaou.Core.Tui_page.PAGE_SIG = struct
  type state = {
    textbox : Miaou_widgets_input.Textbox_widget.t ;
    waiting_confirmation : bool ;
  }

  let init () = {
    textbox = Textbox_widget.open_centered ~width:40 () ;
    waiting_confirmation = false ;
  }

  let handle_key state ~key ~size =
    if state.waiting_confirmation then
      state  (* Child modal is handling input *)
    else
      match key with
      | "Enter" ->
          (* Open confirmation dialog *)
          Modal_manager.push
            (module Confirm_modal)
            ~init:"Submit this form?"
            ~ui:{title = "Confirm"; left = None; max_width = Some 50; dim_background = true}
            ~commit_on:[]
            ~cancel_on:[]
            ~on_close:(fun _ outcome ->
              match outcome with
              | `Commit ->
                  (* User confirmed - close the form *)
                  Modal_manager.set_consume_next_key () ;
                  Modal_manager.close_top `Commit
              | `Cancel ->
                  (* User cancelled - stay in form *)
                  ()) ;
          {state with waiting_confirmation = true}
      | "Esc" ->
          Modal_manager.set_consume_next_key () ;
          Modal_manager.close_top `Cancel ;
          state
      | _ ->
          {state with textbox = Textbox_widget.handle_key state.textbox ~key}

  let view state ~focus ~size =
    Textbox_widget.render state.textbox ~focus

  (* ... other PAGE_SIG methods ... *)
end
```

### Quick Reference

**Use `push_default` when**:
- Simple modals that just accept/cancel input
- No nested modals
- Default Enter/Esc behavior is sufficient

**Use `push` with `commit_on:[]` and `cancel_on:[]` when**:
- Your modal opens nested modals
- You need custom Enter/Esc handling
- You want full control over modal closing

**Always call `set_consume_next_key()` before `close_top`** when closing modals from within `handle_key`.

For more details, see `src/miaou_core/modal_manager.mli` and the examples in `example/demo_lib.ml`.

Configuration & options
-----------------------

- Logging/debug: enable debug output by setting `MIAOU_TUI_DEBUG_MODAL=1` (used by modal manager internals).
- Backend selection: MIAOU uses λ-term by default; the driver/backend interface makes alternate backends possible.

Debugging & environment variables
---------------------------------

**General:**
- `MIAOU_DEBUG=1` — enable debug logging output (driver events, SDL context, widget rendering)
- `MIAOU_OVERLAY=1` — show real-time FPS/TPS metrics in top-right corner (all drivers)
- `MIAOU_TUI_DEBUG_MODAL=1` — verbose modal-manager logging (also honored by `Miaou_internals.Modal_renderer`)
- `MIAOU_TUI_UNICODE_BORDERS=false` — force ASCII borders if your terminal font lacks box-drawing glyphs
- `MIAOU_TUI_ROWS` / `MIAOU_TUI_COLS` — override terminal geometry for the Lambda-Term driver during development

**Backend Selection:**
- `MIAOU_DRIVER=matrix` — use the Matrix driver (default, high-performance diff rendering)
- `MIAOU_DRIVER=term` — use the Lambda-Term driver
- `MIAOU_DRIVER=sdl` — use the SDL2 backend (requires `tsdl` + `tsdl-ttf` + `tsdl-image`)

**Matrix Backend:**
- `MIAOU_MATRIX_FPS=60` — render domain frame rate cap (default: 60)
- `MIAOU_MATRIX_TPS=30` — main domain tick rate (default: 30)

**SDL Backend:**
- `MIAOU_SDL_FONT=/path/to/font.ttf` — specify TrueType font for SDL rendering
- `MIAOU_SDL_FONT_SIZE=14` — font size in pixels (default: 14)
- `MIAOU_SDL_WINDOW_TITLE="My App"` — custom window title
- `MIAOU_SDL_TRANSITION=explode|fade|slide` — page transition effect

**Recording & Replay:**
- `MIAOU_DEBUG_KEYSTROKE_CAPTURE=1` — capture keystrokes to JSONL (see *Recording & replay*)
- `MIAOU_DEBUG_KEYSTROKE_CAPTURE_PATH=/path/to/file.jsonl` — custom keystroke capture path
- `MIAOU_DEBUG_FRAME_CAPTURE=1` — capture rendered frames
- `MIAOU_DEBUG_FRAME_CAPTURE_PATH=/path/to/file.jsonl` — custom frame capture path
- `MIAOU_DEBUG_CAPTURE_DIR=./recordings` — default directory for capture files

**Testing:**
- `MIAOU_TEST_ALLOW_FORCED_SWITCH=1` — enable the headless driver's `__SWITCH__:` escape hatch (useful in scripted tests)

Troubleshooting
---------------

- Package scope errors from dune: if you see errors about unknown packages when changing public names, ensure `(package (name miaou))` exists in `dune-project` and `miaou.opam` is present.
- Missing dependencies: run `opam install --deps-only .` from the repo root or `eval $(opam env)` before building.
- Dangling internal path / cmi errors during large refactors: update and compile libraries in small batches (fix one library, run `dune build`, then continue).

Design notes
------------

MIAOU is intentionally experimental. The library splits responsibilities into three conceptual parts:

- **Core:** Public API, page lifecycle, modal manager, driver-facing helpers. Now includes a robust capability system for abstracting side-effects.
- **Widgets:** Reusable UI widgets including:
  - **Layout:** tables, panes, file browser, progress bars
  - **Input:** textboxes, selectors, modal forms
  - **Display:** sparkline charts, line charts, bar charts, image viewer, QR code generator, pager, tree view, description lists
  - Many widgets support SDL-enhanced rendering for superior visual quality
- **Internals:** Renderer and implementation details that are not part of the public API.

This split helps enforce that only the driver composes overlays and that pages cannot directly call internal renderer APIs.

Status & Contributions
----------------------

MIAOU is early, experimental, and a little chaotic—like a kitten learning to climb curtains.

Pull requests are not welcome (yet). The whole point of the experiment is to practice cat-herded development: changes are coordinated by agents/LLMs under human direction.

Issues, bug reports, and feature requests are very welcome. If you spot something odd or dream of a feature, please open an issue in the project's tracker.

License
-------

MIAOU is released under the MIT License (SPDX: MIT).
The repository follows the project's licensing guidance: source-file license headers should attribute ownership to "Nomadic Labs <contact@nomadic-labs.com>" where appropriate. See `LICENSE.md` for full text.

Versioning & releases
---------------------

Miaou follows semantic versioning (MAJOR.MINOR.PATCH). Cut releases by tagging (`git tag vX.Y.Z && git push origin vX.Y.Z`), then use `dune-release tag && dune-release distrib && dune-release opam submit` to publish tarballs and submit the opam package update. Always run `opam install --deps-only --with-test .`, `dune build @all`, `dune runtest`, and `dune build @install` before tagging to ensure the release is reproducible.

Documentation
-------------

- **[Getting Started](./docs/getting-started.md)** — Build your first MIAOU application
- **[Architecture Overview](./docs/architecture.md)** — Core components, pages, modals, widgets
- **[Capabilities Guide](./docs/capabilities.md)** — Dependency injection system
- **[Examples](./example/README.md)** — Demo applications and widget showcases

Further reading
---------------

- Core API and driver: source under [src/miaou_core/](./src/miaou_core/)
- Widgets:
	- Display primitives, charts, images, and pager: [src/miaou_widgets_display/](./src/miaou_widgets_display/)
	  - Chart widgets: `sparkline_widget`, `line_chart_widget`, `bar_chart_widget`
	  - Image display: `image_widget`, `qr_code_widget`
	  - SDL-enhanced versions: `*_widget_sdl` modules for anti-aliased graphics
	  - See [SDL Charts README](./src/miaou_widgets_display/SDL_CHARTS_README.md) for details
	- Layout (panes, vsection, progress, file browser): [src/miaou_widgets_layout/](./src/miaou_widgets_layout/)
	- Input (textbox, select): [src/miaou_widgets_input/](./src/miaou_widgets_input/)
- Internals (renderer, modal machinery internals): [src/miaou_internals/](./src/miaou_internals/)


## Project home

MIAOU is maintained by Nomadic Labs (<contact@nomadic-labs.com>).

- **Repository:** https://github.com/trilitech/miaou
- **Issues:** https://github.com/trilitech/miaou/issues
- **Contributing:** See [CONTRIBUTING.md](./CONTRIBUTING.md)

## Global Keys API

Miaou provides a type-safe global keys system to prevent key binding conflicts and enable auto-generated help.

### Key Types

The `Keys` module defines all supported key types:

```ocaml
type Keys.t =
  | Up | Down | Left | Right
  | PageUp | PageDown | Home | End
  | Tab | ShiftTab | Enter | Escape | Backspace | Delete
  | Char of string
  | Control of string
  | Function of int
```

### Global Reserved Keys

The following keys are **reserved for application-wide functionality** and cannot be used by individual pages:

- `Control "s"` → Settings
- `Char "?"` → Help
- `Control "m"` → Menu  
- `Control "q"` → Quit

### Declaring Page Keys

Pages must declare which keys they handle using the `handled_keys` function:

```ocaml
module My_page : Tui_page.PAGE_SIG = struct
  (* ... other page functions ... *)
  
  let handled_keys () = [
    Keys.Char "a";      (* Handle 'a' key *)
    Keys.Char "b";      (* Handle 'b' key *)
    Keys.Enter;         (* Handle Enter *)
    Keys.Up;            (* Handle Up arrow *)
    Keys.Function 1;    (* Handle F1 *)
  ]
end
```

### Automatic Conflict Detection

When you register a page, Miaou automatically validates that:

1. **No global key conflicts**: Pages cannot handle reserved global keys
2. **Clear error messages**: Violations fail fast with helpful errors

```ocaml
(* This will fail at registration time: *)
module Bad_page = struct
  let handled_keys () = [Keys.Control "q"]  (* Reserved for Quit! *)
end

(* Runtime error: *)
(* "Page 'bad' attempts to handle reserved global keys: C-Q. *)
(*  Global keys are reserved for application-wide functionality." *)
```

### Checking for Conflicts Between Pages

You can check for key conflicts across all registered pages:

```ocaml
(* Get list of conflicts *)
let conflicts = Registry.check_all_conflicts () in
List.iter (fun (key, pages) ->
  Printf.printf "Key '%s' handled by: %s\n" 
    key (String.concat ", " pages)
) conflicts

(* Or get a human-readable report *)
match Registry.conflict_report () with
| None -> print_endline "No conflicts!"
| Some report -> print_endline report
```

### Benefits

- ✅ **Type-safe**: Keys are variants, not strings
- ✅ **Compile-time safety**: Invalid keys won't compile
- ✅ **Runtime validation**: Conflicts detected at registration
- ✅ **Self-documenting**: `handled_keys` serves as documentation
- ✅ **Auto-generated help**: Future help system can introspect keys

