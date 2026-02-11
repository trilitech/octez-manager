# GIF Scenario Runner

A Python tool for generating documentation GIFs from TUI applications. It runs scripted scenarios against a terminal application, captures the output as asciinema v2 recordings, and can convert them to GIFs.

## Requirements

- Python 3.8+
- PyYAML (`pip install pyyaml`)
- [agg](https://github.com/asciinema/agg) - asciinema GIF generator
- [gifsicle](https://www.lcdf.org/gifsicle/) - GIF optimizer (optional)

Install on Ubuntu/Debian:
```bash
pip install pyyaml
cargo install agg  # or download from releases
sudo apt install gifsicle
```

## Usage

### Basic Usage

```bash
# Run scenario and generate asciinema recording
python3 tools/gif_runner.py docs/scenarios/rpc_browser.yaml \
    -o /tmp/recording.cast \
    --command "_build/default/src/main.exe"

# Convert to GIF
agg /tmp/recording.cast /tmp/output.gif --font-size 20 --cols 160 --rows 40

# Optimize GIF (optional, reduces file size ~40%)
gifsicle -O3 --lossy=80 --colors 256 -o final.gif /tmp/output.gif
```

### Command Line Options

```
gif_runner.py <scenario.yaml> [options]

Arguments:
  scenario.yaml          YAML scenario file

Options:
  -o, --output FILE      Output file (.cast for asciinema v2 format)
  --command CMD          Command to run (default: main.exe or from scenario)
  --debug                Enable debug output
```

## Scenario Format

Scenarios are YAML files that define terminal dimensions, metadata, and a sequence of actions.

### Basic Structure

```yaml
name: Demo Name
description: Short description
cols: 160        # Terminal width
rows: 40         # Terminal height
title: "Window Title"
output: output_name  # Base name for output files
init_wait: 3.0   # Seconds to wait for TUI to initialize

actions:
  - comment: "Description of what's happening"
  - key: r
  - wait: 1.5
  - key: Enter
  # ... more actions
```

### Available Actions

| Action | Description | Example |
|--------|-------------|---------|
| `key: <key>` | Send a keystroke | `key: Enter`, `key: Down`, `key: "1"` |
| `type: <text>` | Type text character by character | `type: "hello world"` |
| `wait: <seconds>` | Wait for specified duration | `wait: 1.5` |
| `wait_for: <text>` | Wait until text appears on screen | `wait_for: "Loading..."` |
| `wait_for_re: <regex>` | Wait until regex matches | `wait_for_re: "Ready.*"` |
| `comment: <text>` | Add a comment (logged but no action) | `comment: "Select node"` |

### Supported Keys

- Navigation: `Up`, `Down`, `Left`, `Right`, `Home`, `End`, `PageUp`, `PageDown`
- Actions: `Enter`, `Tab`, `Escape` (or `Esc`), `Backspace`, `Space`
- Function keys: `F1` through `F12`
- Control keys: `Ctrl-C`, `Ctrl-D`, `Ctrl-L`, etc.
- Any single character: `"a"`, `"1"`, `"/"`, etc.

## Example Scenarios

### Basic RPC Browser Demo

```yaml
name: RPC Browser Demo
cols: 160
rows: 40
init_wait: 3.0

actions:
  - comment: "Wait for main screen"
  - wait: 2.0
  
  - comment: "Open RPC browser"
  - key: r
  - wait: 1.5
  
  - comment: "Navigate to local node"
  - key: Down
  - wait: 0.1
  # ... repeat for navigation
  
  - comment: "Select node"
  - key: Enter
  - wait: 2.0
  
  - comment: "Query endpoint"
  - key: "1"
  - wait: 3.0
```

### Multi-Pager Wide Demo

```yaml
name: RPC Browser Wide Demo
cols: 220
rows: 60
init_wait: 3.0

actions:
  # ... navigation to node ...
  
  - comment: "First query"
  - key: "1"
  - wait: 3.0
  
  - comment: "Create second pager"
  - key: S
  - wait: 1.0
  
  - comment: "Navigate and query second endpoint"
  # ... navigation ...
  - key: Enter
  - wait: 2.5
  
  - comment: "Show both pagers"
  - wait: 5.0
```

## Tips for Good GIFs

1. **Keep it focused**: Show one feature per GIF
2. **Use appropriate timing**: 
   - `wait: 0.1` for rapid navigation
   - `wait: 1.0-2.0` for pauses where viewer should read
   - `wait: 3.0-5.0` for final result display
3. **Add comments**: They help when debugging scenarios
4. **Test incrementally**: Run partial scenarios to verify navigation
5. **Choose appropriate dimensions**:
   - 160x40 for basic demos
   - 200-300 wide for multi-pager demos
6. **Use font-size 18-22** in agg for readable GIFs

## Output Formats

The tool generates **asciinema v2** format (`.cast` files), which can be:
- Played with `asciinema play recording.cast`
- Converted to GIF with `agg`
- Uploaded to asciinema.org
- Embedded in documentation

## Troubleshooting

### GIF shows wrong content
- Increase `init_wait` to ensure TUI fully loads
- Add longer `wait` after navigation actions
- Check that navigation counts are correct for your data

### Navigation lands on wrong item
- The cursor position depends on the current state
- After `S` (split), the browser cursor may not be where expected
- Use explicit navigation from known positions

### Recording is empty or partial
- Ensure the command path is correct
- Check that the TUI starts properly
- Increase timeouts for slow operations

## File Locations

```
tools/gif_runner.py           # The runner script
docs/scenarios/*.yaml         # Scenario definitions
docs/public/gifs/*.gif        # Generated GIFs for documentation
```
