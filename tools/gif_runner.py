#!/usr/bin/env python3
"""
GIF Scenario Runner

Runs TUI demo scenarios with reactive condition checking and generates
terminalizer-compatible YAML recordings.

Usage:
    ./gif_runner.py scenario.yaml [--output recording.yml]

The scenario format supports:
    - key: <key>           Send a keystroke
    - type: <text>         Type text character by character  
    - wait: <seconds>      Wait for specified time
    - wait_for: <text>     Wait until text appears on screen
    - wait_for_re: <regex> Wait until regex matches screen
    - frame: <delay_ms>    Capture a frame with specified delay
    - comment: <text>      Add a comment (no-op)
"""

import argparse
import fcntl
import json
import os
import pty
import re
import select
import signal
import struct
import sys
import termios
import time
import yaml
from dataclasses import dataclass, field
from typing import List, Optional, Union

# Key escape sequences
KEY_MAP = {
    'Enter': '\r',
    'Tab': '\t',
    'Escape': '\x1b',
    'Esc': '\x1b',
    'Up': '\x1b[A',
    'Down': '\x1b[B',
    'Right': '\x1b[C',
    'Left': '\x1b[D',
    'Backspace': '\x7f',
    'Space': ' ',
    'Home': '\x1b[H',
    'End': '\x1b[F',
    'PageUp': '\x1b[5~',
    'PageDown': '\x1b[6~',
    'C-x': '\x18',
    'C-c': '\x03',
    'C-d': '\x04',
    'C-a': '\x01',
    'C-e': '\x05',
    'C-k': '\x0b',
    'C-u': '\x15',
    'F1': '\x1bOP',
    'F2': '\x1bOQ',
    'F3': '\x1bOR',
    'F4': '\x1bOS',
}


@dataclass
class Frame:
    """A single frame in the recording."""
    content: str
    delay: int  # milliseconds


@dataclass 
class Recording:
    """Complete recording with config and frames."""
    cols: int = 120
    rows: int = 35
    title: str = "octez-manager"
    frames: List[Frame] = field(default_factory=list)
    
    def to_asciinema_v2(self) -> str:
        """Generate asciinema v2 format (asciicast).
        
        This format is simpler and has better tooling support (agg, svg-term, etc).
        Format: JSON header line, then JSON event lines.
        """
        import json
        
        lines = []
        
        # Header
        header = {
            "version": 2,
            "width": self.cols,
            "height": self.rows,
            "timestamp": int(time.time()),
            "title": self.title,
            "env": {"TERM": "xterm-256color", "SHELL": "/bin/bash"}
        }
        lines.append(json.dumps(header))
        
        # Events: [time, "o", data]
        current_time = 0.0
        for frame in self.frames:
            # Output event
            event = [current_time, "o", frame.content]
            lines.append(json.dumps(event))
            current_time += frame.delay / 1000.0  # Convert ms to seconds
        
        return '\n'.join(lines)
    
    def to_terminalizer_yaml(self) -> str:
        """Generate terminalizer-compatible YAML.
        
        Terminalizer format:
        - config: section with all settings (indented)
        - records: list of {delay, content} where content is raw terminal output
        """
        # Config section (indented under config:)
        config_lines = [
            "# The configurations that used for the recording, feel free to edit them",
            "config:",
            "",
            f"  cols: {self.cols}",
            f"  rows: {self.rows}",
            "",
            "  repeat: 0",
            "  quality: 100",
            "  frameDelay: auto",
            "  maxIdleTime: 2000",
            "",
            "  frameBox:",
            "    type: floating",
            f'    title: "{self.title}"',
            "    style:",
            "      border: 0px black solid",
            "",
            "  cursorStyle: block",
            '  fontFamily: "JetBrains Mono, Monaco, Lucida Console, Monospace"',
            "  fontSize: 14",
            "  lineHeight: 1.2",
            "  letterSpacing: 0",
            "",
            "  # Catppuccin Mocha theme",
            "  theme:",
            '    background: "#1e1e2e"',
            '    foreground: "#cdd6f4"',
            '    cursor: "#f5e0dc"',
            '    black: "#45475a"',
            '    red: "#f38ba8"',
            '    green: "#a6e3a1"',
            '    yellow: "#f9e2af"',
            '    blue: "#89b4fa"',
            '    magenta: "#f5c2e7"',
            '    cyan: "#94e2d5"',
            '    white: "#bac2de"',
            '    brightBlack: "#585b70"',
            '    brightRed: "#f38ba8"',
            '    brightGreen: "#a6e3a1"',
            '    brightYellow: "#f9e2af"',
            '    brightBlue: "#89b4fa"',
            '    brightMagenta: "#f5c2e7"',
            '    brightCyan: "#94e2d5"',
            '    brightWhite: "#a6adc8"',
            "",
        ]
        
        # Records section - terminalizer expects content as-is with special escaping
        # Use yaml.dump with specific settings to match terminalizer's format
        records_data = []
        for frame in self.frames:
            records_data.append({
                'delay': frame.delay,
                'content': frame.content
            })
        
        # Use default_style='|' for literal block scalars which preserves content better
        records_yaml = yaml.dump(
            {'records': records_data}, 
            default_flow_style=False, 
            allow_unicode=True,
            width=10000,  # Prevent line wrapping
        )
        
        # Combine
        result = '\n'.join(config_lines) + '\n# Records, feel free to edit them\n' + records_yaml
        return result


class PTYController:
    """Controls a process running in a PTY."""
    
    def __init__(self, command: List[str], rows: int = 35, cols: int = 120):
        self.command = command
        self.rows = rows
        self.cols = cols
        self.master_fd: Optional[int] = None
        self.pid: Optional[int] = None
        self.screen_buffer = ""
        
    def start(self):
        """Start the process in a PTY."""
        self.pid, self.master_fd = pty.fork()
        
        if self.pid == 0:
            # Child process
            os.execvp(self.command[0], self.command)
        else:
            # Parent process
            # Set terminal size
            winsize = struct.pack('HHHH', self.rows, self.cols, 0, 0)
            fcntl.ioctl(self.master_fd, termios.TIOCSWINSZ, winsize)
            
            # Set non-blocking
            flags = fcntl.fcntl(self.master_fd, fcntl.F_GETFL)
            fcntl.fcntl(self.master_fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)
    
    def stop(self):
        """Stop the process."""
        if self.pid:
            try:
                os.kill(self.pid, signal.SIGTERM)
                time.sleep(0.1)
                os.kill(self.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            try:
                os.waitpid(self.pid, 0)
            except ChildProcessError:
                pass
        if self.master_fd:
            os.close(self.master_fd)
    
    def read_output(self, timeout: float = 0.1) -> str:
        """Read available output from the PTY."""
        output = ""
        end_time = time.time() + timeout
        
        while time.time() < end_time:
            ready, _, _ = select.select([self.master_fd], [], [], 0.01)
            if ready:
                try:
                    data = os.read(self.master_fd, 4096)
                    if data:
                        output += data.decode('utf-8', errors='replace')
                except (OSError, BlockingIOError):
                    break
            else:
                if output:
                    break
        
        self.screen_buffer += output
        # Keep only last ~100KB of buffer
        if len(self.screen_buffer) > 100000:
            self.screen_buffer = self.screen_buffer[-50000:]
        
        return output
    
    def send_key(self, key: str):
        """Send a keystroke to the PTY."""
        seq = KEY_MAP.get(key, key)
        os.write(self.master_fd, seq.encode('utf-8'))
    
    def send_text(self, text: str):
        """Send text to the PTY."""
        os.write(self.master_fd, text.encode('utf-8'))
    
    def get_screen(self) -> str:
        """Get current screen content (best effort from buffer)."""
        return self.screen_buffer
    
    def wait_for_text(self, text: str, timeout: float = 30.0) -> bool:
        """Wait until text appears on screen."""
        end_time = time.time() + timeout
        while time.time() < end_time:
            self.read_output(0.1)
            if text in self.screen_buffer:
                return True
        return False
    
    def wait_for_regex(self, pattern: str, timeout: float = 30.0) -> bool:
        """Wait until regex matches screen content."""
        regex = re.compile(pattern)
        end_time = time.time() + timeout
        while time.time() < end_time:
            self.read_output(0.1)
            if regex.search(self.screen_buffer):
                return True
        return False
    
    def capture_frame(self) -> str:
        """Capture current frame content."""
        # Read any pending output
        self.read_output(0.05)
        # Return the recent buffer content
        # We take the last rows*cols*2 characters as an approximation
        max_chars = self.rows * self.cols * 2
        return self.screen_buffer[-max_chars:] if len(self.screen_buffer) > max_chars else self.screen_buffer


class ScenarioRunner:
    """Runs a scenario and captures frames."""
    
    def __init__(self, scenario: dict, command: List[str] = None):
        self.scenario = scenario
        self.command = command or ['octez-manager']
        self.recording = Recording(
            cols=scenario.get('cols', 120),
            rows=scenario.get('rows', 35),
            title=scenario.get('title', 'octez-manager'),
        )
        self.pty: Optional[PTYController] = None
        self.last_frame_time = 0.0
        self.verbose = True
    
    def log(self, msg: str):
        """Log a message."""
        if self.verbose:
            print(f"[RUNNER] {msg}", file=sys.stderr)
    
    def capture_frame(self, delay_ms: int = None, drain_time: float = 0.3):
        """Capture a frame with calculated or specified delay.
        
        Args:
            delay_ms: Override delay in milliseconds
            drain_time: Time to wait and collect output before capturing
        """
        now = time.time()
        if delay_ms is None:
            delay_ms = int((now - self.last_frame_time) * 1000) if self.last_frame_time else 100
        delay_ms = max(50, min(delay_ms, 5000))  # Clamp between 50ms and 5s
        
        # Drain output for a bit to get complete screen update
        content = self.pty.read_output(drain_time)
        
        if content:  # Only add frame if there's actual content
            self.recording.frames.append(Frame(content=content, delay=delay_ms))
            self.last_frame_time = now
            self.log(f"Captured frame #{len(self.recording.frames)} (delay: {delay_ms}ms, {len(content)} bytes)")
    
    def run_action(self, action: dict):
        """Execute a single action."""
        if 'key' in action:
            key = action['key']
            self.log(f"Sending key: {key}")
            self.pty.send_key(str(key))
            # Wait a bit for TUI to process and render
            time.sleep(0.1)
            # Auto-capture frame after key press with longer drain
            self.capture_frame(drain_time=0.5)
            
        elif 'type' in action:
            text = action['type']
            self.log(f"Typing: {text}")
            for char in text:
                self.pty.send_key(char)
                time.sleep(0.05)
                self.pty.read_output(0.02)
            self.capture_frame()
            
        elif 'wait' in action:
            seconds = float(action['wait'])
            self.log(f"Waiting: {seconds}s")
            time.sleep(seconds)
            # Capture after wait with accumulated content
            self.capture_frame(drain_time=0.3)
            
        elif 'wait_for' in action:
            text = action['wait_for']
            timeout = action.get('timeout', 30)
            self.log(f"Waiting for text: '{text}' (timeout: {timeout}s)")
            if not self.pty.wait_for_text(text, timeout):
                self.log(f"WARNING: Timeout waiting for '{text}'")
            self.capture_frame()
            
        elif 'wait_for_re' in action:
            pattern = action['wait_for_re']
            timeout = action.get('timeout', 30)
            self.log(f"Waiting for regex: '{pattern}' (timeout: {timeout}s)")
            if not self.pty.wait_for_regex(pattern, timeout):
                self.log(f"WARNING: Timeout waiting for regex '{pattern}'")
            self.capture_frame()
            
        elif 'frame' in action:
            delay_ms = int(action['frame'])
            self.log(f"Capturing frame with delay: {delay_ms}ms")
            self.capture_frame(delay_ms)
            
        elif 'comment' in action:
            self.log(f"Comment: {action['comment']}")
            # No frame capture for comments
            
        else:
            self.log(f"Unknown action: {action}")
    
    def run(self) -> Recording:
        """Run the complete scenario."""
        self.log(f"Starting scenario: {self.scenario.get('name', 'unnamed')}")
        self.log(f"Command: {' '.join(self.command)}")
        
        self.pty = PTYController(
            self.command,
            rows=self.recording.rows,
            cols=self.recording.cols
        )
        
        try:
            self.pty.start()
            self.last_frame_time = time.time()
            
            # Wait for initial render
            init_wait = self.scenario.get('init_wait', 2.0)
            self.log(f"Waiting {init_wait}s for TUI to initialize...")
            time.sleep(init_wait)
            
            # Collect all initial output - this is the full screen
            initial_content = self.pty.read_output(1.0)
            self.log(f"Initial screen: {len(initial_content)} bytes")
            
            # Add the initial frame with all the startup content
            if initial_content:
                self.recording.frames.append(Frame(content=initial_content, delay=500))
                self.last_frame_time = time.time()
                self.log(f"Captured initial frame #{len(self.recording.frames)} ({len(initial_content)} bytes)")
            
            # Run actions
            actions = self.scenario.get('actions', [])
            for i, action in enumerate(actions):
                self.log(f"Action {i+1}/{len(actions)}")
                self.run_action(action)
            
            # Final frame
            self.log("Capturing final frame...")
            time.sleep(0.5)
            self.capture_frame(1000)
            
            # Quit gracefully
            self.log("Sending quit command...")
            self.pty.send_key('q')
            time.sleep(0.3)
            
        finally:
            self.pty.stop()
        
        self.log(f"Scenario complete. Captured {len(self.recording.frames)} frames.")
        return self.recording


def load_scenario(path: str) -> dict:
    """Load scenario from YAML file."""
    with open(path, 'r') as f:
        return yaml.safe_load(f)


def main():
    parser = argparse.ArgumentParser(description='Run TUI demo scenarios and generate recordings')
    parser.add_argument('scenario', help='Path to scenario YAML file')
    parser.add_argument('--output', '-o', default='recording.yml', help='Output file')
    parser.add_argument('--format', '-f', choices=['terminalizer', 'asciinema'], default='asciinema',
                        help='Output format (default: asciinema)')
    parser.add_argument('--command', '-c', default='octez-manager', help='Command to run')
    parser.add_argument('--render', '-r', action='store_true', help='Render to GIF after recording')
    parser.add_argument('--gif-output', '-g', help='GIF output path (default: based on scenario name)')
    args = parser.parse_args()
    
    # Load scenario
    print(f"Loading scenario: {args.scenario}", file=sys.stderr)
    scenario = load_scenario(args.scenario)
    
    # Parse command
    command = args.command.split() if isinstance(args.command, str) else args.command
    
    # Run scenario
    runner = ScenarioRunner(scenario, command)
    recording = runner.run()
    
    # Write recording
    print(f"Writing recording to: {args.output}", file=sys.stderr)
    with open(args.output, 'w') as f:
        if args.format == 'asciinema':
            f.write(recording.to_asciinema_v2())
        else:
            f.write(recording.to_terminalizer_yaml())
    
    # Optionally render
    if args.render:
        gif_output = args.gif_output or args.output.replace('.yml', '.gif').replace('.cast', '.gif')
        print(f"Rendering GIF: {gif_output}", file=sys.stderr)
        
        if args.format == 'asciinema':
            # Try agg (asciinema gif generator) first, then svg-term-cli
            ret = os.system(f"agg {args.output} {gif_output} 2>/dev/null")
            if ret != 0:
                print("agg not found, trying svg-term...", file=sys.stderr)
                svg_output = gif_output.replace('.gif', '.svg')
                ret = os.system(f"svg-term --in {args.output} --out {svg_output} 2>/dev/null")
                if ret == 0:
                    print(f"SVG created: {svg_output}", file=sys.stderr)
                    print("Note: Install 'agg' for GIF output: cargo install agg", file=sys.stderr)
                else:
                    print("Neither agg nor svg-term found.", file=sys.stderr)
                    print("Install agg: cargo install --git https://github.com/asciinema/agg", file=sys.stderr)
                    print("Or: npm install -g svg-term-cli", file=sys.stderr)
        else:
            ret = os.system(f"terminalizer render {args.output} --output {gif_output}")
            
        if ret == 0:
            print(f"GIF created: {gif_output}", file=sys.stderr)
            # Compress with gifsicle if available
            if gif_output.endswith('.gif'):
                compressed = gif_output.replace('.gif', '_compressed.gif')
                ret = os.system(f"gifsicle -O3 --lossy=80 --colors 256 -o {compressed} {gif_output} 2>/dev/null")
                if ret == 0:
                    os.rename(compressed, gif_output)
                    print(f"GIF compressed: {gif_output}", file=sys.stderr)
    
    print("Done!", file=sys.stderr)


if __name__ == '__main__':
    main()
