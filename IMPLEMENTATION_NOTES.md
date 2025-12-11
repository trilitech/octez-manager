# Installation Progress Modal Implementation

This document describes the implementation of the snapshot import visualization feature for node installation.

## Problem Statement

During node installation with snapshot download and import, the TUI was blocking and provided no visual feedback about progress. Users had to wait without knowing what was happening or how long it would take.

## Solution Overview

The solution adds:
1. **Background execution** - Installation runs in a background task to keep UI responsive
2. **Progress modal** - A modal window shows installation progress with:
   - Checklist of steps (✓ complete, ◐ in progress, ○ pending)
   - Progress bar tracking current step
   - Optional log output display (toggle with 'l' key)
3. **Progress callbacks** - The installer module now supports optional callbacks to report progress

## Implementation Details

### 1. Installer Module Changes (`src/installer.ml`, `src/installer.mli`)

Added optional progress callbacks to `install_node`:
- `?on_download_progress:(int -> int option -> unit)` - Called during snapshot download with percentage
- `?on_step_complete:(string -> unit)` - Called when each major step completes

Steps reported:
- `"setup"` - Initial setup (directories, config)
- `"download"` - Snapshot download (only if snapshot is requested)
- `"import"` - Snapshot import (only if snapshot is requested)
- `"configure"` - Final configuration (service registration, enabling)

The callbacks are threaded through:
- `install_node` → `perform_bootstrap` → `perform_snapshot_plan` → `download_snapshot_to_tmp`

### 2. Installation Progress Modal (`src/ui/install_progress_modal.ml`, `.mli`)

New modal module that:
- Shows a checklist of installation steps with status indicators
- Displays a progress bar with the current step name
- Supports toggling log output display (press 'l')
- Allows canceling (Esc) - installation continues in background
- Uses a global ref for thread-safe updates from background tasks

The modal dynamically creates steps based on whether a snapshot is being installed:
- With snapshot: setup → download → import → configure
- Without snapshot: setup → configure

### 3. Install Form Integration (`src/ui/pages/install_node_form.ml`)

Modified the `run_install` function to:
1. Determine if a snapshot is needed
2. Open the progress modal before starting installation
3. Submit installation to background runner with progress callbacks
4. Update modal state via callbacks during installation
5. Close modal and navigate to instances page on completion
6. Show error modal if installation fails

### 4. Thread Safety

The implementation uses a global ref (`current_modal_state`) to allow safe updates from background threads. This follows the same pattern as the `Context` module which uses refs that are dereferenced during each UI render cycle.

## User Experience

**Before:**
- User clicks "Confirm & Install"
- UI blocks completely during installation
- No progress indication
- No way to see what's happening
- Output only visible in logs (not in TUI)

**After:**
- User clicks "Confirm & Install"
- Progress modal opens immediately
- Checklist shows current step
- Progress bar advances through steps
- Download progress is tracked (0-100%)
- User can cancel (installation continues in background)
- User can toggle log output (press 'l')
- Modal auto-closes on completion
- Navigation to instances page on success

## Future Enhancements

1. **Log Capture** - Currently the log toggle is a placeholder. To implement full log capture:
   - Modify `import_snapshot` to capture stderr/stdout
   - Parse import logs for additional progress information
   - Display captured logs in the modal when toggle is enabled

2. **Import Progress Parsing** - The snapshot import output contains progress information that could be parsed:
   ```
   Dec 11 16:52:01.165: importing data from snapshot ...
   ```
   This could provide more granular progress during the import step.

3. **Error Recovery** - Add more detailed error reporting in the modal (not just closing it)

## Testing

Manual testing is required:
1. Install node without snapshot - verify 2-step checklist
2. Install node with snapshot - verify 4-step checklist  
3. Verify download progress updates during snapshot download
4. Verify modal closes on completion
5. Verify navigation to instances page after install
6. Test cancel (Esc) - verify installation continues in background
7. Test log toggle (l key) - verify UI updates

## Files Changed

- `src/installer.ml` - Added progress callback parameters and calls
- `src/installer.mli` - Updated signature for install_node
- `src/ui/install_progress_modal.ml` - New modal module
- `src/ui/install_progress_modal.mli` - New modal interface
- `src/ui/pages/install_node_form.ml` - Integrated modal and background execution
