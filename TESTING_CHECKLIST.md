# Testing and Verification Checklist

This checklist should be completed during manual testing of the snapshot import visualization feature.

## Build and Basic Checks

- [ ] Code compiles without errors: `dune build`
- [ ] Code passes formatting check: `dune build @fmt` or `ocamlformat --check src/**/*.ml`
- [ ] No new warnings introduced
- [ ] Tests pass: `dune runtest`

## Functional Testing

### Without Snapshot

- [ ] Start TUI: `dune exec -- octez-manager ui`
- [ ] Navigate to install node form
- [ ] Fill in all required fields WITHOUT selecting a snapshot
- [ ] Click "Confirm & Install"
- [ ] **Expected:** Progress modal appears with 2 steps (setup, configure)
- [ ] **Expected:** Steps complete in order with checkmarks
- [ ] **Expected:** Progress bar advances through steps
- [ ] **Expected:** Modal auto-closes on completion
- [ ] **Expected:** UI navigates to instances page
- [ ] **Expected:** New instance appears in instances list

### With Snapshot (tzinit)

- [ ] Navigate to install node form
- [ ] Fill in all required fields
- [ ] Select a snapshot from tzinit
- [ ] Click "Confirm & Install"
- [ ] **Expected:** Progress modal appears with 4 steps (setup, download, import, configure)
- [ ] **Expected:** Download step shows progress bar advancing
- [ ] **Expected:** Steps complete in order: setup ✓ → download ◐ → import ◐ → configure ◐
- [ ] **Expected:** Progress bar label updates with current step name
- [ ] **Expected:** Modal auto-closes on completion
- [ ] **Expected:** UI navigates to instances page
- [ ] **Expected:** New instance appears in instances list

### With Snapshot (custom URI)

- [ ] Navigate to install node form
- [ ] Fill in all required fields
- [ ] Enter a custom snapshot URI (HTTP or file://)
- [ ] Click "Confirm & Install"
- [ ] **Expected:** Progress modal appears with 4 steps
- [ ] **Expected:** Behavior similar to tzinit snapshot above

### Log Toggle

- [ ] During installation, press 'l' key
- [ ] **Expected:** Log section appears (may be empty if logs not captured yet)
- [ ] Press 'l' again
- [ ] **Expected:** Log section disappears
- [ ] **Expected:** Modal still responsive

### Cancel Operation

- [ ] Start an installation with snapshot (longer duration)
- [ ] Press Esc while installation is in progress
- [ ] **Expected:** Modal closes
- [ ] **Expected:** Installation continues in background (check with `systemctl --user status`)
- [ ] **Expected:** After completion, instance appears in instances list

### Error Handling

- [ ] Provide invalid configuration (e.g., invalid network name, non-existent app_bin_dir)
- [ ] Click "Confirm & Install"
- [ ] **Expected:** Error modal appears with meaningful error message
- [ ] **Expected:** Progress modal is closed
- [ ] **Expected:** Can retry installation after fixing issue

### Data Directory Preservation

- [ ] Install a node instance with snapshot
- [ ] Navigate back to install form
- [ ] Try to create another instance with same data directory
- [ ] Select "Keep existing data"
- [ ] **Expected:** Installation completes without wiping data
- [ ] Select "Refresh (wipe and import)" instead
- [ ] **Expected:** Progress modal shows snapshot download/import steps

## Performance and Thread Safety

- [ ] During installation, UI remains responsive (can navigate away and back)
- [ ] Progress updates appear smoothly without flickering
- [ ] No race conditions or crashes observed
- [ ] Background task completes even after canceling modal

## Regression Testing

- [ ] Install baker still works
- [ ] Install daemon still works
- [ ] Install signer still works
- [ ] Existing instance management (start/stop/remove) still works
- [ ] Snapshot refresh (from instances page) still works
- [ ] Other TUI pages still function correctly

## Edge Cases

- [ ] Install with very fast local snapshot (file://) - modal should appear briefly
- [ ] Install with slow network - progress should update gradually
- [ ] Install without network access - appropriate error shown
- [ ] Multiple rapid installations - each gets its own modal

## Code Quality

- [ ] No hardcoded values or magic numbers
- [ ] Proper error handling throughout
- [ ] Comments are clear and helpful
- [ ] Code follows project style conventions
- [ ] No memory leaks (global refs are cleaned up)

## Documentation

- [ ] IMPLEMENTATION_NOTES.md is accurate and helpful
- [ ] Code changes are self-documenting
- [ ] Future enhancements are clearly documented

---

## Test Results

**Date:** _____________  
**Tester:** _____________  
**Version/Commit:** _____________  

**Overall Result:** [ ] PASS [ ] FAIL [ ] NEEDS WORK

**Notes:**
