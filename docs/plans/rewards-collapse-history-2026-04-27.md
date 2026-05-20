# Rewards page: collapse History tab into Overview

**Branch**: `feat/rewards-collapse-history`
**PR base**: `feat/rewards-custom-baker-key` (PR #915)
**Worktree**: `../octez-manager-collapse-history`
**Date**: 2026-04-27

## Goal

Remove the **History** tab from the Rewards page and move the only useful
behaviour it carried — selecting a past cycle and acting on it — onto the
**Overview** tab's existing *Recent Cycles* table. The "Trends (Last 30
Cycles)" sparkline panel is dropped: it adds visual noise without informing
any decision the user can make from this page.

After this change, the Rewards page has 3 tabs (Overview / Delegators /
Configuration) instead of 4.

## What's gone

1. The entire **History** tab (`Rewards_state.History`, the `[3]` tab-bar
   entry, the `Char "3"` shortcut, the `handle_history_key` arm).
2. **`src/ui/pages/rewards/rewards_history.ml{,i}`** — the file is removed
   in its entirety (rendering + the trends sparkline + the cursor table).
3. **`history_cursor` field** on `Rewards_state.t`. Replaced by a generic
   `cycle_cursor` used by the now-interactive Recent Cycles table on the
   Overview.
4. The "Trends (Last 30 Cycles)" sparkline. Not migrated. The Sparkline
   widget itself stays (used by `system_metrics_scheduler` and the
   `diagnostics_page`).

## What changes

### `Rewards_state` (`rewards_state.{ml,mli}`)

- `active_tab` loses `History`. `tab_index` / `tab_of_index` /
  `all_tabs` shrink by one. `Configuration` keeps its label but its index
  goes from `3` to `2` (under-the-hood; users see tabs as a horizontal
  bar, no "tab 3" disappearing).
- `history_cursor` → `cycle_cursor` (rename; same int semantics).
  Always indexes into `Rewards_scheduler.get_recent_cycles ~instance`.

### `Rewards_overview` (`rewards_overview.ml`)

- **Recent Cycles** table becomes interactive: `▸` cursor highlight on
  `state.cycle_cursor`, themed_emphasis on the selected row.
- The existing *Cycle detail* render path (`render_cycle_detail`) is
  reused unchanged — it already covers `state.selected_cycle = Some N`.
  Pressing **Enter** on a row sets `selected_cycle` and the page renders
  detail; **Esc** clears it back to dashboard (existing behaviour).
- Action shortcuts (`g` / `p` / `d`) now operate on the **selected**
  cycle when one is highlighted, falling back to the latest completed
  cycle otherwise. `t` (continual toggle) is unchanged — it acts at the
  baker level, not per-cycle.

### `rewards_page.ml`

- Drop `handle_history_key`; merge cursor + Enter handling into a new
  `handle_overview_key` arm. Cycle bounds come from
  `List.length (Rewards_scheduler.get_recent_cycles ~instance)`.
- Mouse: tab-bar click handler shrinks to 3 tabs.
- Keymap entries: drop `History → Navigate / View`, add `j/k Cycle` and
  `Enter View` to the **Overview** tab. The existing `g`/`p`/`d`/`t`
  entries stay; their help text updates to *"on selected cycle"* when a
  cursor is active.
- `handled_keys` loses `Char "3"`.
- `init` no longer accepts `Some "history"` from
  `Context.take_pending_rewards_tab` — that pending value should never
  reach this code now, but we still match it as a fallback to
  `Overview` to avoid a runtime regression for callers we haven't
  audited.

### Removed file

- `src/ui/pages/rewards/rewards_history.ml`
- `src/ui/pages/rewards/rewards_history.mli`

### Tests

- No existing test in `test/` references `Rewards_history`,
  `Rewards_state.History`, or `history_cursor` (verified with grep).
  No test churn expected.

### CHANGELOG

- New `Changed` entry under `[Unreleased]`: *"Rewards page: History tab
  removed; cycle navigation, view, and per-cycle actions consolidated on
  the Overview tab. Trends sparkline removed."*

## Open question — defer or include?

**Per-cycle action defaults.** Today `g` / `p` / `d` (Generate / Pay /
Dry-run) on Overview implicitly target the **latest completed cycle**.
After this change, those actions can target an explicitly selected
cycle. There's a UX choice:

- **(A) Selection-only**: action requires `selected_cycle = Some N`.
  Users have to press Enter (or j/k + space?) before `g`/`p` does
  anything cycle-specific.
- **(B) Cursor-aware fallback**: when a cycle is highlighted by the
  cursor, actions target it; otherwise they keep the current
  "latest completed" behaviour.

The plan above assumes (B). Mark in the validation quiz if the
preference is otherwise.

## Out of scope

- Restructuring the Delegators tab.
- Changes to `Rewards_scheduler` data fetch or cache shape.
- CLI: `rewards history` command stays — it serves a separate use case
  (machine-readable JSON / scripted polling). This PR is UI-only.

## Commit shape

Single commit on `feat/rewards-collapse-history`:

```
refactor(rewards): drop History tab, consolidate cycles on Overview

- Remove Rewards_history module, sparkline trends panel, History tab.
- Rename history_cursor → cycle_cursor on Rewards_state.
- Make Overview's Recent Cycles table interactive (j/k / Enter).
- Action shortcuts (g/p/d) operate on the cursor's selected cycle when
  one is highlighted; otherwise fall back to latest completed.
- CHANGELOG: Changed entry under [Unreleased].
```
