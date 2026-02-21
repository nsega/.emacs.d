# Session Context

**Session ID:** 3662272f-90eb-4f6d-acc3-1b561237eac1

**Commit Message:** 2. buffer-list-update-hook fires extremely often

## Prompt

2. buffer-list-update-hook fires extremely often

## Summary

`window-size-change-functions` only fires when a window is actually resized, unlike `window-configuration-change-hook` which fires on nearly any layout change.

**Summary of changes made:**

| Issue | Before | After |
|---|---|---|
| Terminal title | `buffer-list-update-hook` (fires constantly) | `run-with-idle-timer 1` (at most once/sec when idle) |
| Frame title (GUI) | `(:eval ...)` on every redraw | Cached, updated by idle timer |
| vterm width | `window-configuration-change-hook` | `window-size-change-functions` |

To diagnose what's still consuming CPU after restarting Emacs, you can use:
```
M-x profiler-start RET cpu RET
;; reproduce the high-CPU situation for a few seconds
M-x profiler-report
```

## Key Actions

- - **Edit**: /Users/naokisega/src/github.com/nsega/.emacs.d/init.el
- - **Edit**: /Users/naokisega/src/github.com/nsega/.emacs.d/init.el
- - **Edit**: /Users/naokisega/src/github.com/nsega/.emacs.d/init.el
