# Session Context

**Session ID:** 3662272f-90eb-4f6d-acc3-1b561237eac1

**Commit Message:** I would like to choose  2. buffer-list-update-hook fires extremely often

## Prompt

I would like to choose  2. buffer-list-update-hook fires extremely often

## Summary

Done. The only change kept is replacing `buffer-list-update-hook` with `run-with-idle-timer 1`:

```elisp
;; Before (fires on every buffer access):
(add-hook 'buffer-list-update-hook #'my/set-terminal-title)

;; After (fires at most once per second when idle):
(run-with-idle-timer 1 t #'my/set-terminal-title)
```

Restart Emacs to pick up the change and check if CPU improves.

## Key Actions

- - **Edit**: /Users/naokisega/src/github.com/nsega/.emacs.d/init.el
- - **Edit**: /Users/naokisega/src/github.com/nsega/.emacs.d/init.el
