# Terminal Emacs Tips

## 🖥️ Key Binding Differences in Terminal

Some key bindings work differently in terminal Emacs vs GUI Emacs. This guide provides terminal-friendly alternatives.

## 🔑 Terminal-Friendly Key Bindings

### Search & Navigation

The `M-s` prefix often conflicts with terminal emulators. Use these alternatives:

| GUI Binding | Terminal Alternative | Command | Description |
|-------------|---------------------|---------|-------------|
| `M-s l` | **`C-c s l`** | consult-line | Search lines in buffer |
| `M-s g` | **`C-c s g`** | consult-grep | Grep search |
| `M-s r` | **`C-c s r`** | consult-ripgrep | Fast ripgrep search |
| `M-g i` | **`C-c s i`** | consult-imenu | Jump to function/section |

### Other Common Issues

| Issue | GUI Binding | Terminal Alternative | Solution |
|-------|-------------|---------------------|----------|
| Meta key | `M-x` | `ESC x` | Use ESC as meta |
| C-. conflicts | `C-.` | `M-x embark-act` | Call directly |
| C-h conflicts | `C-h` | Works (fixed in config) | Mapped to backspace |

## 💡 Using ESC as Meta

In terminal Emacs, you can always use `ESC` instead of `Alt/Option`:

```
M-x  →  Press ESC, then x
M-.  →  Press ESC, then .
M-,  →  Press ESC, then ,
```

**Tip:** Press and release ESC (don't hold it), then press the next key.

## 🔧 Fix Terminal Meta Key

### macOS Terminal.app

1. Open Terminal → Preferences
2. Profiles → Keyboard
3. Check "Use Option as Meta key"

### iTerm2 (Recommended)

1. Preferences → Profiles → Keys
2. Left Option Key → `Esc+`
3. Right Option Key → `Esc+`

### kitty

Add to `~/.config/kitty/kitty.conf`:
```
macos_option_as_alt yes
```

### Alacritty

Add to `~/.config/alacritty/alacritty.yml`:
```yaml
key_bindings:
  - { key: A,         mods: Alt,     chars: "\x1ba"                       }
  - { key: B,         mods: Alt,     chars: "\x1bb"                       }
  # ... (repeat for all letters)
```

## 📋 Quick Reference for Terminal

### Essential Commands (Terminal-Optimized)

```
Opening & Navigation:
  C-x C-f       Find file
  C-x b         Switch buffer
  C-c p f       Find file in project

Search (Terminal-friendly!):
  C-c s l       Search buffer (was M-s l)
  C-c s r       Search project (was M-s r)
  C-c s i       Jump to function (was M-g i)

Code Navigation:
  M-.  or ESC . Jump to definition
  M-,  or ESC , Go back
  M-?  or ESC ? Find references

Editing:
  C-;           Edit all occurrences
  M-%  or ESC % Replace
  C-w           Cut line
  M-w  or ESC w Copy line
```

## 🎯 Recommended Terminal Workflow

Since terminal Emacs has some limitations, here's the optimal workflow:

### Option 1: Use GUI Emacs (Recommended)
```bash
# Start GUI Emacs from terminal
emacs &

# Or create alias in ~/.zshrc:
alias e='emacs &'
```

**Benefits:**
- All key bindings work
- Better font rendering
- Mouse support
- Easier clipboard integration

### Option 2: Use Terminal Emacs with Alternatives
```bash
# Use terminal emacs
emacs -nw

# Remember to use:
# - C-c s l instead of M-s l
# - C-c s r instead of M-s r
# - ESC x instead of M-x (if meta doesn't work)
```

### Option 3: Hybrid Approach
```bash
# Quick edits in terminal
emacs -nw config.go

# Full development in GUI
emacs /path/to/project &
```

## 🐛 Troubleshooting

### "M-s l does nothing"

**Solution:** Use `C-c s l` instead. This is the terminal-friendly alternative.

**Test it:**
```
1. Open a file: emacs -nw ~/.bashrc
2. Press: C-c s l
3. Type search term
4. Should show matching lines!
```

### "Meta key doesn't work at all"

**Solution 1:** Use ESC instead
```
Instead of: M-x
Press: ESC (release) then x
```

**Solution 2:** Configure terminal (see above)

### "C-. doesn't work"

**Solution:** Use the long form:
```
M-x embark-act
```

Or add a different binding in init.el:
```elisp
(global-set-key (kbd "C-c .") 'embark-act)
```

### "C-h still shows help instead of backspace"

This should be fixed in your config. If not:
```elisp
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
```

## 📝 Terminal-Specific .emacs.d/init.el Snippet

If you primarily use terminal Emacs, add this to your init.el:

```elisp
;; ============================================================
;; Terminal-friendly additional bindings
;; ============================================================
(when (not (display-graphic-p))
  ;; Additional C-c bindings for common operations
  (global-set-key (kbd "C-c /") 'consult-line)      ; Quick search
  (global-set-key (kbd "C-c .") 'embark-act)        ; Embark in terminal
  (message "Terminal mode: Using C-c s l, C-c s r for search"))
```

## 🎓 Learning Path for Terminal Users

1. **Day 1:** Master the `C-c s` prefix
   - `C-c s l` for buffer search
   - `C-c s r` for project search
   - `C-c s i` for jumping to functions

2. **Week 1:** Learn ESC alternatives
   - `ESC x` instead of `M-x`
   - `ESC .` instead of `M-.`
   - `ESC ,` instead of `M-,`

3. **Week 2:** Configure your terminal
   - Set Option/Alt as Meta
   - Test all key bindings
   - Make notes of what doesn't work

4. **Optional:** Switch to GUI for full experience
   - Better key binding support
   - Improved usability

## 📊 GUI vs Terminal Comparison

| Feature | GUI Emacs | Terminal Emacs |
|---------|-----------|----------------|
| All key bindings | ✅ Yes | ⚠️ Some conflicts |
| M-s prefix | ✅ Works | ❌ Often conflicts (use C-c s) |
| Mouse support | ✅ Full | ⚠️ Limited |
| Copy/paste | ✅ Easy | ⚠️ Terminal-dependent |
| Font rendering | ✅ Beautiful | ⚠️ Terminal-limited |
| Startup speed | ⚠️ Slower | ✅ Faster |
| Remote editing | ⚠️ SSH + X11 | ✅ Direct SSH |
| Resource usage | ⚠️ Higher | ✅ Lower |

## 🚀 Pro Tips

### Tip 1: Test Your Meta Key
```
1. Open emacs -nw
2. Press: M-x (or ESC x)
3. If you see "M-x" in the minibuffer → Meta works!
4. If nothing happens → Meta not configured, use ESC
```

### Tip 2: Create Terminal-Specific Aliases
```bash
# In ~/.zshrc or ~/.bashrc
alias et='emacs -nw'           # Terminal emacs
alias eg='emacs &'             # GUI emacs
alias eq='emacs -nw -q'        # Quick terminal (no config)
```

### Tip 3: Use emacsclient for Speed
```bash
# Start server once
emacs --daemon

# Connect with instant startup
emacsclient -t              # Terminal
emacsclient -c &            # GUI
```

### Tip 4: Keep Cheatsheet Handy
```bash
# In one terminal: your work
emacs -nw project/main.go

# In another terminal: reference
cat ~/.emacs.d/TERMINAL_TIPS.md
```

## ✅ Quick Setup Checklist

- [ ] Configure terminal to use Option/Alt as Meta
- [ ] Test `M-x` - does it work?
- [ ] Test `C-c s l` - search should work
- [ ] Test `ESC x` - alternative meta should work
- [ ] Decide: Terminal or GUI for main development?
- [ ] Create aliases for quick access
- [ ] Bookmark this file for reference

## 🔗 Additional Resources

- [EmacsWiki: Terminal Key Bindings](https://www.emacswiki.org/emacs/KeyBindingDiscussion)
- [Terminal Emulator Comparison](https://www.emacswiki.org/emacs/CategoryTerminalEmulator)
- Your config cheatsheet: `~/.emacs.d/CHEATSHEET.txt`

---

**Remember:**
- `C-c s l` is your friend in terminal!
- When in doubt, use `ESC` instead of `Alt/Option`
- GUI Emacs gives the full experience

Happy terminal Emacs-ing! 🚀
