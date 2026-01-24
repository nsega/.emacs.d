# Modern Emacs User Guide

**For Emacs 30+ with Vertico, Eglot, and Projectile**

This guide covers all key bindings, workflows, and tips for navigating code effectively in this modernized Emacs configuration.

---

## Table of Contents

1. [Quick Reference](#quick-reference) - Essential key bindings
2. [Workflows](#workflows) - Common code reading patterns
3. [Terminal Emacs](#terminal-emacs) - Terminal-specific tips
4. [Advanced Features](#advanced-features) - Power user techniques
5. [Troubleshooting](#troubleshooting) - Common issues and solutions

---

## Quick Reference

### Most Important (Learn These First!)

| Key Binding | Description |
|-------------|-------------|
| `M-.` | Jump to definition (LSP-powered) |
| `M-,` | Go back (like browser back button) |
| `M-?` | Find all references |
| `M-g i` | Jump to function/class in file (imenu) |
| `M-s l` | Search lines in buffer (live preview) |
| `C-c p f` | Find file in project |
| `C-x b` | Switch buffer (shows recent files) |

### Project Navigation (Projectile)

| Key Binding | Description |
|-------------|-------------|
| `C-c p p` | Switch project |
| `C-c p f` | Find file in project |
| `C-c p a` | Toggle between code and test |
| `C-c p s g` | Search (grep) in project |
| `C-c p !` | Run shell command in project root |

### Search & Find (Consult)

| Key Binding | Description |
|-------------|-------------|
| `M-s l` | Search lines (consult-line) - LIVE PREVIEW |
| `M-s r` | Ripgrep search (fast, respects .gitignore) |
| `M-s g` | Grep search (slower but universal) |
| `C-x b` | Buffer list with preview |
| `M-g i` | Jump to function/section (consult-imenu) |

### Code Intelligence (Eglot LSP)

| Key Binding | Description | Works In |
|-------------|-------------|----------|
| `M-.` | Jump to definition | Python, Go, TypeScript, C/C++ |
| `M-?` | Find all references | Python, Go, TypeScript, C/C++ |
| `M-,` | Pop back to previous location | All modes |
| `C-h .` | Show documentation at point | LSP modes |
| `C-c r` | Rename symbol everywhere | LSP modes |

**Fallback:** dumb-jump for non-LSP languages

### Editing & Refactoring

| Key Binding | Description |
|-------------|-------------|
| `C-;` | Edit all occurrences (iedit-mode) |
| `M-%` | Replace (anzu shows count) |
| `C-M-%` | Regex replace |
| `C-c i` | Indent buffer or region |
| `M-w` | Copy line (no selection needed) |
| `C-w` | Cut line (no selection needed) |

### Advanced (Embark Actions)

| Key Binding | Description |
|-------------|-------------|
| `C-.` | Show actions for thing at point |
| `C-h B` | Show all key bindings |

**Tip:** Use `C-.` when you have a completion candidate selected!

### Vertico Navigation (While in completion)

| Key Binding | Description |
|-------------|-------------|
| `C-j` / `C-n` | Next item |
| `C-k` / `C-p` | Previous item |
| `RET` | Select item |
| `M-RET` | Submit exactly what you typed |
| `TAB` | Complete common prefix |

---

## Workflows

### Workflow 1: Exploring a New Project

**Goal**: Understand the structure and find entry points

```
1. C-c p p          → Select project
2. C-c p D          → Open project root in Dired
3. C-c p f          → Type "main" or "index" to find entry point
4. M-g i            → See all functions in file
5. M-.              → Jump to interesting function
6. M-?              → See who calls it
7. M-,              → Go back
```

### Workflow 2: Understanding a Specific Function

**Goal**: Trace execution flow and understand dependencies

```
1. C-c p f          → Find file containing function
2. M-s l            → Search for function name
3. M-.              → Jump to definition
4. M-?              → Find all references (who calls it?)
5. Navigate results → C-n/C-p through xref buffer
6. RET              → Jump to reference
7. M-,              → Return to previous location
```

### Workflow 3: Following Call Chain

**Goal**: Trace from high-level function down to implementation

```
1. M-.              → Jump to first function
2. Read code
3. M-.              → Jump to function it calls
4. Repeat...
5. M-,              → Pop back one level
6. M-,              → Pop back again (stack-based)
```

### Workflow 4: Project-Wide Search

**Goal**: Find all occurrences of a pattern

```
1. M-s r            → consult-ripgrep
2. Type pattern     → See live preview of matches
3. C-n/C-p          → Navigate results
4. RET              → Jump to match
5. M-,              → Return to search results
```

### Workflow 5: Refactoring a Symbol

**Goal**: Rename variable/function everywhere

```
1. M-.              → Jump to definition
2. C-c r            → eglot-rename
3. Type new name
4. RET              → Renames everywhere in project
```

### Workflow 6: Comparing Implementations

**Goal**: Compare similar functions or files

```
1. C-c p f          → Find first file
2. M-g i            → Jump to first function
3. C-x 3            → Split window vertically
4. C-x o            → Switch to other window
5. C-c p f          → Find second file
6. M-g i            → Jump to second function
7. Compare side-by-side
```

---

## Terminal Emacs

### Key Binding Differences

Some key bindings work differently in terminal Emacs vs GUI Emacs. Here are terminal-friendly alternatives:

#### Search & Navigation Alternatives

The `M-s` prefix often conflicts with terminal emulators:

| GUI Binding | Terminal Alternative | Command | Description |
|-------------|---------------------|---------|-------------|
| `M-s l` | **`C-c s l`** | consult-line | Search lines in buffer |
| `M-s g` | **`C-c s g`** | consult-grep | Grep search |
| `M-s r` | **`C-c s r`** | consult-ripgrep | Fast ripgrep search |
| `M-g i` | **`C-c s i`** | consult-imenu | Jump to function/section |

#### Other Common Issues

| Issue | GUI Binding | Terminal Alternative | Solution |
|-------|-------------|---------------------|----------|
| Meta key | `M-x` | `ESC x` | Use ESC as meta |
| C-. conflicts | `C-.` | `M-x embark-act` | Call directly |
| C-h conflicts | `C-h` | Works (fixed in config) | Mapped to backspace |

### Using ESC as Meta

In terminal Emacs, you can always use `ESC` instead of `Alt/Option`:

```
M-x  →  Press ESC, then x
M-.  →  Press ESC, then .
M-,  →  Press ESC, then ,
```

**Tip:** Press and release ESC (don't hold it), then press the next key.

### Configure Terminal Meta Key

#### macOS Terminal.app

1. Open Terminal → Preferences
2. Profiles → Keyboard
3. Check "Use Option as Meta key"

#### iTerm2 (Recommended)

1. Preferences → Profiles → Keys
2. Left Option Key → `Esc+`
3. Right Option Key → `Esc+`

#### kitty

Add to `~/.config/kitty/kitty.conf`:
```
macos_option_as_alt yes
```

#### Alacritty

Add to `~/.config/alacritty/alacritty.yml`:
```yaml
key_bindings:
  - { key: A, mods: Alt, chars: "\x1ba" }
  - { key: B, mods: Alt, chars: "\x1bb" }
  # ... (repeat for all letters)
```

### Terminal-Optimized Quick Reference

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

### GUI vs Terminal Comparison

| Feature | GUI Emacs | Terminal Emacs |
|---------|-----------|----------------|
| All key bindings | Yes | Some conflicts (use C-c s) |
| M-s prefix | Works | Often conflicts (use C-c s) |
| Mouse support | Full | Limited |
| Copy/paste | Easy | Terminal-dependent |
| Font rendering | Beautiful | Terminal-limited |
| Startup speed | Slower | Faster |
| Remote editing | SSH + X11 | Direct SSH |
| Resource usage | Higher | Lower |

### Recommended Terminal Workflow

**Option 1: Use GUI Emacs (Recommended)**
```bash
# Start GUI Emacs from terminal
emacs &

# Or create alias in ~/.zshrc:
alias e='emacs &'
```

**Benefits:** All key bindings work, better rendering, mouse support

**Option 2: Use Terminal Emacs with Alternatives**
```bash
# Use terminal emacs
emacs -nw

# Remember to use:
# - C-c s l instead of M-s l
# - C-c s r instead of M-s r
# - ESC x instead of M-x (if meta doesn't work)
```

**Option 3: Hybrid Approach**
```bash
# Quick edits in terminal
emacs -nw config.go

# Full development in GUI
emacs /path/to/project &
```

---

## Advanced Features

### Orderless Matching

Type search terms in any order:
- `"user test"` matches `test_user_function` or `user_management_test`
- Works in all vertico completions

### Live Previews

- `C-x b` - Preview buffers before switching
- `M-s l` - See matches as you type
- Navigate with `C-n`/`C-p` to peek

### Multi-Edit (iedit)

```
M-s l          → Find pattern
C-;            → Edit all occurrences at once
→ Type changes
C-;            → Exit iedit
```

### Embark Actions

```
C-x b          → Buffer list
→ Select buffer
C-.            → Show actions (view, save, kill, diff, etc.)
```

### Smart Line Operations

**No selection needed:**
- `M-w` copies current line
- `C-w` cuts current line
- Works when no region is active

### Auto-Indent on Yank

When you paste (`C-y`) in programming modes, code is automatically indented to match surrounding context.

---

## Troubleshooting

### LSP Not Working?

1. **Check if Eglot is active:** Look for "Eglot" in mode line
2. **Manually start:** `M-x eglot-ensure`
3. **Restart server:** `M-x eglot-reconnect`
4. **Check server is installed:**
   ```bash
   which gopls
   which pylsp
   which typescript-language-server
   ```

### Search is Slow?

Install ripgrep for blazing fast search:
```bash
brew install ripgrep
```
Then use `M-s r` instead of `M-s g`

### Can't Find Commands?

Check available commands:
```
M-x consult-  [TAB]    # See all consult commands
M-x projectile-  [TAB] # See all projectile commands
C-h B                  # Show all key bindings
```

### Terminal M-s l Does Nothing?

**Solution:** Use `C-c s l` instead. This is the terminal-friendly alternative.

**Test it:**
```
1. Open a file: emacs -nw ~/.bashrc
2. Press: C-c s l
3. Type search term
4. Should show matching lines!
```

### Meta Key Doesn't Work in Terminal?

**Solution 1:** Use ESC instead
```
Instead of: M-x
Press: ESC (release) then x
```

**Solution 2:** Configure terminal (see Terminal Emacs section above)

### C-. Doesn't Work?

**Solution:** Use the long form:
```
M-x embark-act
```

Or add a different binding in init.el:
```elisp
(global-set-key (kbd "C-c .") 'embark-act)
```

### Performance Issues?

Check package load times:
```
M-x use-package-report
```

---

## Pro Tips

- **Orderless matching:** `"user test"` matches `test_user` or `user_test`
- **C-x b shows PREVIEW** - use `C-n`/`C-p` to peek before opening
- **M-s l has LIVE PREVIEW** - see matches as you type!
- **M-, is your friend** - always takes you back
- **LSP = smart** - look for "Eglot" in mode line
- **Terminal users:** Memorize `C-c s l`, `C-c s r`, `C-c s i`
- **Use projectile:** `C-c p` for all project operations
- **Embark is powerful:** Try `C-.` on different things (files, buffers, symbols)

---

## Additional Resources

- **Tutorial:** See `TUTORIAL.md` for hands-on exercises
- **Configuration:** `init.el` - Main configuration file
- **Modernization:** `.claude/plans/modernization-for-emacs30.md` - What changed
- **Vertico Documentation:** https://github.com/minad/vertico
- **Eglot Manual:** https://joaotavora.github.io/eglot/
- **Projectile Docs:** https://docs.projectile.mx/
