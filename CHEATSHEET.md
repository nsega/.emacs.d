# Modern Emacs Code Reading Cheat Sheet

> **Vertico + Eglot + Projectile**

---

## 🎯 MOST IMPORTANT (Learn These First!)

| Key Binding | Description |
|-------------|-------------|
| `M-.` | Jump to definition (LSP-powered!) |
| `M-,` | Go back (like browser back button) |
| `M-?` | Find all references |
| `M-g i` | Jump to function/class in file (imenu) |
| `M-s l` | Search lines in buffer (live preview!) |
| `C-c p f` | Find file in project |
| `C-x b` | Switch buffer (shows recent files) |

---

## 📂 PROJECT NAVIGATION (Projectile)

| Key Binding | Description |
|-------------|-------------|
| `C-c p p` | Switch project |
| `C-c p f` | Find file in project |
| `C-c p a` | Toggle between code and test |
| `C-c p s g` | Search (grep) in project |
| `C-c p !` | Run shell command in project root |

---

## 🔍 SEARCH & FIND (Consult)

| Key Binding | Description |
|-------------|-------------|
| `M-s l` | Search lines (consult-line) - LIVE PREVIEW |
| `M-s r` | Ripgrep search (fast, respects .gitignore) |
| `M-s g` | Grep search (slower but universal) |
| `C-x b` | Buffer list with preview |
| `M-g i` | Jump to function/section (consult-imenu) |

### ⚠️ TERMINAL USERS: If M-s doesn't work, use these instead:

| Key Binding | Description |
|-------------|-------------|
| `C-c s l` | Search lines (alternative to M-s l) |
| `C-c s r` | Ripgrep search (alternative to M-s r) |
| `C-c s i` | Jump to function (alternative to M-g i) |

---

## 🧠 CODE INTELLIGENCE (Eglot LSP)

| Key Binding | Description |
|-------------|-------------|
| `M-.` | Jump to definition |
| `M-?` | Find all references |
| `M-,` | Pop back to previous location |
| `C-h .` | Show documentation at point |
| `C-c r` | Rename symbol everywhere (eglot-rename) |

**Works in:** Python, Go, TypeScript, C, C++
**Fallback:** dumb-jump for other languages

---

## ✏️ EDITING & REFACTORING

| Key Binding | Description |
|-------------|-------------|
| `C-;` | Edit all occurrences (iedit-mode) |
| `M-%` | Replace (anzu shows count) |
| `C-M-%` | Regex replace |
| `C-c i` | Indent buffer or region |
| `M-w` | Copy line (no selection needed) |
| `C-w` | Cut line (no selection needed) |

---

## ⚡ ADVANCED (Embark Actions)

| Key Binding | Description |
|-------------|-------------|
| `C-.` | Show actions for thing at point |
| `C-h B` | Show all key bindings |

**Tip:** Use `C-.` when you have a completion candidate selected!

---

## 📋 VERTICO NAVIGATION (While in completion)

| Key Binding | Description |
|-------------|-------------|
| `C-j` / `C-n` | Next item |
| `C-k` / `C-p` | Previous item |
| `RET` | Select item |
| `M-RET` | Submit exactly what you typed |
| `TAB` | Complete common prefix |

---

## 🎓 COMMON WORKFLOWS

### Find & Read Function
```
C-c p f  →  filename  →  M-g i  →  function name  →  RET
```

### Follow Code Flow
```
M-.  (jump to definition)  →  M-?  (who calls it?)  →  M-,  (go back)
```

### Search Entire Project
```
M-s r  →  search term  →  navigate results  →  RET
```

### Rename Everywhere
```
M-.  (jump to def)  →  C-c r  →  new name  →  RET
```

---

## 💡 PRO TIPS

- **Orderless matching:** `"user test"` matches `test_user` or `user_test`
- **C-x b shows PREVIEW** - use `C-n`/`C-p` to peek before opening
- **M-s l has LIVE PREVIEW** - see matches as you type!
- **M-, is your friend** - always takes you back
- **LSP = smart** - look for "Eglot" in mode line

---

## 🔧 TROUBLESHOOTING

| Issue | Solution |
|-------|----------|
| LSP not working? | `M-x eglot-ensure` |
| Slow search? | Install ripgrep: `brew install ripgrep` |
| Check packages? | `M-x use-package-report` |

---

## 🖥️ TERMINAL EMACS USERS

Some keys don't work in terminal? Use these alternatives:

| Instead of | Use |
|------------|-----|
| `M-x` | `ESC` then `x` (press ESC, release, then x) |
| `M-s l` | `C-c s l` |
| `M-s r` | `C-c s r` |
| `M-.` | `ESC` then `.` |

**Full terminal guide:** `~/.emacs.d/TERMINAL_TIPS.md`

---

## 📖 MORE HELP

| Resource | Path |
|----------|------|
| **Full Guide** | `~/.emacs.d/NAVIGATION_GUIDE.md` |
| **Hands-on Tutorial** | `~/.emacs.d/HANDS_ON_TUTORIAL.md` |
| **Terminal Tips** | `~/.emacs.d/TERMINAL_TIPS.md` ⭐ NEW |
| **Your Config** | `~/.emacs.d/init.el` |

---

*Generated for modernized Emacs configuration (Vertico + Eglot + Projectile)*
