# Modern Emacs Navigation Guide

## 🎯 Quick Reference: Key Bindings for Code Reading

### Opening Files & Buffers

| Key | Command | What It Does |
|-----|---------|--------------|
| `C-x C-f` | `find-file` | Find/open file (with vertico completion) |
| `C-x b` | `consult-buffer` | Switch buffers (shows recent files, buffers) |
| `C-c p f` | `projectile-find-file` | Find file in current project |
| `C-c p p` | `projectile-switch-project` | Switch to another project |
| `C-c p s g` | `projectile-grep` | Search across project files |

### Navigation Within Files

| Key | Command | What It Does |
|-----|---------|--------------|
| `M-g i` | `consult-imenu` | Jump to function/class/section in current file |
| `M-s l` | `consult-line` | Search lines in current buffer (live preview) |
| `C-s` | `isearch-forward` | Incremental search forward |
| `C-r` | `isearch-backward` | Incremental search backward |

### Code Intelligence (LSP via Eglot)

| Key | Command | What It Does | Works In |
|-----|---------|--------------|----------|
| `M-.` | `xref-find-definitions` | Jump to definition | Python, Go, TypeScript, C/C++ |
| `M-?` | `xref-find-references` | Find all references | Python, Go, TypeScript, C/C++ |
| `M-,` | `xref-pop-marker-stack` | Go back to previous location | All modes |
| `C-h .` | `eldoc-doc-buffer` | Show documentation at point | LSP modes |
| `C-c r` | `eglot-rename` | Rename symbol everywhere | LSP modes |

### Search & Replace

| Key | Command | What It Does |
|-----|---------|--------------|
| `M-s g` | `consult-grep` | Grep search in directory |
| `M-s r` | `consult-ripgrep` | Fast ripgrep search (if rg installed) |
| `M-%` | `anzu-query-replace` | Interactive replace (shows count) |
| `C-M-%` | `anzu-query-replace-regexp` | Regex replace |
| `C-;` | `iedit-mode` | Edit all occurrences simultaneously |

### Contextual Actions

| Key | Command | What It Does |
|-----|---------|--------------|
| `C-.` | `embark-act` | Show actions for thing at point |
| `C-h B` | `embark-bindings` | Show all key bindings |

### Project Management

| Key | Command | What It Does |
|-----|---------|--------------|
| `C-c p` | `projectile-command-map` | Project commands prefix |
| `C-c p !` | `projectile-run-shell-command-in-root` | Run shell command in project root |
| `C-c p &` | `projectile-run-async-shell-command-in-root` | Run async command |

---

## 📚 Workflows for Reading Code

### Workflow 1: Exploring a New Project

**Goal**: Understand the structure and find entry points

1. **Open the project**
   ```
   C-c p p  → Select project
   ```

2. **Get project overview**
   ```
   C-c p D  → Open project root in Dired
   ```

3. **Find the main entry point**
   ```
   C-c p f  → Type "main" or "index" or "app"
   ```

4. **See file structure**
   ```
   M-g i    → View all functions/classes in file
   ```

5. **Search for key concepts**
   ```
   C-c p s g → Search for keywords across project
   ```

---

### Workflow 2: Following Code Flow

**Goal**: Understand how a function works by following its calls

1. **Jump to function definition**
   ```
   M-.      → Jump to definition (works for functions, classes, variables)
   ```

2. **See who calls this function**
   ```
   M-?      → Find all references
   ```

3. **Navigate through results**
   ```
   M-g M-n  → Next reference
   M-g M-p  → Previous reference
   ```

4. **Return to where you started**
   ```
   M-,      → Pop back (works like browser back button)
   ```

**Example**: Understanding a Python function
```python
# Cursor on "process_data"
def main():
    result = process_data(input)  # M-. here jumps to definition

# After M-., you're here:
def process_data(data):
    cleaned = clean_data(data)    # M-. again to dive deeper
    return analyze(cleaned)

# M-, to go back up the call stack
```

---

### Workflow 3: Searching for Patterns

**Goal**: Find all occurrences of a pattern in the codebase

1. **Search with live preview**
   ```
   M-s l    → Search current buffer (see matches as you type)
   ```

2. **Search across project**
   ```
   M-s r    → Ripgrep search (fast, respects .gitignore)
   M-s g    → Grep search (slower but more universal)
   ```

3. **Navigate results**
   - Results appear in vertico
   - `C-n`/`C-p` to move through results
   - `RET` to jump to that result

4. **Refine search**
   - Use orderless matching: "func test" matches "test_function" or "function_test"
   - Use regex: "def .*test.*:" finds all test functions

---

### Workflow 4: Understanding Data Structures

**Goal**: See how a class or struct is used

1. **Jump to class definition**
   ```
   M-.      → On class name
   ```

2. **See all methods/fields**
   ```
   M-g i    → Shows imenu outline
   ```

3. **Find all usages**
   ```
   M-?      → Find all references to this class
   ```

4. **See documentation**
   ```
   C-h .    → Show documentation at point (via LSP)
   ```

**Example**: Exploring a Go struct
```go
// M-. on "User" jumps here:
type User struct {
    ID   int
    Name string
}

// M-g i shows:
// [Variables]
// - ID
// - Name
// [Methods]
// - GetName
// - SetName

// M-? shows all places using User type
```

---

### Workflow 5: Quick Jumps Within File

**Goal**: Navigate large files efficiently

1. **Jump to any function**
   ```
   M-g i    → Type function name → RET
   ```

2. **Search within buffer**
   ```
   M-s l    → Type search term (with live preview)
   ```

3. **Jump to line number**
   ```
   M-g g    → Enter line number
   ```

4. **Bookmark important locations**
   ```
   C-x r m  → Set bookmark
   C-x r b  → Jump to bookmark
   ```

---

## 🔍 Advanced Techniques

### Using Embark for Context Actions

When you have a completion candidate selected (in vertico), press `C-.` to see what you can do with it:

```
C-x b         → Open buffer list
→ Select a buffer
→ C-.         → Shows actions: "View", "Save", "Kill", "Diff", etc.
```

### Chaining Consult Commands

Consult commands can be combined:

1. **Find file then search in it**
   ```
   C-c p f  → Find file in project
   M-s l    → Search lines in that file
   ```

2. **Switch buffer then jump to function**
   ```
   C-x b    → Switch to buffer
   M-g i    → Jump to function
   ```

### Using Orderless Matching

Orderless lets you type search terms in any order:

- **"user test"** matches:
  - "test_user_function"
  - "function_test_for_user"
  - "user_management_test"

- **"py main func"** matches:
  - "main_function.py"
  - "func_in_main.py"

### Project-Wide Refactoring

1. **Rename symbol everywhere**
   ```
   M-.         → Jump to definition
   C-c r       → eglot-rename
   → Type new name
   → RET       → Renames in all files!
   ```

2. **Edit multiple occurrences**
   ```
   M-s l       → Find pattern
   C-;         → iedit-mode (edits all at once)
   → Type changes
   → C-; again → Exit iedit
   ```

---

## 🎓 Learning Path

### Level 1: Basic Navigation (Day 1)
- [ ] `C-x C-f` - Open files
- [ ] `C-x b` - Switch buffers
- [ ] `M-.` - Jump to definition
- [ ] `M-,` - Go back
- [ ] `M-g i` - View file outline

### Level 2: Search & Find (Week 1)
- [ ] `M-s l` - Search in buffer
- [ ] `C-c p f` - Find file in project
- [ ] `M-?` - Find references
- [ ] `M-s r` - Search across project

### Level 3: Advanced (Week 2)
- [ ] `C-.` - Embark actions
- [ ] `C-;` - Multi-edit with iedit
- [ ] `C-c r` - Rename with LSP
- [ ] Orderless search patterns

### Level 4: Master (Month 1)
- [ ] Combine workflows
- [ ] Use bookmarks effectively
- [ ] Create custom projectile searches
- [ ] Master embark actions

---

## 💡 Pro Tips

### 1. Use Consult-Buffer Preview
When using `C-x b`, consult shows a live preview. Navigate with `C-n`/`C-p` to peek at files before opening!

### 2. Narrow Consult Results
In consult-buffer (`C-x b`):
- Type `b SPC` to show only buffers
- Type `f SPC` to show only files
- Type `p SPC` to show only projects

### 3. Vertico Navigation
In any vertico completion:
- `C-j`/`C-k` - Next/previous (custom bindings)
- `C-n`/`C-p` - Next/previous (default)
- `M-RET` - Submit exactly what you typed (ignore suggestions)

### 4. LSP Features to Remember
When Eglot is active (Python, Go, TypeScript, C/C++):
- Hover over symbol → See documentation in echo area
- `C-h .` → Full documentation in separate buffer
- Company completion is LSP-powered (smart!)

### 5. Projectile Root Detection
Projectile auto-detects project roots by looking for:
- `.git/` directory
- `go.mod` (Go projects)
- `package.json` (Node projects)
- `.projectile` file (manual marker)

---

## 🐛 Troubleshooting

### LSP not working?
```elisp
M-x eglot-ensure        ; Manually start LSP
M-x eglot-reconnect     ; Restart LSP server
M-x eglot-shutdown-all  ; Stop all LSP servers
```

### Slow project search?
- Install ripgrep: `brew install ripgrep`
- Use `M-s r` instead of `M-s g`

### Can't find definition?
- Make sure you're in a supported language (Python, Go, TypeScript, C/C++)
- Check if LSP server is running: Look for "Eglot" in mode line
- For other languages, `M-.` uses dumb-jump (still works, just less smart)

### Completion not showing?
- Company auto-activates after typing 2 characters
- Force completion: `TAB` in C/C++ modes
- Check company is active: `M-x company-mode`

---

## 📖 Real-World Example: Reading Go Code

Let's explore the demo/concurrency directory:

```bash
# 1. Open project
C-c p p → Select project → /path/to/demo

# 2. Find the main test file
C-c p f → "counter_test" → RET

# 3. See all tests in file
M-g i → Shows:
  - TestAtomicCounter
  - TestMutexCounter
  - TestBuggyCounter
  - TestWorkerPool

# 4. Jump to TestAtomicCounter
M-g i → Type "atomic" → RET

# 5. See what AtomicCounter does
M-. on "AtomicCounter" → Jumps to definition

# 6. Find all places using AtomicCounter
M-? → Shows all references

# 7. Navigate references
M-g M-n → Next reference
M-g M-p → Previous reference

# 8. Go back to test
M-, → Back to TestAtomicCounter

# 9. Search for "Add" method calls
M-s l → "Add(" → See all Add calls highlighted

# 10. Compare with mutex version
C-c p f → "counter.go" → RET
M-g i → "MutexCounter" → RET
```

---

## 🎯 Common Tasks Quick Reference

| Task | Keys | Workflow |
|------|------|----------|
| "Find main function" | `C-c p f` → "main" | Project find file |
| "Find all TODOs" | `M-s r` → "TODO" | Ripgrep search |
| "Jump to test" | `C-c p a` | Toggle between code and test |
| "See call hierarchy" | `M-.` then `M-?` | Definition then references |
| "Rename variable" | `M-.` then `C-c r` | LSP rename |
| "Find configuration" | `C-c p f` → "config" | Find file |
| "Search imports" | `M-s l` → "import" | Line search |

---

## 📚 Further Learning

### Check Package Statistics
```elisp
M-x use-package-report    ; See load times of all packages
```

### Explore Vertico Features
- Official docs: https://github.com/minad/vertico
- Try `vertico-reverse-mode` for completion at bottom

### Learn More Consult Commands
```
M-x consult-  [TAB]       ; See all consult commands
```

### Projectile Cheat Sheet
```
C-c p C-h                 ; Show all projectile commands
```

---

## 🔗 Resources

- **Vertico**: https://github.com/minad/vertico
- **Consult**: https://github.com/minad/consult
- **Eglot Manual**: `C-h i` → Eglot
- **Projectile Docs**: https://docs.projectile.mx
- **Your Config**: `C-x C-f ~/.emacs.d/init.el`

---

**Happy Code Reading! 🚀**

*Generated with Claude Code for modernized Emacs configuration*
