# Hands-On Emacs Tutorial

**Learn by Doing: Navigate and Edit Code Effectively**

This tutorial teaches you the essential Emacs skills through practical exercises. Work through each section in order - skills build on each other.

---

## Prerequisites

Before starting, ensure you have:
- Emacs 30+ with this configuration loaded
- A project with source code (any language works)
- For LSP features: language server installed (gopls, pylsp, typescript-language-server, etc.)

**Tip:** Use `C-c s` prefix for all search commands - it works in both terminal and GUI Emacs.

---

## Part 1: Essential Navigation

Master these 5 commands first. They cover 80% of daily navigation.

### Exercise 1.1: Find Files in a Project

**Goal:** Open files quickly without navigating directories.

1. Open a project:
   ```
   C-x p p       -> Select a project (or open any git repo)
   ```

2. Find a file by partial name:
   ```
   C-x p f       -> Type partial filename
   ```

   **Try it:** Type just "main" or "test" - Vertico shows matching files instantly.

3. Use fuzzy matching:
   ```
   C-x p f       -> Type "usrctl" to match "user_controller.py"
   ```

   Terms can be in any order: "test user" matches "user_test.go"

### Exercise 1.2: Navigate Within a File

**Goal:** Jump to functions, classes, or sections without scrolling.

1. Open any source file, then:
   ```
   C-c s i       -> Shows all functions/classes/headers (consult-imenu)
   ```

2. Navigate the list:
   ```
   C-n / C-p     -> Move up/down
   RET           -> Jump to selection
   ```

3. Filter by typing:
   ```
   C-c s i       -> Type "add" to filter to functions containing "add"
   ```

### Exercise 1.3: Search Within Current File

**Goal:** Find text in the current buffer with live preview.

1. Search with live preview:
   ```
   C-c s l       -> Type search term (consult-line)
   ```

2. Navigate matches:
   ```
   C-n / C-p     -> Preview each match in real-time
   RET           -> Jump to selected match
   ```

3. **Try it:** Open a config file and search for "TODO" or "FIXME"

### Exercise 1.4: Switch Between Open Files

**Goal:** Quickly switch between buffers you've opened.

1. See recent buffers:
   ```
   C-x b         -> Shows buffers with preview
   ```

2. Navigate with preview:
   ```
   C-n / C-p     -> Preview buffer contents before switching
   RET           -> Switch to buffer
   ```

3. Filter by typing partial name

### Exercise 1.5: Go Back After Jumping

**Goal:** Return to where you were after navigation.

1. After any jump (M-., search, etc.):
   ```
   M-,           -> Pop back to previous location
   ```

2. Keep pressing `M-,` to go further back (stack-based)

**This is crucial:** `M-,` is your "undo navigation" - use it liberally!

---

## Part 2: Code Intelligence (LSP)

These features require a language server. Check for "Eglot" in the mode line.

### Exercise 2.1: Jump to Definition

**Goal:** Go directly to where a function/class is defined.

1. Put cursor on any function call or variable:
   ```
   M-.           -> Jump to definition
   ```

2. Go back:
   ```
   M-,           -> Return to where you were
   ```

3. **Try it:** Find a function call, press `M-.`, then `M-,` to return.

### Exercise 2.2: Find All References

**Goal:** See everywhere a symbol is used.

1. Put cursor on a function or variable name:
   ```
   M-?           -> Find all references
   ```

2. Navigate the xref buffer:
   ```
   C-n / C-p     -> Move through results
   RET           -> Jump to reference
   M-,           -> Return to xref list
   ```

### Exercise 2.3: View Documentation

**Goal:** See documentation for symbol at point.

1. Put cursor on a function or type:
   ```
   C-h .         -> Show documentation popup
   ```

### Exercise 2.4: Rename Symbol

**Goal:** Rename a variable/function across the entire project.

1. Put cursor on the symbol to rename:
   ```
   C-c r         -> eglot-rename
   ```

2. Type the new name and press `RET`

3. All references are updated automatically!

**Warning:** This modifies files. Review changes with `git diff` after.

---

## Part 3: Project-Wide Search

Search across all files in your project instantly.

### Exercise 3.1: Search with Ripgrep

**Goal:** Find text across all project files.

1. Search the entire project:
   ```
   C-c s r       -> Type search pattern (consult-ripgrep)
   ```

2. Navigate results with live preview:
   ```
   C-n / C-p     -> Preview matches in context
   RET           -> Jump to match
   ```

3. Use regex patterns:
   ```
   C-c s r       -> "func.*Test" finds all test functions
   ```

**Note:** Requires `ripgrep` installed (`brew install ripgrep`)

### Exercise 3.2: Search with Grep (Fallback)

**Goal:** Search when ripgrep isn't available.

```
C-c s g         -> consult-grep (slower but universal)
```

### Exercise 3.3: Find Files by Pattern

**Goal:** Find files matching a glob pattern.

1. Use project find-file with patterns:
   ```
   C-x p f       -> "*test*" finds all test files
   C-x p f       -> "*.md" finds all markdown files
   ```

---

## Part 4: Editing Efficiently

### Exercise 4.1: Edit Multiple Occurrences

**Goal:** Change all instances of a word at once.

1. Put cursor on a word:
   ```
   C-;           -> Enter iedit-mode (all occurrences highlighted)
   ```

2. Type to replace all simultaneously

3. Exit:
   ```
   C-;           -> Exit iedit-mode
   ```

### Exercise 4.2: Smart Copy/Cut Lines

**Goal:** Copy or cut entire lines without selecting.

1. Without any selection:
   ```
   M-w           -> Copy current line
   C-w           -> Cut current line
   ```

2. Paste:
   ```
   C-y           -> Paste (auto-indents in code files)
   ```

### Exercise 4.3: Search and Replace

**Goal:** Replace text in current buffer.

1. Start replace:
   ```
   M-%           -> Query replace (shows match count)
   ```

2. For each match:
   ```
   y             -> Replace this one
   n             -> Skip this one
   !             -> Replace all remaining
   q             -> Quit
   ```

---

## Part 5: Practical Workflows

### Workflow A: Exploring New Code

**Scenario:** You need to understand unfamiliar code.

```
1. C-x p p          -> Open the project
2. C-x p f          -> Find main entry point (main.go, index.ts, app.py)
3. C-c s i          -> View file structure
4. M-.              -> Jump into interesting functions
5. M-?              -> See who calls this function
6. M-,              -> Navigate back through your path
```

### Workflow B: Investigating a Bug

**Scenario:** Find where an error message comes from.

```
1. C-c s r          -> Search for error message text
2. RET              -> Jump to first match
3. M-.              -> Jump to function that generates it
4. M-?              -> Find all callers
5. C-c s l          -> Search for related error handling
```

### Workflow C: Refactoring a Function

**Scenario:** Rename and update a function safely.

```
1. C-x p f          -> Find file containing function
2. C-c s i          -> Jump to the function
3. M-?              -> Review all usages first
4. C-c r            -> Rename symbol (updates all files)
5. C-c s r          -> Verify: search old name (should find nothing)
```

### Workflow D: Code Review

**Scenario:** Review changes in a file.

```
1. C-x p f          -> Open the changed file
2. C-c s i          -> See structure
3. C-c s l          -> Search for specific changes
4. M-.              -> Understand what functions do
5. M-?              -> Check impact on callers
```

### Workflow E: Comparing Implementations

**Scenario:** View two files side by side.

```
1. C-x p f          -> Open first file
2. C-c s i          -> Jump to function of interest
3. C-x 3            -> Split window vertically
4. C-x o            -> Move to other window
5. C-x p f          -> Open second file
6. C-c s i          -> Jump to comparable function
7. C-x 1            -> Return to single window when done
```

---

## Part 6: Practice Challenges

Test your skills with these exercises.

### Challenge 1: The Navigator

**Goal:** Find all test files in your project, then find a specific test function.

<details>
<summary>Hint</summary>

```
C-x p f       -> Type "test" or "*_test*"
C-c s i       -> Find function in test file
```
</details>

### Challenge 2: The Detective

**Goal:** Find all TODO comments in your project and jump to each one.

<details>
<summary>Hint</summary>

```
C-c s r       -> "TODO"
C-n / C-p     -> Navigate results
RET           -> Jump to each
M-,           -> Return to results
```
</details>

### Challenge 3: The Tracer

**Goal:** Starting from any function, trace its call chain 3 levels deep, then return.

<details>
<summary>Hint</summary>

```
M-.           -> Jump to first called function
M-.           -> Jump to second level
M-.           -> Jump to third level
M-,           -> Back one level
M-,           -> Back two levels
M-,           -> Back to start
```
</details>

### Challenge 4: The Refactorer

**Goal:** Find all usages of a variable, then rename it project-wide.

<details>
<summary>Hint</summary>

```
M-?           -> Find all references (review first!)
M-.           -> Go to definition
C-c r         -> Rename symbol
```
</details>

### Challenge 5: The Analyst

**Goal:** Find all error handling patterns in your project.

<details>
<summary>Hint</summary>

```
# For Go:
C-c s r       -> "if err != nil"

# For Python:
C-c s r       -> "except.*:"

# For JavaScript/TypeScript:
C-c s r       -> "catch.*{"
```
</details>

---

## Quick Reference Card

### Navigation (Works Everywhere)
| Key | Terminal Alt | Action |
|-----|--------------|--------|
| `C-x p p` | - | Switch project |
| `C-x p f` | - | Find file in project |
| `C-x b` | - | Switch buffer |
| `M-g i` | `C-c s i` | Jump to symbol (imenu) |
| `M-,` | `ESC ,` | Go back |

### Search
| Key | Terminal Alt | Action |
|-----|--------------|--------|
| `M-s l` | `C-c s l` | Search current buffer |
| `M-s r` | `C-c s r` | Search project (ripgrep) |
| `M-s g` | `C-c s g` | Search project (grep) |

### Code Intelligence (LSP)
| Key | Terminal Alt | Action |
|-----|--------------|--------|
| `M-.` | `ESC .` | Jump to definition |
| `M-?` | `ESC ?` | Find references |
| `C-h .` | - | Show documentation |
| `C-c r` | - | Rename symbol |

### Editing
| Key | Action |
|-----|--------|
| `C-;` | Edit all occurrences (iedit) |
| `M-%` | Search and replace |
| `M-w` | Copy line (no selection) |
| `C-w` | Cut line (no selection) |

---

## Troubleshooting

### LSP Not Working?
1. Check mode line for "Eglot"
2. Try: `M-x eglot-ensure`
3. Restart: `M-x eglot-reconnect`
4. Verify language server is installed

### Search Commands Not Working?
- In terminal, always use `C-c s` prefix
- Install ripgrep: `brew install ripgrep`
- Check: `M-x consult-ripgrep` works directly?

### Meta Key Issues in Terminal?
- Use `ESC` then the key (e.g., `ESC .` for `M-.`)
- Or configure terminal: iTerm2 -> Preferences -> Profiles -> Keys -> Left Option Key -> Esc+

---

## Next Steps

1. **Today:** Complete Part 1 (Essential Navigation)
2. **This Week:** Work through Parts 2-4
3. **Next Week:** Practice the workflows in Part 5
4. **Ongoing:** Try one challenge per day until they're easy

**Remember:** Muscle memory takes practice. Use these commands daily!

---

## Additional Resources

- **USER_GUIDE.md** - Complete reference for all key bindings
- **README.md** - Configuration overview
- [Vertico Documentation](https://github.com/minad/vertico)
- [Eglot Manual](https://joaotavora.github.io/eglot/)
- [project.el Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)
