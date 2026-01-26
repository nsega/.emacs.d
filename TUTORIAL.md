# Hands-On Tutorial: Exploring Your Codebase

This tutorial covers practical exercises for navigating codebases using your modernized Emacs setup. The configuration includes:

- **Vertico ecosystem** for completion (vertico, consult, marginalia, embark, orderless)
- **Eglot LSP** for intelligent code navigation
- **Tree-sitter** for accurate syntax highlighting (when grammars are installed)
- **Projectile** for project management

**Supported languages with full LSP:**
- Python (python-lsp-server or basedpyright)
- Go (gopls)
- TypeScript/JavaScript (typescript-language-server)
- C/C++ (clangd)
- Java (jdtls)
- Kotlin (kotlin-language-server)

---

## Part 1: Exploring the Go Demo Projects

### Exercise 1: Understanding the Calculator Package

**Scenario**: You want to understand how the calculator works and its tests.

#### Step-by-step:

1. **Open Emacs and navigate to your demo directory**
   ```
   C-x C-f  → ~/Library/CloudStorage/GoogleDrive.../nsega-notebook/demo/
   ```

2. **Find the calculator implementation**
   ```
   C-c p f  → Type: "calculator.go"
   → RET
   ```

   You should see:
   ```go
   package calculator

   func Add(a, b int) int {
       return a + b
   }
   // ... more functions
   ```

3. **See all functions in the file**
   ```
   M-g i
   ```

   Vertico will show:
   - `Add`
   - `Subtract`
   - `Multiply`
   - `Divide`

   Navigate with `C-j`/`C-k`, press `RET` on any function to jump to it.

4. **Find all places that use `Add`**
   ```
   Put cursor on "Add"
   M-?
   ```

   You'll see:
   - `calculator_test.go:15` - in TestAdd
   - `calculator_test.go:42` - in TestAddNegative
   - `calculator_table_test.go:28` - in table-driven test

   Navigate through results with `M-g M-n` (next) / `M-g M-p` (previous)

5. **Jump to the test file**
   ```
   C-c p a  → Toggle between implementation and test
   ```

   OR manually:
   ```
   C-c p f  → "calculator_test.go"
   ```

6. **View test structure**
   ```
   M-g i
   ```

   Shows:
   - `TestAdd`
   - `TestSubtract`
   - `TestMultiply`
   - `TestDivide`
   - Table-driven tests

7. **Search for assertion pattern**
   ```
   M-s l  → "t.Errorf"
   ```

   See all error handling in tests highlighted with live preview!

8. **Go back to where you started**
   ```
   M-,  → Pop back through your navigation history
   ```

---

### Exercise 2: Understanding Concurrency Patterns

**Scenario**: Compare the three counter implementations.

#### Step-by-step:

1. **Open the counter file**
   ```
   C-c p f  → "counter.go"
   ```

2. **See the three implementations**
   ```
   M-g i
   ```

   Shows:
   - [Types] AtomicCounter, MutexCounter, BuggyCounter
   - [Methods] Add, Value for each type

3. **Compare AtomicCounter and MutexCounter**

   a. Jump to AtomicCounter.Add:
   ```
   M-g i  → Type "Atomic"  → Select "Add"
   ```

   You'll see:
   ```go
   func (c *AtomicCounter) Add(delta int64) {
       atomic.AddInt64(&c.value, delta)
   }
   ```

   b. Now jump to MutexCounter.Add:
   ```
   M-g i  → Type "Mutex"  → Select "Add"
   ```

   You'll see:
   ```go
   func (c *MutexCounter) Add(delta int64) {
       c.mu.Lock()
       defer c.mu.Unlock()
       c.value += delta
   }
   ```

4. **Find all tests for these counters**
   ```
   M-?  → On "AtomicCounter" type name
   ```

   Shows all files using AtomicCounter

5. **Search for race conditions**
   ```
   M-s r  → "race"
   ```

   Finds:
   - `race_condition_test.go`
   - Comments mentioning race conditions

6. **Open the race condition test**
   ```
   Navigate to result → RET
   ```

---

### Exercise 3: Understanding Test Patterns

**Scenario**: Learn about table-driven tests.

#### Step-by-step:

1. **Find table-driven test example**
   ```
   C-c p f  → "calculator_table"
   ```

2. **Jump to the test table**
   ```
   M-s l  → "tests :="
   ```

   You'll see the test case table:
   ```go
   tests := []struct {
       name string
       a, b int
       want int
   }{
       {"positive numbers", 2, 3, 5},
       {"negative numbers", -2, -3, -5},
       ...
   }
   ```

3. **See how the table is used**
   ```
   M-s l  → "t.Run"
   ```

   Shows the loop iterating over test cases

4. **Find all table-driven tests in project**
   ```
   M-s r  → "t.Run"
   ```

   Finds all table-driven tests across all files!

---

## Part 2: Exploring Your Obsidian Vault

### Exercise 4: Finding Notes by Topic

**Scenario**: Find all notes about LeetCode problems.

#### Step-by-step:

1. **Search for LeetCode in your vault**
   ```
   C-c p p  → Select "nsega-notebook"
   M-s r    → "LeetCode"
   ```

   This searches across all 800+ markdown files!

2. **Navigate results**
   - Use `C-n`/`C-p` to preview different matches
   - Press `RET` to open the file
   - Press `M-g M-n` to go to next match after opening

3. **Jump to specific LeetCode problem**
   ```
   C-c p f  → Type problem number like "0001" or "two-sum"
   ```

4. **View note structure**
   ```
   M-g i
   ```

   Shows all headers in the markdown file:
   - # Problem
   - ## Approach
   - ## Solution
   - ## Complexity

---

### Exercise 5: Exploring Your Weekly Reviews

**Scenario**: Find patterns in your weekly reviews.

#### Step-by-step:

1. **Find all weekly review files**
   ```
   C-c p f  → "WeeklyReview"
   ```

   Or search:
   ```
   M-s r  → "weekly review"
   ```

2. **Open recent weekly review**
   ```
   C-c p f  → "2026"  → Select most recent
   ```

3. **See review structure**
   ```
   M-g i
   ```

   Shows all sections of your review template

4. **Search for specific topic across reviews**
   ```
   M-s r  → "accomplishments"
   ```

   See all accomplishments across all weeks!

---

### Exercise 6: Navigating MOC (Map of Content)

**Scenario**: Understand connections between notes.

#### Step-by-step:

1. **Open an MOC**
   ```
   C-c p f  → "MOC"
   ```

2. **Follow a link to another note**
   ```
   Put cursor on [[Note Name]]
   M-.  → Jumps to that note!
   ```

3. **Go back**
   ```
   M-,  → Returns to MOC
   ```

4. **Find all notes linking to current note**
   ```
   M-?  → Find references (backlinks)
   ```

---

## Part 3: JVM Development (Java/Kotlin/Gradle)

### Exercise 7: Navigating a Java/Kotlin Project

**Scenario**: Explore a Spring Boot or Android project with mixed Java/Kotlin code.

#### Step-by-step:

1. **Open your JVM project**
   ```
   C-c p p  -> Select your project
   ```

2. **Find a Java class**
   ```
   C-c p f  -> "Application.java" or "MainActivity.kt"
   ```

3. **View class structure**
   ```
   M-g i
   ```

   Shows:
   - Class declarations
   - Methods and functions
   - Inner classes

4. **Jump to a method definition**
   ```
   M-.  -> On method name
   ```

   LSP (JDTLS for Java, kotlin-language-server for Kotlin) finds the definition!

5. **Find all usages**
   ```
   M-?  -> On a class or method
   ```

6. **Navigate Gradle build files**
   ```
   C-c p f  -> "build.gradle" or "build.gradle.kts"
   ```

   - `.gradle` files use Groovy syntax (groovy-mode)
   - `.gradle.kts` files use Kotlin syntax (kotlin-mode)

7. **Check LSP status**
   ```
   Look for "Eglot" in mode line
   M-x eglot-reconnect  -> If LSP seems stuck
   ```

---

### Exercise 8: Refactoring in JVM Languages

**Scenario**: Rename a class or method safely across files.

#### Step-by-step:

1. **Navigate to the symbol**
   ```
   C-c p f  -> Find the file
   M-.      -> Jump to definition
   ```

2. **Rename with LSP**
   ```
   C-c r    -> eglot-rename
   Type new name
   RET
   ```

   LSP updates all references across the project!

3. **Verify the rename**
   ```
   M-s r  -> Search for old name (should find nothing)
   M-s r  -> Search for new name (should find all occurrences)
   ```

---

## Part 4: Power User Workflows

### Workflow 1: Code Review Pattern

**Use case**: Reviewing changes in a Go file

```
1. C-c p f     → Open file
2. M-g i       → See structure
3. M-.         → Jump to modified function
4. M-?         → See who calls it
5. M-s l       → Search for error handling
6. M-,         → Go back
```

### Workflow 2: Bug Investigation

**Use case**: Finding where a bug might be

```
1. M-s r       → Search error message across project
2. RET         → Jump to first occurrence
3. M-.         → Jump to function definition
4. M-?         → Find all callers
5. C-.         → Embark actions: "Open externally", "Grep"
```

### Workflow 3: Documentation Writing

**Use case**: Writing docs for a function

```
1. M-.         → Jump to function
2. M-g i       → See all functions in file
3. M-?         → Find usage examples
4. C-h .       → See LSP documentation
5. C-x b       → Switch to doc file
6. M-s l       → Search for similar docs
```

### Workflow 4: Refactoring

**Use case**: Renaming a function everywhere

```
1. M-.         → Jump to definition
2. C-c r       → eglot-rename
3. Type new name
4. RET         → Renames in ALL files!
5. C-c p s g   → Verify: search old name (should find nothing)
```

---

## Part 5: Advanced Techniques

### Technique 1: Multi-File Editing

**Scenario**: Add logging to all test functions

```
1. M-s r  → "func Test"            # Find all test functions
2. C-;    → iedit-mode             # Edit all at once
3. Type your changes
4. C-;    → Exit iedit             # Applied to all!
```

### Technique 2: Comparative Reading

**Scenario**: Compare atomic vs mutex counter

```
# Split window
C-x 3                              # Split vertically

# Left window: AtomicCounter
C-c p f  → "counter.go"
M-g i    → "AtomicCounter"

# Right window: MutexCounter
C-x o                              # Switch to other window
M-g i    → "MutexCounter"

# Now you can see both implementations side-by-side!
```

### Technique 3: Project-Wide Pattern Analysis

**Scenario**: Find all error handling patterns

```
M-s r  → "if err != nil"           # Find all error checks
→ View results
→ RET on interesting patterns
→ M-. to see context
→ M-? to see how it's used
```

### Technique 4: Bookmark Important Locations

**Scenario**: Mark key files for quick access

```
# In important file:
C-x r m  → Name: "main-config"     # Set bookmark

# Later, from anywhere:
C-x r b  → "main-config"           # Jump back!
```

---

## Part 6: Your Most Common Tasks

### Task: "Find a function I wrote last week"

```
C-x b       → Shows recent buffers
→ Type partial name
→ RET
→ M-g i     → Find function
```

### Task: "See all TODOs in project"

```
M-s r  → "TODO"
→ See all TODOs across all files!
```

### Task: "Find example of similar code"

```
M-s r  → Search pattern
→ Navigate results
→ RET on good example
```

### Task: "Jump between test and code"

```
C-c p a    → Toggle implementation/test
```

### Task: "Find all uses of a type"

```
M-.        → Jump to type definition
M-?        → Find all references
```

---

## Practice Challenges

Try these to master your new tools:

### Challenge 1: The Detective
**Goal**: Find which test uses the BuggyCounter

1. Open any file in demo/concurrency
2. Use search to find "BuggyCounter"
3. Jump to its test
4. See what makes it buggy
5. Find the bug in the implementation

<details>
<summary>Solution</summary>

```
M-s r  → "BuggyCounter"
→ RET on test file
→ M-. on BuggyCounter
→ See: no synchronization!
```
</details>

### Challenge 2: The Architect
**Goal**: Understand the structure of all counters

1. Open counter.go
2. View all types
3. Compare their implementations
4. Find their tests
5. See race condition examples

<details>
<summary>Solution</summary>

```
C-c p f  → "counter.go"
M-g i    → View all types
→ Jump to each type's methods
M-?      → Find tests
M-s r    → "race" to find race tests
```
</details>

### Challenge 3: The Refactorer
**Goal**: Find all Add methods across counters

1. Search for "Add(" in counter.go
2. See all implementations
3. Compare them
4. Find all callers

<details>
<summary>Solution</summary>

```
M-s l  → "func (c"
→ See all methods
M-?    → On each "Add" to find callers
```
</details>

---

## Cheat Sheet for Your Vault

### Quick Jumps
```
LeetCode problems:     C-c p f  → "30_Permanent/LeetCode"
Weekly reviews:        C-c p f  → "61_WeeklyReview"
Inbox:                 C-c p f  → "00_Inbox"
Fleeting notes:        C-c p f  → "10_FleetingNote"
MOCs:                  C-c p f  → "40_MOC"
```

### Quick Searches
```
Find TODO:             M-s r  → "TODO"
Find specific date:    M-s r  → "2026-01"
Find tag:              M-s r  → "#tag-name"
Find by YAML tag:      M-s r  → "tags: learning"
```

---

## Next Steps

1. **Today**: Practice Exercises 1-3 (Go demo projects)
2. **This Week**: Try JVM exercises (Part 3) and workflows (Part 4)
3. **Next Week**: Master advanced techniques in Part 5
4. **Month 1**: Complete all practice challenges

---

## Pro Tips from Your Setup

1. **Your most powerful combo**: `C-c p f` (find file) + `M-g i` (jump to section)
   - Works in code: jumps to functions
   - Works in markdown: jumps to headers

2. **Best for large files**: `M-s l` (consult-line) has LIVE preview

3. **Your LSP works in**: Python, Go, TypeScript, JavaScript, C/C++, Java, Kotlin
   - In these languages, `M-.` is incredibly smart!
   - Tree-sitter provides accurate syntax highlighting

4. **Ripgrep respects**: `.gitignore`, so searches are clean

5. **Remember**: `M-,` is your friend - it ALWAYS takes you back

6. **Terminal alternatives**: If Meta key conflicts in terminal, use `C-c s` prefix:
   - `C-c s l` instead of `M-s l` (consult-line)
   - `C-c s r` instead of `M-s r` (consult-ripgrep)
   - `C-c s g` instead of `M-s g` (consult-grep)

---

## Common Questions

**Q: How do I know if LSP is active?**
A: Look for "Eglot" in the mode line at the bottom

**Q: Why isn't M-. working?**
A: Check if you're in a supported language:
   - Python, Go, TypeScript, JavaScript, C/C++, Java, Kotlin
   For other files, it uses dumb-jump (still works, just less smart)

**Q: How do I know if Tree-sitter is active?**
A: The mode name will have "-ts-" in it (e.g., `go-ts-mode` instead of `go-mode`)

**Q: How to search in specific directory?**
A: `C-c p f` → Navigate to directory → `M-s g` or `M-s r`

**Q: Can I search markdown files?**
A: Yes! `M-s r` searches ALL file types

**Q: How to see just the files I edited recently?**
A: `C-x b` → Type nothing → See recent buffers at top

**Q: How do I rename a symbol across the project?**
A: Put cursor on symbol → `C-c r` (eglot-rename) → Type new name → RET

**Q: How to format code on save?**
A: Go files auto-format on save. For others: `C-c l =` or `M-x eglot-format-buffer`

---

**Happy exploring!**

Start with Exercise 1 and work your way through. Each exercise builds on the previous one.

*Remember: The key to mastering these tools is muscle memory. Practice daily!*
