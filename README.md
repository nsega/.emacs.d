# .emacs.d

Personal Emacs configuration optimized for Emacs 30+ on macOS.

## Requirements

- **Emacs 30.x** or later
- **macOS** (Apple Silicon supported)
- **Homebrew** for package management

## Features

### Core Features
- **Helm** - Incremental completion and selection narrowing framework
- **Projectile** - Project interaction library
- **Company** - Modular completion framework
- **Yasnippet** - Template system
- **Smartparens** - Automatic parenthesis pairing
- **Migemo** - Japanese incremental search with romaji input

### LSP Support (via Eglot)
Built-in Eglot LSP client provides:
- Code completion
- Go to definition / Find references
- Real-time diagnostics
- Code formatting

## Language Support

| Language   | Mode               | LSP Server                     | Install Command |
|------------|--------------------|--------------------------------|-----------------|
| C/C++      | cc-mode            | -                              | Built-in        |
| Python     | python-mode        | pylsp                          | `pip install python-lsp-server` |
| Go         | go-mode            | gopls                          | `go install golang.org/x/tools/gopls@latest` |
| TypeScript | typescript-ts-mode | typescript-language-server     | `npm install -g typescript-language-server typescript` |
| YAML       | yaml-mode          | yaml-language-server           | `npm install -g yaml-language-server` |
| Markdown   | markdown-mode      | -                              | `brew install pandoc` (optional) |

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/nsega/.emacs.d.git ~/.emacs.d
```

### 2. Install external dependencies

```bash
# Japanese search support
brew install cmigemo

# Markdown preview (optional)
brew install pandoc
```

### 3. Install LSP servers

```bash
# Python
pip install python-lsp-server

# Go
go install golang.org/x/tools/gopls@latest

# TypeScript
npm install -g typescript-language-server typescript

# YAML (optional)
npm install -g yaml-language-server
```

### 4. Launch Emacs

On first launch, packages will be automatically installed from MELPA.

```bash
emacs
```

## Key Bindings

### General
| Key         | Command                  | Description                    |
|-------------|--------------------------|--------------------------------|
| `C-c h`     | helm-command-prefix      | Helm commands                  |
| `M-x`       | helm-M-x                 | Execute command with Helm      |
| `C-x b`     | helm-mini                | Switch buffer with Helm        |
| `C-x C-f`   | helm-find-files          | Find files with Helm           |
| `C-x 1`     | zygospore-toggle         | Toggle single window           |
| `C-c w`     | whitespace-mode          | Toggle whitespace visibility   |
| `C-c i`     | indent-region-or-buffer  | Indent region or buffer        |

### Code Navigation (helm-gtags)
| Key         | Command                  | Description                    |
|-------------|--------------------------|--------------------------------|
| `M-.`       | helm-gtags-dwim          | Jump to definition             |
| `M-,`       | helm-gtags-pop-stack     | Return from definition         |
| `C-c g a`   | helm-gtags-tags-in-this-function | Show tags in function |
| `C-j`       | helm-gtags-select        | Select tag                     |

### Editing
| Key         | Command                  | Description                    |
|-------------|--------------------------|--------------------------------|
| `M-;`       | comment-dwim-2           | Smart comment                  |
| `C-;`       | iedit-mode               | Edit multiple occurrences      |
| `M-o`       | open-line                | Open new line                  |
| `C-a`       | prelude-move-beginning   | Smart beginning of line        |

## Directory Structure

```
~/.emacs.d/
├── init.el              # Main configuration file
├── custom/              # Custom elisp modules
│   ├── setup-helm.el
│   ├── setup-helm-gtags.el
│   ├── setup-ggtags.el
│   ├── setup-cedet.el
│   └── setup-editing.el
└── elpa/                # Installed packages (auto-generated)
```

## Customization

### Adding new packages

Add package names to the `demo-packages` list in `init.el`:

```elisp
(defconst demo-packages
  '(anzu
    company
    ;; Add your packages here
    your-new-package))
```

### Configuring LSP for new languages

```elisp
;; Add hook for your language mode
(add-hook 'your-mode-hook 'eglot-ensure)

;; Configure LSP server if needed
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(your-mode . ("your-lsp-server" "--stdio"))))
```

## Troubleshooting

### Packages fail to install

```bash
rm -rf ~/.emacs.d/elpa
emacs
```

### LSP server not found

Ensure your PATH is correctly set up. This config uses `exec-path-from-shell` to inherit PATH from your shell.

```bash
# Verify LSP server is in PATH
which gopls
which pylsp
which typescript-language-server
```

### Japanese input (migemo) not working

```bash
brew install cmigemo
```

## License

MIT
