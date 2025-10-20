# 🧠 Funmacs — Modern Emacs, Simplified

<div align="center">
  <img src="https://img.shields.io/github/v/release/mujaxso/funmacs?style=flat-square" alt="Release">
  <img src="https://img.shields.io/github/stars/mujaxso/funmacs?style=flat-square" alt="Stars">
  <img src="https://img.shields.io/github/forks/mujaxso/funmacs?style=flat-square" alt="Forks">
  <img src="https://img.shields.io/github/issues/mujaxso/funmacs?style=flat-square" alt="Open Issues">
  <img src="https://img.shields.io/github/last-commit/mujaxso/funmacs?style=flat-square" alt="Last Commit">
</div>

<div align="center">
  <img src="https://img.shields.io/badge/License-MIT-green.svg" alt="License: MIT">
  <img src="https://img.shields.io/badge/Emacs-29%2B-blue.svg" alt="Emacs Version">
  <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg" alt="PRs Welcome">
  <img src="https://img.shields.io/badge/Made%20with-Emacs%20Lisp-orange.svg" alt="Built with Emacs Lisp">
</div>

### Supported Languages
<div align="center">
  <img src="https://img.shields.io/badge/C-%2300599C.svg?style=for-the-badge&logo=c&logoColor=white" alt="C">
  <img src="https://img.shields.io/badge/C++-%2300599C.svg?style=for-the-badge&logo=c%2B%2B&logoColor=white" alt="C++">
  <img src="https://img.shields.io/badge/Python-%2314354C.svg?style=for-the-badge&logo=python&logoColor=white" alt="Python">
  <img src="https://img.shields.io/badge/Rust-%23000000.svg?style=for-the-badge&logo=rust&logoColor=white" alt="Rust">
  <img src="https://img.shields.io/badge/Go-%2300ADD8.svg?style=for-the-badge&logo=go&logoColor=white" alt="Go">
  <img src="https://img.shields.io/badge/Zig-%23000000.svg?style=for-the-badge&logo=zig&logoColor=white" alt="Zig">
  <img src="https://img.shields.io/badge/JavaScript-%23F7DF1E.svg?style=for-the-badge&logo=javascript&logoColor=black" alt="JavaScript">
  <img src="https://img.shields.io/badge/TypeScript-%23007ACC.svg?style=for-the-badge&logo=typescript&logoColor=white" alt="TypeScript">
  <img src="https://img.shields.io/badge/HTML5-%23E34F26.svg?style=for-the-badge&logo=html5&logoColor=white" alt="HTML5">
  <img src="https://img.shields.io/badge/CSS3-%231572B6.svg?style=for-the-badge&logo=css3&logoColor=white" alt="CSS3">
  <img src="https://img.shields.io/badge/JSON-%23FFF.svg?style=for-the-badge&logo=json&logoColor=black" alt="JSON">
  <img src="https://img.shields.io/badge/YAML-%23000000.svg?style=for-the-badge&logo=yaml&logoColor=white" alt="YAML">
  <img src="https://img.shields.io/badge/Markdown-%23000000.svg?style=for-the-badge&logo=markdown&logoColor=white" alt="Markdown">
  <img src="https://img.shields.io/badge/Nix-%23000000.svg?style=for-the-badge&logo=nixos&logoColor=white" alt="Nix">
</div>

<br><br>

**Skip the configuration rabbit hole.** Funmacs is a production-ready Emacs setup with modern tooling baked in—LSP integration using eglot, tree-sitter parsing, and a refined completion experience. Its modular core/ and modules/ architecture means you can extend or strip down features without breaking anything.

> **Funmacs** is a **modern, production-ready Emacs configuration** built for developers who want a powerful yet elegant experience — **no yak-shaving required.**  
>  
> It’s built around a modular architecture, integrates **LSP**, **Tree-sitter**, and **Corfu-based completion**, and delivers a polished UX that just works out of the box.

---

<video src="https://github.com/user-attachments/assets/6aba4bbf-591a-4198-8597-613cb3c01877" controls width="720"></video>

---

## 📑 Table of Contents

1. [✨ Highlights](#-highlights)  
2. [🎨 UI & Aesthetics](#-ui--aesthetics)  
3. [⚡ Editing Experience](#-editing-experience)  
4. [🧠 Completion System](#-completion-system)  
5. [🛠 Development Features](#-development-features)  
6. [🌍 Supported Languages](#-supported-languages)  
    - [C / C++](#c--c)  
    - [Python](#python)  
    - [Rust](#rust)  
    - [Go](#go)  
    - [Zig](#zig)  
    - [JavaScript & TypeScript](#javascript--typescript)  
    - [Web Development](#web-development)  
    - [Configuration & Markup](#configuration--markup)  
    - [Markdown](#markdown)  
7. [🚀 Installation](#-installation)  
8. [💖 Support](#-support)  
9. [🔗 Related Projects](#-related-projects)  
10. [⚖️ License](#%EF%B8%8F-license)

---

## ✨ Highlights

- 🚀 **Zero Setup** — Start coding instantly with pre-configured defaults.  
- 🧩 **Modular Design** — Enable or disable features with minimal effort.  
- ⚙️ **Modern Toolchain** — Tree-sitter, Eglot, and Corfu baked in.  
- 🎯 **Optimized for Productivity** — Smart defaults and minimal distractions.  
- 🧘 **Clean Aesthetic** — A distraction-free, beautiful interface.

---

## 🎨 UI & Aesthetics

- 🪄 Minimal interface — no unnecessary menu/tool bars.  
- ⚡ **Doom modeline** for rich status info with Nerd Font icons.  
- 💡 **Which-key** integration to guide you through keybindings.  
- 🎨 **Custom themes** for both light and dark modes.  
- 🧘 Clean typography and layout for long coding sessions.

---

## ⚡ Editing Experience

- ✨ Automatic tag closing in markup and HTML modes.  
- 🔢 Relative line numbers for easier navigation.  
- 🧭 **Vundo** — tree-structured undo/redo history.  
- 🔇 Bell sound disabled (no more beeps!).  
- 💨 Smooth scrolling and consistent indentation defaults.

---

## 🧠 Completion System

| Component | Description |
|------------|--------------|
| **Vertico** | Lightweight and fast minibuffer completion UI. |
| **Orderless** | Fuzzy, flexible, and intuitive matching style. |
| **Corfu** | Pop-up completion menu for inline suggestions. |
| **Embark** | Contextual actions for completion items. |
| **Cape** | Adds additional completion sources (symbols, files, etc.). |
| **Nerd Icons Corfu** | Displays icons alongside completion candidates. |

> 🧩 *The result is a refined, modern completion experience that rivals VS Code or JetBrains IDEs—without leaving Emacs.*

---

## 🛠 Development Features

- 🧰 **Eglot (LSP)** – Language Server Protocol integration for code intelligence.  
- 🌳 **Tree-sitter** – Modern syntax highlighting and structural parsing.  
- ⚙️ **Automatic grammar installation** for supported languages.  
- ✨ **Apheleia** – Asynchronous and fast code formatting.  
- 📦 Language-specific **template snippets** included by default.  
- 🧠 Intelligent indentation and syntax-aware editing.

---

## 🌍 Supported Languages

Funmacs ships with out-of-the-box configurations for popular programming ecosystems.  
Each language module includes LSP integration, formatting, syntax highlighting, and completion support.

### 🧱 C / C++

- LSP via `clangd` for autocompletion, diagnostics, and symbol navigation.  
- Tree-sitter highlighting for precise syntax awareness.  
- On-save formatting with `clang-format`.  
- Ready for CMake-based and Makefile projects.

### 🐍 Python

- LSP powered by `pyright` or `pylsp`.  
- Built-in support for virtual environments.  
- Automatic formatting with **Black** or **YAPF**.  
- REPL integration and code navigation included.

### 🦀 Rust

- Uses `rust-analyzer` for advanced IDE features.  
- Inline diagnostics and code actions.  
- Built-in formatter (`rustfmt`) and cargo command shortcuts.  
- Tree-sitter for rich syntax highlighting.

### 🐹 Go

- LSP with `gopls` and on-save formatting.  
- Auto-imports and completion for packages.  
- Integrated testing workflow via `go test`.  

### ⚡ Zig

- LSP via `zls` (Zig Language Server).  
- Tree-sitter grammar for Zig syntax highlighting.  
- Integrated build and run commands for quick iteration.  
- Formatter support using `zig fmt`.  
- Autocompletion for symbols, imports, and built-ins.  

### ⚡ JavaScript & TypeScript

- LSP via `typescript-language-server`.  
- Tree-sitter-based highlighting for JS/TS/JSX/TSX.  
- Support for ESLint and Prettier formatting.  
- Module snippets for **React**, **Vue**, and **Svelte** frameworks.

### 🌐 Web Development

- First-class HTML and CSS support.  
- Autoclosing tags, attribute completion, and live linting.  
- Framework-specific modules for React, Vue, Svelte, TailwindCSS.

### ⚙️ Configuration & Markup

- Syntax highlighting for **JSON**, **YAML**, **Nix**, and more.  
- Tree-sitter parsing ensures accurate indentation and folding.  
- LSP and schema validation for configuration files.

### 📝 Markdown

- `markdown-mode` for writing and syntax highlighting.  
- `markdown-preview-mode` for real-time preview in another buffer.  
- Perfect for documentation, notes, and technical writing.

---

## 🚀 Installation

Clone directly into your Emacs configuration directory:

```bash
git clone https://github.com/mujaxso/funmacs.git ~/.config/emacs
```

## 💖 Support

If you enjoy Funmacs and want to **support ongoing development**, you can:

**Buy me a coffee:**  
[![Buy Me A Coffee](https://www.buymeacoffee.com/assets/img/custom_images/purple_img.png)](https://buymeacoffee.com/mujaxso)

**Or become a patron on Patreon:**  
[![Patreon](https://c5.patreon.com/external/logo/become_a_patron_button@2x.png)](https://www.patreon.com/mujaxso)

Your support helps keep Funmacs maintained, updated, and evolving.

---

## 🔗 Related Projects

- [**NEOTE**](https://github.com/mujaxso/neote) – Lightweight next-gen text editor.  
- [**MujaOS**](https://github.com/mujaxso/mujaos) – Modern and secure operating system.

---

## ⚖️ License

Funmacs is released under the **MIT License**.  
See [LICENSE](https://github.com/mujaxso/funmacs/blob/main/LICENSE) for full details.

---

> 🧩 **FOSS:** [foss.land](https://www.foss.land) &nbsp;•&nbsp;  
> 💻 **GitHub:** [@mujaxso](https://github.com/mujaxso) &nbsp;•&nbsp;  
> 🐦 **Twitter/X:** [@mujaxso](https://twitter.com/mujaxso)

