# Org-mode Linking by ID

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-26.1+-blue.svg)](https://www.gnu.org/software/emacs/)
[![Org-mode](https://img.shields.io/badge/Org--mode-9.0+-green.svg)](https://orgmode.org/)

Extension for Emacs Org-mode to create cross-links between headers using IDs instead of header content.

## 🌟 Features

### 1. Link Headers in Current File
- Find all headers with IDs in the currently open file
- Allow selection of headers from a list
- Insert ID links at cursor position

### 2. Link Headers in Directory
- Find headers with IDs in all .org files in a directory (excluding subdirectories)
- Use Ivy with fuzzy search for easy discovery
- Interactive directory selection

### 3. Link to Named Images/Tables
- Find objects with `#+NAME` and `#+CAPTION`
- Display caption list for user selection
- Allow customization of link description

## 🚀 Installation

### System Requirements

- Emacs 26.1 or higher
- Org-mode
- Ivy (for fuzzy search)
- Helm (for object search)

### Installation Methods

#### Method 1: Clone repository

```bash
git clone https://github.com/drringo/orgmode-linked-by-id.git
```

Add to your config file (`.emacs`, `init.el`, or `config.el`):
```elisp
(load "~/path/to/orgmode-linked-by-id/orgmode-linked-by-id.lisp")
```

#### Method 2: Using use-package

```elisp
(use-package orgmode-linked-by-id
  :load-path "~/path/to/orgmode-linked-by-id"
  :config
  (require 'orgmode-linked-by-id))
```

## ⌨️ Keybindings

| Keybinding | Function | Description |
|------------|----------|-------------|
| `C-c l i` or `SPC l i` | Insert ID link in current file | Find headers in current file |
| `C-c l f` or `SPC l f` | Insert ID link in directory | Find headers in directory |
| `C-c l c` or `SPC l c` | Insert link to named image/table | Find objects with NAME/CAPTION |

## 📖 Usage Guide

### Basic Workflow

1. **Create ID for header**: `SPC m I`
2. **Create link to another header**:
   - `C-c l i`: Search in current file
   - `C-c l f`: Search in directory
   - `C-c l c`: Find named images/tables
3. Select header from list
4. Customize description if needed
5. Link is inserted at cursor position

### Usage Example

```org
* Header 1
:PROPERTIES:
:ID: abc123
:END:

* Header 2
:PROPERTIES:
:ID: def456
:END:

Content referencing [[id:abc123][Header 1]] and [[id:def456][Header 2]].

#+NAME: my-image
#+CAPTION: Sample image
[[file:image.png]]

Link to [[my-image][Sample image]].
```

## 🎯 Why Use IDs

- **Avoid issues when header content changes**
- **No need to think about CUSTOM_ID**
- **IDs are automatically generated using `org-id-get-create`**

## 📊 Comparison with Traditional Method

| Traditional Method | This Extension |
|-------------------|----------------|
| `[[*Header name][Header name]]` | `[[id:abc123][Header name]]` |
| Breaks when header changes | Still works |
| Difficult to search | Easy to search |

## 🤝 Contributing

All contributions are welcome! Please:

1. Fork the repository
2. Create a new branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Create a Pull Request

## 📝 License

This project is distributed under the MIT license. See the [LICENSE](LICENSE) file for more details.

## 🔗 Links

- **Repository**: https://github.com/drringo/orgmode-linked-by-id
- **Issues**: https://github.com/drringo/orgmode-linked-by-id/issues
- **Discussions**: https://github.com/drringo/orgmode-linked-by-id/discussions

## 📚 Additional Documentation

See the [intro.md](intro.md) file to read a detailed blog post about this project.

---

⭐ If this project is useful, please give it a star! 