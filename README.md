# Orgmode-linked-by-id

## Installation

### For Doom Emacs (recommended)

1. **Add to `~/.config/doom/packages.el`:**
   ```elisp
   (package! orgmode-linked-by-id
     :recipe (:host github :repo "drringo/orgmode-linked-by-id"))
   ```
2. **Sync Doom and restart Emacs:**
   ```sh
   doom sync
   ```
   Then restart Emacs.
3. **Require the package in your `~/.config/doom/config.el`:**
   ```elisp
   (require 'orgmode-linked-by-id)
   ```
   This will enable the default keybindings:
   - `C-c l i` : Insert ID link in current file
   - `C-c l f` : Insert ID link in directory
   - `C-c l c` : Insert link to named image/table

4. **(Optional) Doom-style keybindings:**
   If you want to use Doom's `SPC l i` style keybindings, add this to your `config.el`:
   ```elisp
   (when (fboundp 'map!)
     (map! :leader
           (:prefix ("l" . "org link by id")
            :desc "Insert ID in file"   "i" #'drringo/org-get-headers-with-ids
            :desc "Insert ID in folder" "f" #'drringo/org-get-headers-with-ids-in-folder
            :desc "Insert entity link"  "c" #'drringo/helm-insert-org-image-link-with-custom-caption)))
   ```
   > **Note:** The package itself does NOT use `map!` internally, so it works in both Doom and vanilla Emacs.

### For vanilla Emacs (or other distributions)

1. **Clone the repo:**
   ```sh
   git clone https://github.com/drringo/orgmode-linked-by-id.git ~/.emacs.d/lisp/orgmode-linked-by-id
   ```
2. **Add to your `init.el` or `.emacs`:**
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/orgmode-linked-by-id")
   (require 'orgmode-linked-by-id)
   ```
3. **Use the default keybindings:**
   - `C-c l i` : Insert ID link in current file
   - `C-c l f` : Insert ID link in directory
   - `C-c l c` : Insert link to named image/table

## Why Use IDs?
- **Avoid issues when header content changes**
- **No need to think about CUSTOM_ID**
- **IDs are automatically generated using `org-id-get-create`**

## Features & Concept
- **Link headers in current file**: Find all headers with IDs in the currently open file, select from a list, and insert an ID link at the cursor position.
- **Link headers in directory**: Find headers with IDs in all .org files in a directory (excluding subdirectories), use Helm for fuzzy search, and select interactively.
- **Link to named images/tables**: Find objects with `#+NAME` and `#+CAPTION`, display a caption list for user selection, and allow customization of the link description.

## Comparison with Traditional Method
| Traditional Method | This Extension |
|-------------------|----------------|
| `[[*Header name][Header name]]` | `[[id:abc123][Header name]]` |
| Breaks when header changes | Still works |
| Difficult to search | Easy to search |

---

## Notes
- **Do not use `map!` or Doom-specific macros in the package file itself.** Only use them in your personal config if you use Doom Emacs.
- The package is compatible with both Doom Emacs and vanilla Emacs.
- All default keybindings (`C-c l i`, `C-c l f`, `C-c l c`) are always available via `global-set-key`.
- If you want Doom-style keybindings (`SPC l i`...), add them in your `config.el` as shown above.
- If you see errors about `:prefix` or `map!`, make sure you have removed all `map!` lines from the package file and only use them in your Doom config.