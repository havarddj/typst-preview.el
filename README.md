[![MELPA](https://melpa.org/packages/typst-preview-badge.svg)](https://melpa.org/#/typst-preview)

typst-preview.el
==========

Live preview of typst files inside emacs! Building on
[tinymist](https://github.com/Myriad-Dreamin/tinymist), which
was originally written for VS Code, and also inspired by
[typst-preview.nvim](https://github.com/chomosuke/typst-preview.nvim).

**Features**:

-   Live preview of edits
-   Source to preview jumping
-   Preview to source jumping

https://github.com/havarddj/typst-preview.el/assets/96797844/e44426b2-b404-416e-bb1d-54242f8bb077

# Installation

## MELPA

Can be installed from Melpa in [the usual way](https://melpa.org/#/getting-started).

Install a `tinymist` binary from <https://github.com/Myriad-Dreamin/tinymist> and make sure
it\'s in your \$PATH. To test this, create test.typ and run

```sh
tinymist preview test.typ
```
in the same directory. If you want to customize which `tinymist` binary to use, configure `typst-preview-executable` - see more on customization below.

## Manual installation
Install `tinymist` as described above. Then put `typst-preview.el` in your load-path, make sure `websocket` is installed, and put this in your init file:

```el
(require 'typst-preview)
```

Or, if you use `use-package`, try:

```el
(use-package websocket)
(use-package typst-preview
  :load-path "path-to-typst-preview.el")
```

If you use `doom`, try:

``` el
(package! typst-preview
  :recipe (:host github :repo "havarddj/typst-preview.el"))
```

# Usage and configuration

## Basic usage

Inside a .typ file, run `M-x typst-preview-mode`. It will prompt you to set a master file, which by default is the file you are currently editing. This is useful if you have a file which links to other files using `#include`. Then it starts a preview in your default browser, and connects the source buffer to the server, sending live updates. 

Start, stop and restart `typst-preview` using `M-x typst-preview-start`,
`M-x typst-preview-stop` and `M-x typst-preview-restart`. Jumping from source to preview: `M-x typst-preview-send-position`. 

## Change default browser

To preview the .typ file using `xwidget`, provided your emacs is built
with `xwidget`-support:

```el
(setq typst-preview-browser "xwidget") ;; default is "default"
```

To preview the .typ file in a non-default browser, you can set `typst-preview-browser` to a browser of your choice, and `typst-preview` will try to find the browser using `(executable-find typst-preview-browser)`. This does not work on macOS, but might perhaps work on linux. You can alternatively add a hook to `typst-preview-mode-hook` to set `browse-url-generic-program` for typst buffers specifically. 

## Previewing slides

The `tinymist` preview program supports a slide mode. To enable it in buffer, run `M-: (setq-local typst-preview-preview-mode "slide")`, and then start the preview. Note that if you have an existing `typst-preview` process running, you might have to stop it with `typst-preview-stop` first (and run `M-x revert-buffer` for good measure). 


## Details:

Enabling `typst-preview-mode` runs `typst-preview-start`, which does a
few things:

-   Asks for a master file, and if there is a preview process with same master, attaches the current file to that process. Otherwise it will: 
    -   Start `tinymist` on the current file, sending results to the
        buffer `*ws-typst-server*`.
    -   Connect to the `tinymist` server using `websocket`.
-   Then it opens a browser pointing at the address of the preview, and 
-   Adds a hook to `after-change-functions` which sends the buffer to
    the server at each keystroke.

## Advanced configuration

Here is a sample configuration using `use-package`.

``` el
(use-package websocket)

(use-package typst-preview
  ;; :load-path "path/to/typst-preview.el" ;; if installed manually
  :init
  (setq typst-preview-autostart t) ; start preview automatically when typst-preview-mode is activated
  (setq typst-preview-open-browser-automatically t) ; open browser automatically when typst-preview-start is run

  :custom
  (typst-preview-browser "default") 	; this is the default option; other options are `eaf-browser' or `xwidget'.
  (typst-preview-invert-colors "auto")	; invert colors depending on system theme
  (typst-preview-executable "tinymist") ; path to tinymist binary (relative or absolute)
  (typst-preview-partial-rendering t)   ; enable partial rendering 
  
  :config
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position))

```

## FAQ: 

### How do I configure the root directory of my project?

Customize the `typst-preview-default-dir` variable; this can be done file-locally, see [main.typ](test/test-subfolders/main_folder/main.typ) for an example.

### Can I run typst-preview in an emacs buffer not attached to a file?

At the moment, no. But see [#13](https://github.com/havarddj/typst-preview.el/issues/13).

### How can I set the typst-preview browser to something else than my system default browser?

Typst-preview.el uses `browse-url` under the hood, so you can customize `browse-url-browser-function` as described [in this blog post](https://assortedarray.com/posts/browse-url-sites-custom/). Previously this was done inside the typst-preview.el code, but was changed for v1.0.0beta in order to simplify the code for submission to Melpa.



# License:
This project is licensed under the GPL License - see the LICENSE.md file for details


# Todos:
If you want to get involved, here are some suggestions for potential improvements:

-   [ ] Create an option to let the tinymist language server handle previews
-   [ ] Implement better error message handling
-   [ ] Implement better error handling in general
-   [ ] Add outline functionality (NB: this might come for free from tinymist?)
-   [ ] Sync memory files on save
-   [ ] Solve some out of bounds problem? (jump to source on certain symbols)
-   [ ] Refresh local variables of connected files when running clear-active-files
