typst-preview.el
==========

*Warning*: This is work in progress and quite rough around the edges, but the basic functionality is there.

Live preview of typst files inside emacs! Building on
[typst-preview](https://github.com/Enter-tainer/typst-preview), which
was originally written for VS Code, and also inspired by
[typst-preview.nvim](https://github.com/chomosuke/typst-preview.nvim).

**Features**:

-   Live preview of edits
-   Source to preview jumping
-   Preview to source jumping

https://github.com/havarddj/typst-preview.el/assets/96797844/e44426b2-b404-416e-bb1d-54242f8bb077

# Installation

## MELPA

Currently not on Melpa

## Manual installation

Install a `tinymist` binary from
<https://github.com/Myriad-Dreamin/tinymist> and make sure
it\'s in your \$PATH. To test this, create test.typ and run

```sh
tinymist preview test.typ
```

It is also possible to use the old `typst-preview`, found at <https://github.com/Enter-tainer/typst-preview/releases>. Run

``` sh
typst-preview test.typ
```
to test it. Since `typst-preview` has been deprecated in favour of `tinymist preview`, the latter is the new default. You can change back to `typst-preview` using 

``` el
(setq typst-preview-executable "typst-preview")
```

Then put `typst-preview.el` in your load-path, make sure `websocket` is installed, and put this in your init file:

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

Inside a .typ-file, run `M-x typst-preview-mode`. It will prompt you to set a master file, which by default is the file you are currently editing. This is useful if you have a file which links to other files using `#include`. Then it starts a preview in your default browser, and connects the source buffer to the server, sending live updates. 

Start, stop and restart `typst-preview` using `M-x typst-preview-start`,
`M-x typst-preview-stop` and `M-x typst-preview-restart`. Jumping from source to preview: `M-x typst-preview-send-position`. 

## Change default browser

To preview the .typ file using `xwidget`, provided your emacs is built
with `xwidget`-support:

```el
(setq typst-preview-browser "xwidget") ;; default is "default"
```

To preview the .typ file in a non-default browser, you can set `typst-preview-browser` to a browser of your choice, and `typst-preview` will try to find the browser using `(executable-find typst-preview-browser)`. This does not work on macOS, but might perhaps work on linux. You can alternatively add a hook to `typst-preview-mode-hook` to set `browse-url-generic-program` for typst buffers specifically. 


## Details:

Enabling `typst-preview-mode` runs `typst-preview-start`, which does a
few things:

-   Asks for a master file, and if there is a preview process with same master, attaches the current file to that process. Otherwise it will: 
    -   Start `typst-preview` on the current file, sending results to the
        buffer `*ws-typst-server*`.
    -   Connect to the `typst-preview` server using `websocket`.
-   Then it opens a browser pointing at the address of the preview, and 
-   Adds a hook to `after-change-functions` which sends the buffer to
    the server at each keystroke.

## Advanced configuration

Here is a sample configuration using `use-package` which includes `typst-ts-mode` and `typst-lsp`. 

``` el
(use-package websocket)
(use-package typst-preview
  :load-path "directory-of-typst-preview.el"
  :config
  (setq typst-preview-browser "default")
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position)
  )

(use-package typst-ts-mode
  :load-path "directory-of-typst-ts-mode.el"
  :custom
  (typst-ts-mode-watch-options "--open")
  :config
  ;; make sure to install typst-lsp from
  ;; https://github.com/nvarner/typst-lsp/releases
  ;; or use tinymist
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "typst-lsp")
    :major-modes '(typst-ts-mode)
    :server-id 'typst-lsp))
  )
```

## FAQ: 

### How do I configure the root directory of my project?

Customize the `typst-preview-default-dir` variable; this can be done file-locally, see [main.typ](test-subfolders/main_folder/main.typ) for an example.

### Can I run typst-preview in an emacs buffer not attached to a file?

At the moment, no. But see [#13](https://github.com/havarddj/typst-preview.el/issues/13).



# License:
This project is licensed under the GPL License - see the LICENSE.md file for details


# Todos:
If you want to get involved, feel free to fork this repository and look into one of the following:

-   [ ] Implement better error handling
-   [ ] Get rid of annoying 'No active typst-preview'-message (find better solution)
-   [ ] Add example using :vc in use-package
-   [ ] Add outline functionality (NB: this might come for free from tinymist?)
-   [ ] Ensure that slides work properly
-   [ ] Fix \"revert buffer makes typst restart\" - should be enough look for existing instance. Does reverting reset buffer-local variables?
-   [ ] Sync memory files on save
-   [ ] Solve some out of bounds problem? (jump to source on certain symbols)
-   [ ] Refresh local variables of connected files when running clear-active-files
