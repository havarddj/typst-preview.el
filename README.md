typst-preview.el
==========

*Warning*: This is work in progress, and is currently quite
janky. Shutdown and startup is not very graceful, and it doesn\'t know
how to work with multiple documents simultaneously. But the basic
functionality is there.

Live preview of typst files inside emacs! Building on
[typst-preview](https://github.com/Enter-tainer/typst-preview), which
was originally written for VS Code, and also inspired by
[typst-preview.nvim](https://github.com/chomosuke/typst-preview.nvim).

**Features**:

-   Live preview of edits
-   Source to preview jumping
-   Preview to source jumping

# Installation

## MELPA

Currently not on Melpa

## Manual installation

Install the typst-preview binary from
<https://github.com/Enter-tainer/typst-preview/releases> and make sure
it\'s in your \$PATH. To test this, create test.typ and run

```sh
typst-preview test.typ
```

Then put `typst-preview.el` in your load-path, and put this in your init
file:

```el
(require 'typst-preview)
```

Or, if you use `use-package`, try:

```el
(use-package websocket)
(use-package typst-preview
  :straight nil             ; if you use straight
  :load-path "path-to-typst-preview.el")
```


========
# Usage and installation

## Basic usage

Inside a .typ-file, run `M-x typst-preview-mode`. This will start a
preview in your default browser, and connect the source buffer to the
server, sending live updates.

Start, stop and restart `typst-preview` using `M-x typst-preview-start`,
`M-x typst-preview-stop` and `M-x typst-preview-restart`.

Jumping from source to preview: `M-x typst-preview-send-position`. This
only works in text (i.e. not in a code block, say in math mode) because
of how the typst compiler works. See
<https://github.com/Enter-tainer/typst-preview/issues/182.jk>

## Change default browser

To preview the .typ file in a non-default browser:

```el
(setq typst-preview-browser "safari")
```

To preview the .typ file using `xwidget`, provided your emacs is built
with `xwidget`-support:

```el
(setq typst-preview-browser "xwidget")
```

## Details:

Enabling `typst-preview-mode` runs `typst-preview-start`, which does a
few things:

-   Starts `typst-preview` on the current file, sending results to the
    buffer `*ws-typst-server*`.
-   Connects to the `typst-preview` server using `websocket`
-   Opens a browser pointing at the address of the preview
-   Adds a hook to `after-change-functions` which sends the buffer to
    the server at each keystroke.

# Todos:

-   [ ] Open browsers in linux/windows, not just MacOS
-   [x] Ensure that opening several .typ instances works
-   [ ] Clean up typst-preview-start and fix the xwidget hack
-   [ ] Add license
-   [ ] Fix \"revert buffer makes typst restart\" - should be enough to
    look for existing instance. Does reverting reset buffer-local
    variables?
-   [ ] Optionally centre buffer on preview-to-source
-   [x] Migrate to README.md
-   [ ] Add screencast
-   [ ] Add outline functionality
