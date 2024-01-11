// *spacing, headings & eq numbering*
#set heading(numbering: "1.1.1")
#set math.equation(numbering: "(1.1)")
#show heading: set block(above: 1.4em, below: 1em)

#show par: set block(spacing: 1.1em)
// *font*
#set text(
  // font: "New Computer Modern",
  size: 11pt
)

// #show heading.where(level:1): it => [
//   #set align(center)
//   #set text(12pt, weight: "regular")
//     #counter(heading).display() #smallcaps(it.body)
// ]
// #show heading.where(level:2): it => [
//   #set align(center)
//   #set text(11pt, weight: "regular")
//     #counter(heading).display() #smallcaps(it.body)
// ]


// *bibliography*
// #set cite(style: "chicago-author-date")

#show link: set text(fill: rgb(150, 50, 100))


// *commutative diagrams*
#import "@preview/commute:0.2.0": node, arr, commutative-diagram

// *theorem environments*
#import "@preview/ctheorems:1.1.0": *
#show: thmrules
// #let mythm(name, Name) = thmbox(name, Name, inset: (x: 0em, top: 0em),
//  breakable:true, base_level: 1.1heorem")
// heorem = mythm("theorem", "Theorem")
// #let lemma = mythm("lemma", "Lemma")
// #let proposition = mythm("proposition", "Proposition")
// #let definition = mythm("definition", "Definition")
// #let example = mythm("example", "Example")
#let theorem = thmbox("theorem", "Theorem", inset: (x: 0em, top: 0em), base_level: 1)
#let lemma = thmbox("theorem", "Lemma", inset: (x: 0em, top: 0em), base_level: 1)
#let proposition = thmbox("theorem", "Proposition", inset: (x: 0em, top: 0em), base_level: 1)
#let definition = thmbox("theorem", "Definition", inset: (x: 0em, top: 0em), base_level: 1)
#let example = thmbox("theorem", "Example", inset: (x: 0em, top: 0em), base_level: 1)


#let proof = thmplain(
    "proof",
    "Proof",
    base: "theorem",
    inset: (x: 0em, top: 0em),
    bodyfmt: body => [#body #h(1fr) $square$]
).with(numbering: none)

// *title stuff*

#let title = [Typst-preview.el: live preview of typst documents]
#align(center, text(17pt)[
    *#title*
])

Live preview of typst files inside emacs! Building on #link("https://github.com/Enter-tainer/typst-preview")[typst-preview], which was originally written for VS Code, and also inspired by #link("https://github.com/chomosuke/typst-preview.nvim")[typst-preview.nvim].

#align(center)[*Features*:
- Live preview of edits
- Source to preview jumping
- Preview to source jumping]

= Installation

== MELPA
Currently not on Melpa

== Manual installation

Install the typst-preview binary from
#link("https://github.com/Enter-tainer/typst-preview/releases")[typst-preview]
and make sure it's in your `$PATH`. To test this, create test.typ and run
```bash 
typst-preview test.typ
```				// 

Then put `typst-preview.el` in your load-path, and put this in your init file:
```lisp
 (require 'typst-preview)
```
Or, if you use `use-package`, try:

```lisp
(use-package websocket)
(use-package typst-preview
  :straight nil 			; if you use straight
  :load-path "path-to-typst-preview.el")
``` 
= Usage & configuration

== Basic usage
Inside a .typ-file, run `M-x typst-preview-mode`. This will start a preview in your default browser, and connect the source buffer to the server, sending live updates.

Start, stop and restart ~typst-preview~ using ~M-x typst-preview-start~, ~M-x typst-preview-stop~ and ~M-x typst-preview-restart~. 

Jumping from source to preview: `M-x typst-preview-send-position`. 
This only works in text (i.e. not in a code block, say in math mode) because of how the typst compiler works. See #link("https://github.com/Enter-tainer/typst-preview/issues/182") 

== Change default browser

To preview the .typ file in a non-default browser: 
```lisp
(setq typst-preview-browser "safari")
```

To preview the .typ file using `xwidget`, provided your emacs is built with `xwidget`-support: 

```lisp
(setq typst-preview-browser "xwidget")
```
== Details:
Enabling `typst-preview-mode` runs `typst-preview-start`, which does a few things:
- Starts `typst-preview` on the current file, sending results to the buffer `*ws-typst-server*`.
- Connects to the `typst-preview` server using `websocket`
- Opens a browser pointing at the address of the preview
- Adds a hook to `after-change-functions` which sends the buffer to the server at each keystroke. 

== Todos: 
- [ ] Open browsers in linux/windows, not just MacOS
- [ ] Ensure that opening several .typ instances works
- [ ] Clean up typst-preview-start and fix the xwidget hack
- [ ] Add license




