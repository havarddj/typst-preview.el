;;; typst-preview.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: nil; -*-

;; Copyright (C) 2023 Håvard Damm-Johnsen

;; Author: Håvard Damm-Johnsen <havard-dj@hotmail.com>
;; URL:
;; Version: 0.1-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: typst, preview

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements live typst preview using
;; https://github.com/Enter-tainer/typst-preview

;;;; Installation

;;;;; MELPA

;; Currently not on Melpa

;;;;; Manual

;; Install these required packages:

;; Install the typst-preview binary from
;; https://github.com/Enter-tainer/typst-preview/releases
;; and make sure it's in your $PATH. To test this, create test.typ and run
;; typst-preview test.typ
;; 

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'typst-preview)

;;;; Usage

;; Run one of these commands:


;;;; Tips

;; + You can customize settings in the `typst-preview' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: foo[1], which showed me how to bifurcate, and bar[2],
;; which takes care of flanges.
;;
;;  [1] https://example.com/foo.el
;;  [2] https://example.com/bar.el

;;; Code:

;;;; Requirements

(require 'websocket)
(require 'json)

;;;; Customization

;; (defgroup typst-preview nil
;;   "Settings for `typst-preview'."
;;   :link '(url-link "https://example.com/typst-preview.el"))

;; (defcustom typst-preview-something nil
;;   "This setting does something."
;;   :type 'something)

;;;; Variables

(defvar typst-preview-browser "default"
  "Default browser for previewing Typst files.
 Options: \"xwidget\",\"safari\", or \"default\".")

(defvar typst-preview-host "127.0.0.1:23627"
  "Default address for typst websocket.")


;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

(defvar typst-preview-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "typst-preview map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "C-c C-j" #'typst-preview-send-position
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;; Commands

;;;###autoload

(define-minor-mode typst-preview-mode
  "Toggle typst-preview minor mode"
  nil
  :global t

  (if typst-preview-mode
      (progn
	(message "Typst-preview enabled!")
	(typst-preview-start)

	)
    (remove-hook 'after-change-functions #'typst-send-buffer))
  )

(defun typst-preview-start ()
  (interactive)
  "Start typst-preview server and connect current buffer."

  (setq typst-preview-file (buffer-name))
  (setq typst-preview-path (buffer-file-name))

  (setq typst-ws-buffer (get-buffer-create "*ws-typst-server*"))

  (start-process "typst-preview-proc" typst-ws-buffer
               "typst-preview" "--partial-rendering" "--no-open"
	       typst-preview-file)
  
  (sleep-for 1) ;; just for testing!


  ;; find typst-preview control-host address from typst-ws-buffer
  (setq control-host (find-server-address "Control plane"))
  ;; (setq static-host (find-server-address "Static file"))

  
  ;; connect to typst-preview socket
  (setq control-socket
	(websocket-open (concat "ws://" control-host)
			:on-message (lambda (_websocket frame) (parse-message _websocket frame))
			:on-close (lambda (_websocket) (message "websocket closed")
				    (kill-buffer typst-ws-buffer)
				    )))


  ;; add hook to update on change
  ;; (sleep-for 2)

  (message "Current buffer: %S" (current-buffer))
  (message "typst-preview-buffer is %S" typst-preview-file )
  (with-current-buffer typst-preview-file
	(add-hook 'after-change-functions
      #'typst-send-buffer-on-type nil t)
    )

  ;; open typst-browser

  (typst-connect-browser typst-preview-browser typst-preview-host)
  )

(defun typst-preview-connected-p ()
  "Returns t if websocket is connected to typst-preview server"
  (interactive)
  (websocket-openp control-socket))

(defun typst-preview-stop ()
  "Stop typst-preview buffer by killing websocket connection"
  (interactive)
  (kill-buffer typst-ws-buffer)
  )
(defun typst-preview-restart ()
  "Restart typst-preview server"
  (interactive)
  (typst-preview-stop)
  (sleep-for 1)
  (typst-preview-start)
  )

(defun typst-preview-send-position ()
  "send point in buffer to preview server"
  (interactive)
  (let ((msg (json-encode `(("event". "panelScrollTo")
			    ("filepath" . ,typst-preview-path)
			    ("line" . ,(line-number-at-pos))
			    ("character" . ,(current-column)))
			  )))
    (websocket-send-text control-socket msg)
    ))

(defun typst-preview-sync-memory ()
  "Sync typst-preview memory"
  (interactive)
(let ((msg (json-encode `(("event" . "syncMemoryFiles")
			("files"  (,typst-preview-path
				   . ,(buffer-whole-string)
				   ))))))
  (websocket-send-text control-socket msg)
  ))


;;;; Functions

;;;;; Public

;; this is probably not the right way to do it but eh
(make-variable-buffer-local 'typst-preview-file)
(make-variable-buffer-local 'typst-preview-path)
(make-variable-buffer-local 'control-socket)
(make-variable-buffer-local 'control-host)
(make-variable-buffer-local 'typst-ws-buffer)
;;;;; Private

(defun parse-message (sock frame)
  "React to message received from typst-preview server."
  (let* ((msg (json-parse-string (websocket-frame-text frame)))
	 (event (gethash "event" msg))
	 )
    ;; (message "typst-preview-file: %s" typst-preview-file)
    (pcase event
      ("editorScrollTo"
       (let ((buffer (car (last (split-string (gethash "filepath" msg) "/")))))
	 (goto-position-in-buffer buffer (gethash "start" msg))))
      ("compileStatus"
       (if (string= "CompileError" (gethash "kind" msg))
	   (message "Compile error")
				)))))


(defun typst-connect-browser (browser hostname)
  "Open browser at websocket URL hostname."
  (pcase browser
    ("safari" (shell-command (concat "open -a Safari http://" hostname)))
    ("xwidget" (xwidget-webkit-browse-url (concat "http://" hostname)))
    ("default" (shell-command (concat "open http://" hostname)))
    (_ (shell-command (concat "open -a " browser " ws://" hostname)))
    )
  )

(defun find-server-address (str)
  "Find server address for typst-preview:
str should be either \"control-host\" or \"static-host\"."
  (interactive)
  (with-current-buffer typst-ws-buffer
    (save-excursion
      (beginning-of-buffer)
      (search-forward (concat str " server listening on:") nil)
      (car (last (split-string (thing-at-point 'line 'no-properties))))
      )))

(defun get-position-in-buffer ()
  "Get position in buffer as a vector"
       (interactive)
       (vector (line-number-at-pos) (current-column)))
			  

(defun goto-position-in-buffer (buffer-name vec)
  "Go to a specific position vec = (line, column) in the specified buffer."
  (message "Going to %s in %s" vec buffer-name)
  (pop-to-buffer buffer-name)
    (goto-char (point-min))
    (forward-line (aref vec 0))
    (forward-char (aref vec 1)))

(defun buffer-whole-string ()
  "return contents of current buffer as string"
  (save-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun typst-send-buffer ()
  "Send buffer to typst-preview server."
  (let ((content
	 (json-encode `(("event" . "updateMemoryFiles")
			("files"  (,typst-preview-path
				   . ,(buffer-whole-string)
				   ))))))    ;; (message "Content from typst-send-buffer: %s" content)
    
    (websocket-send-text control-socket content)
    ))

;; according to SO, https://stackoverflow.com/questions/31079099/emacs-after-change-functions-are-not-executed-after-buffer-modification
;; need to wrap this, and provide input variables
(defun typst-send-buffer-on-type (begin end length)
  (let ((inhibit-modification-hooks nil))
    (typst-send-buffer))
  )


;;;; Footer

(provide 'typst-preview)
;;; typst-preview.el ends here
