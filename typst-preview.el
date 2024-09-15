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
;; or https://github.com/Myriad-Dreamin/tinymist

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

(defvar typst-preview-host "127.0.0.1:0"
  "Default address for typst websocket.")

(defvar typst-preview-invert-colors "auto"
  "Invert colors in preview. Options: 'never', 'always', 'auto'."
  )

(defvar typst-preview-executable "tinymist preview" 
  "Program for running typst preview. Options: 'typst-preview', 'tinymist preview'."
  )

(defvar tp--active-buffers '()
  "Active typst buffers")
(defvar typst-preview-center-src t
  "If non-NIL, center typst preview source buffer when jumping to source")

(cl-defstruct tp--master
  path process children socket static-host control-host)

(defvar tp--active-masters '()
  "list of active typst-preview masters")

(defvar typst-preview-ask-if-pin-main t
  "if non-NIL, ask if main file should be saved as commented file variable")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

(defvar typst-preview-mode-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "typst-preview-mode map"))
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
  :global nil

  (if typst-preview-mode
      (progn
	(message "Typst-preview enabled!")
	(typst-preview-start)
	)
    (remove-hook 'after-change-functions #'tp--send-buffer-on-type t))
  :key-map typst-preview-mode-map
  )


;; this is probably not the right way to do it but eh
(make-variable-buffer-local 'tp--file)
(make-variable-buffer-local 'tp--master-file)
;; stop emacs from complaining about unsafe buffer local variables
(put 'tp--master-file 'safe-local-variable #'stringp)
(make-variable-buffer-local 'tp--local-master)
(make-variable-buffer-local 'tp--file-path)
(make-variable-buffer-local 'tp--control-socket)
(make-variable-buffer-local 'tp--control-host)
(make-variable-buffer-local 'tp--ws-buffer)
(make-variable-buffer-local 'tp--process)

(defun typst-preview-start ()
  "Start typst-preview server and connect current buffer."
  
  (interactive)
  (if (typst-preview-connected-p)
      (message "Typst-preview already connected")
    ;; these are buffer-local
    (setq tp--file (buffer-name))
    (setq tp--file-path (buffer-file-name))

    (setq tp--ws-buffer (get-buffer-create "*ws-typst-server*"))

    (unless tp--master-file
     (setq tp--master-file
	  (expand-file-name
	   (read-file-name (format "Master file (default: %s): " tp--file) nil tp--file))
	  )
     (if (and typst-preview-ask-if-pin-main
	      (y-or-n-p "Save master file as local variable?"))
	 (add-file-local-variable 'tp--master-file tp--master-file)
     )
    )
    
    (setq tp--preview-dir (file-name-directory tp--master-file))

    (cl-loop for master in tp--active-masters
	     if (string-equal tp--master-file (tp--master-path master))
	     do
	       (message "Found active master: %S" master)
	       (push tp--file-path (tp--master-children master))
	       (setq tp--local-master master)
	       (setq tp--process (tp--master-process master))
	       (setq tp--control-socket (tp--master-socket master))
	       (setq tp--control-host (tp--master-control-host master))
	       (setq tp--static-host (tp--master-static-host master))
	       (message "Set local master: %s" tp--local-master)
	       
	     )
	     
    (unless tp--local-master
      (if (eq typst-preview-executable "tinymist preview")
	  (setq tp--process (start-process "typst-preview-proc" tp--ws-buffer
					   "tinymist" "preview" "--partial-rendering" "--no-open"
					   "--host" typst-preview-host
					   "--control-plane-host"  "127.0.0.1:0"
					   "--data-plane-host"  "127.0.0.1:0"
					   "--root" tp--preview-dir
					   "--invert-colors" typst-preview-invert-colors
					   tp--master-file))
	(setq tp--process (start-process "typst-preview-proc" tp--ws-buffer
					 "typst-preview" "--partial-rendering" "--no-open"
					   "--host" typst-preview-host
					   "--control-plane-host"  "127.0.0.1:0"
					   "--data-plane-host"  "127.0.0.1:0"
					   "--root" tp--preview-dir
					   "--invert-colors" typst-preview-invert-colors
					   tp--master-file)
	      )
	)     

      (message "started tp--process")

      (sleep-for 1) ;; just for testing!

      (message "opening websocket")
      ;; find typst-preview tp--control-host address from tp--ws-buffer

      (setq tp--control-host (tp--find-server-address "Control plane"))
      (setq tp--static-host (tp--find-server-address "Static file"))
      ;; (setq data-host (tp--find-server-address "Data plane"))
      (message "Control host: %S \n Static host: %S" tp--control-host tp--static-host)
      ;; connect to typst-preview socket
      (setq tp--control-socket
	    (websocket-open (concat "ws://" tp--control-host)
			    :on-message (lambda (_websocket frame) (tp--parse-message _websocket frame))
			    ;; :on-close (lambda (_websocket) (message "websocket closed")
			    ;; 		(kill-buffer tp--ws-buffer)
			    ;; 		)
			    )
	    )
      
      
      (message "Master not defined, setting new master with path %S" tp--master-file)
      

      (setq tp--local-master
	    (make-tp--master
	     :path tp--master-file
	     :process tp--process
	     :children (list tp--file-path)
	     :socket tp--control-socket
	     :static-host tp--static-host
	     :control-host tp--control-host
	     )
	    )
      (push tp--local-master tp--active-masters)
      )

    


    ;; (typst-preview-sync-memory)
    ;; add hook to update on change
    ;; (sleep-for 2)

    (message "Current buffer: %S" (current-buffer))
    (message "typst-preview-buffer is %S" tp--file)
    (with-current-buffer tp--file
      (add-hook 'after-change-functions
		#'tp--send-buffer-on-type nil t)
      )

    ;; open typst-browser

    (tp--connect-browser typst-preview-browser tp--static-host)
    (push `(,tp--file-path) tp--active-buffers)
    )
  )

(defun typst-preview-connected-p ()
  "Returns t if websocket is connected to typst-preview server"
  (and
   (boundp 'tp--control-socket)
   (websocket-openp tp--control-socket)
   (process-live-p tp--process)
   (member tp--file-path (tp--master-children tp--local-master))))

(defun typst-preview-stop ()
  "Stop typst-preview buffer by killing websocket connection"
  (interactive)
  (if (not (typst-preview-connected-p))
      (message "No active typst preview in this buffer")

    (if (not (eq (length (tp--master-children tp--local-master)) 1))
	(setf (tp--master-children tp--local-master)
	      (delete tp--file-path (tp--master-children tp--local-master)))
      (delete-process tp--process)
      (delete (list tp--file-path) tp--active-buffers)
      (setf tp--active-masters (delete tp--local-master tp--active-masters))
      (if (eq '() tp--active-masters)
	  (kill-buffer tp--ws-buffer)
	)
      ))
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
  (let ((msg (json-encode `(("event" . "panelScrollTo")
			    ("filepath" . ,tp--file-path)
			    ("line" . ,(1- (line-number-at-pos)))
			    ("character" . ,(max 1 (current-column))))
			  )))
    (websocket-send-text tp--control-socket msg)
    (message "Sending position to typst-preview server")
    ))

(defun typst-preview-sync-memory ()
  "Sync typst-preview memory"
  (interactive)
(let ((msg (json-encode `(("event" . "syncMemoryFiles")
			("files"  (,tp--file-path
				   . ,(tp--stringify-buffer)
				   ))))))
  (websocket-send-text tp--control-socket msg)
  (message "syncing memory files")
  ))


(defun typst-preview-open-browser ()
  "Open typst-preview browser interactively"
  (interactive)
  (let* ((browser-list '("default" "xwidget" "safari" "google chrome" "eaf-browser"))
	 (browser (completing-read "Browser:" browser-list nil nil)))
    (tp--connect-browser browser tp--static-host)
    )
  )

(defun typst-preview-list-active-files ()
  "List active typst files"
  (interactive)
  (let ((str ""))
    (cl-loop for master in tp--active-masters
	     do
	     (setq str (concat str (format "*master:* %s\n"
					   (tp--master-path master))))
	     (cl-loop for child in (tp--master-children master)
		      do
		      (setq str (concat str (format "|--> %s\n" child))
			       )))
    (message str)
    ))

(defun typst-preview-clear-active-files ()
  "Clear all active typst files"
  (interactive)
  (setq tp--active-masters '())
    )

;;;; Functions

;;;;; Public
;;;;; Private

;; todo: implement this
;; (defun is-valid-browser (browser)
;;   "Test if argument browser is a valid browser "
;;   )

(defun tp--parse-message (sock frame)
  "React to message received from typst-preview server."
  (let* ((msg (json-parse-string (websocket-frame-text frame)))
	 (event (gethash "event" msg))
	 )
    ;; (message "typst preview file: %s" tp--file)
    (pcase event
      ("editorScrollTo"
       (let ((file-name (gethash "filepath" msg)))
	 (tp--goto-file-position file-name (gethash "start" msg))
	 ))
      ("compileStatus"
       (if (string= "CompileError" (gethash "kind" msg))
	   (message "Compile error")
	 ))
      ("syncEditorChanges"
       (typst-preview-sync-memory)
       )
      )))


(defun tp--connect-browser (browser hostname)
  "Open browser at websocket URL hostname."
  (let ((full-url (concat "http://" hostname)))
    (pcase browser
      ("xwidget" (xwidget-webkit-browse-url full-url))
      ("default" (browse-url full-url))
      ("eaf-browser" (eaf-open-browser-other-window full-url))
      (_
       (let* ((browse-url-generic-program (executable-find browser))
	      (browse-url-browser-function 'browse-url-generic))
	 (browse-url (concat "http://" hostname)))
       )
      )
    )
  )


(defun tp--find-server-address (str)
  "Find server address for typst-preview:
str should be either \"tp--control-host\" or \"tp--static-host\"."
  (interactive)
  (with-current-buffer tp--ws-buffer
    (save-excursion
      (end-of-buffer)
      (search-backward (concat str " server listening on:") nil)
      (car (last (split-string (thing-at-point 'line 'no-properties))))
      )))

(defun tp--get-buffer-position ()
  "Get position in buffer as a vector"
       (interactive)
       (vector (line-number-at-pos) (current-column)))
			  

(defun tp--goto-file-position (file-name vec)
  "Go to a specific position vec = (line, column) in the specified file."
  (message "Going to %s in %s" vec file-name)
  (pop-to-buffer (find-file file-name))
  
    (goto-char (point-min))
    (forward-line (aref vec 0))
    (forward-char (aref vec 1))
    (if typst-preview-center-src
	(recenter-top-bottom)))

(defun tp--stringify-buffer ()
  "return contents of current buffer as string"
  (save-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun tp--send-buffer ()
  "Send buffer to typst-preview server."
  (let ((content
	 (json-encode `(("event" . "updateMemoryFiles")
			("files"  (,tp--file-path
				   . ,(tp--stringify-buffer)
				   ))))))    ;; (message "Content from tp--send-buffer: %s" content)    
    (websocket-send-text tp--control-socket content)
    ))

;; according to SO, https://stackoverflow.com/questions/31079099/emacs-after-change-functions-are-not-executed-after-buffer-modification
;; need to wrap this, and provide input variables
(defun tp--send-buffer-on-type (begin end length)
  (let ((inhibit-modification-hooks nil))
    (tp--send-buffer))
  )


;;;; Footer

(provide 'typst-preview)
;;; typst-preview.el ends here
