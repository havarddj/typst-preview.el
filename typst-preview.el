;;; typst-preview.el --- Live preview of typst -*- lexical-binding: nil; -*-

;; Copyright (C) 2023 Håvard Damm-Johnsen

;; Author: Håvard Damm-Johnsen <havard-dj@hotmail.com>
;; URL: https://github.com/havarddj/typst-preview.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, languages, tools

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

;;; Code:

;;;; Requirements

(require 'websocket)
(require 'json)

;;;; Customization

(defgroup typst-preview nil
  "Settings for `typst-preview-mode'."
  :link '(url-link "https://github.com/havarddj/typst-preview.el"))

;;;; Variables

;; PUBLIC

(defcustom typst-preview-browser "default"
  "Default browser for previewing Typst files.
Options: \"xwidget\",\"safari\", or \"default\"."
  :type 'string
  :group 'typst-preview)

(defcustom typst-preview-invert-colors "auto"
  "Invert colors in preview. Options: 'never', 'always', 'auto'."
  :type 'string
  :group 'typst-preview)

(defcustom typst-preview-executable "tinymist preview"
  "Program for running typst preview. Options: 'typst-preview', 'tinymist preview'."
  :type 'string
  :group 'typst-preview)

(defcustom typst-preview-ask-if-pin-main t
  "If non-NIL, ask if main file should be saved as commented file variable."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-start-subfile-preview-automatically t
  "If non-NIL, start `typst-preview-mode' automatically in subfiles.
More precisely, when prompted from a previewed file to jump to a buffer
currently not running typst-preview, activate `typst-preview-mode' and
add buffer to children of the relevant master.
This is intended for multi-file projects where a file is included using e.g. #include."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-open-browser-automatically t
  "If non-NIL, open browser automatically when `typst-preview-start' is run."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-autostart t
  "If non-NIL, start typst preview automatically when `typst-preview-mode' is activated."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-center-src t
  "If non-NIL, center typst preview source buffer when jumping to source."
  :type 'boolean
  :group 'typst-preview)

(defvar typst-preview-host "127.0.0.1:0"
  "Default address for typst websocket.")

;; PRIVATE

(defvar tp--active-buffers '()
  "Get active typst buffers.")


(cl-defstruct tp--master
  path process children socket static-host control-host)

(defvar tp--active-masters '()
  "List of active typst-preview masters.")

;;;;; Keymaps

;; This technique makes it easier and less verbose to define keymaps
;; that have many bindings.

(defvar typst-preview-mode-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "typst-preview-mode map"))
        (maps (list
               ;; Mappings go here, e.g.:
               "C-c C-j" #'typst-preview-send-position)))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map))

;;;; Commands

;;;###autoload
(define-minor-mode typst-preview-mode
  "Toggle typst-preview minor mode."
  :init nil
  :lighter "typst-preview"
  :global nil

  (if typst-preview-mode
      (if typst-preview-autostart
	  (progn
	    (typst-preview-start)
	    (message "Typst preview started!"))
	(message "Typst-preview-mode enabled, run `typst-preview-start' to start preview"))
    (remove-hook 'after-change-functions #'tp--send-buffer-on-type t))
  :key-map typst-preview-mode-map )


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


;;;; Functions

;;;;; Public

;;;###autoload
(defun typst-preview-start (&optional open-browser)
  "Start typst-preview server and connect current buffer.

If `OPEN-BROWSER' set and non-NIL, then it will automatically open a default browser window."
  (interactive)

  (unless (boundp open-browser)
    (setq open-browser typst-preview-open-browser-automatically))

  (unless (executable-find (car (split-string typst-preview-executable)))
    (error "Typst-preview executable not found. Please install tinymist or
typst-preview, or modify `typst-preview-executable'"))
  
  (if (typst-preview-connected-p)
      (message "Typst-preview already connected")

    (setq tp--file (buffer-name))
    (setq tp--file-path (buffer-file-name))
    (setq tp--ws-buffer (get-buffer-create "*ws-typst-server*"))

    (unless tp--master-file
      (setq tp--master-file
	    (expand-file-name
	     (read-file-name (format "Master file (default: %s): " tp--file) nil tp--file)))
      (if (and typst-preview-ask-if-pin-main
	       (y-or-n-p "Save master file as local variable?"))
	  (add-file-local-variable 'tp--master-file tp--master-file)))
    
    (setq tp--preview-dir (file-name-directory tp--master-file))

    (cl-loop for master in tp--active-masters
	     if (string-equal tp--master-file (tp--master-path master))
	     do
	     (push tp--file-path (tp--master-children master))
	     (setq tp--local-master master)
	     (setq tp--process (tp--master-process master))
	     (setq tp--control-socket (tp--master-socket master))
	     (setq tp--control-host (tp--master-control-host master))
	     (setq tp--static-host (tp--master-static-host master)))
    
    (unless tp--local-master
      (if (string= typst-preview-executable "tinymist preview")
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
					 tp--master-file)))
      (message "Started %S process." typst-preview-executable)

      (sleep-for 1) ;; give time for websocket to populate tp--ws-buffer

      (message "Opening websocket.")
      ;; find typst-preview tp--control-host address from tp--ws-buffer

      (setq tp--control-host (tp--find-server-address "Control plane"))
      (setq tp--static-host (tp--find-server-address "Static file"))

      (message "Control host: %S \n Static host: %S" tp--control-host tp--static-host)

      
      ;; connect to typst-preview socket
      (setq tp--control-socket
	    (websocket-open (concat "ws://" tp--control-host)
			    :on-message (lambda (_websocket frame) (tp--parse-message _websocket frame))
			    :on-close (lambda (_websocket) (message "Typst-preview-mode websocket closed."))))
      
      (message "Master not defined, setting new master with path %S." tp--master-file)

      (setq tp--local-master
	    (make-tp--master
	     :path tp--master-file
	     :process tp--process
	     :children (list tp--file-path)
	     :socket tp--control-socket
	     :static-host tp--static-host
	     :control-host tp--control-host))
      (push tp--local-master tp--active-masters))

    (with-current-buffer tp--file
      (add-hook 'after-change-functions
		#'tp--send-buffer-on-type nil t))

    (if open-browser
	(tp--connect-browser typst-preview-browser tp--static-host)
      (message "Typst-preview started, navigate to %s in your browser or run `typst-preview-open-browser'." tp--static-host))
    
    (push `(,tp--file-path) tp--active-buffers)
    (add-hook 'kill-buffer-hook #'typst-preview-stop)))

;;;###autoload
(defun typst-preview-connected-p ()
  "Return t if websocket is connected to typst-preview server."
  (and
   (bound-and-true-p tp--local-master)
   (boundp 'tp--control-socket)
   (websocket-openp tp--control-socket)
   (process-live-p tp--process)
   (member tp--file-path (tp--master-children tp--local-master))))

;;;###autoload
(defun typst-preview-stop ()
  "Stop typst-preview buffer by killing websocket connection."
  (interactive)
  (if (not (typst-preview-connected-p))
      (message "No active typst preview in this buffer.")
    (let ((global-master (car (member tp--local-master tp--active-masters))))
      (if (not (string-equal tp--master-file tp--file-path))
	  (setf (tp--master-children global-master)
		(delete tp--file-path (tp--master-children global-master)))
	(delete-process tp--process)
	(delete (list tp--file-path) tp--active-buffers)
	(setf tp--active-masters (delete tp--local-master tp--active-masters))
	(if (eq '() tp--active-masters)
	    (kill-buffer tp--ws-buffer)))
      (kill-local-variable 'tp--local-master))))

;;;###autoload
(defun typst-preview-restart ()
  "Restart typst-preview server."
  (interactive)
  (typst-preview-stop)
  (sleep-for 1)
  (typst-preview-start))

;;;###autoload
(defun typst-preview-send-position ()
  "Send position in buffer to preview server."
  (interactive)
  (let ((msg (json-encode `(("event" . "panelScrollTo")
			    ("filepath" . ,tp--file-path)
			    ("line" . ,(1- (line-number-at-pos)))
			    ("character" . ,(max 1 (current-column)))))))
    (websocket-send-text tp--control-socket msg)
    (message "Sending position to typst-preview server")))

(defun typst-preview-sync-memory ()
  "Sync typst-preview memory."
  (interactive)
  (let ((msg (json-encode `(("event" . "syncMemoryFiles")
			    ("files"  (,tp--file-path
				       . ,(tp--stringify-buffer)))))))
    (websocket-send-text tp--control-socket msg)
    (message "syncing memory files")))


;;;###autoload
(defun typst-preview-open-browser ()
  "Open typst-preview browser interactively."
  (interactive)
  (let* ((browser-list '("default" "xwidget" "safari" "google chrome" "eaf-browser"))
	 (browser (completing-read "Browser:" browser-list nil nil)))
    (tp--connect-browser browser tp--static-host)))

;;;###autoload
(defun typst-preview-list-active-files ()
  "List active typst files."
  (interactive)
  (let ((str ""))
    (cl-loop for master in tp--active-masters
	     do
	     (setq str (concat str (format "*master:* %s\n"
					   (tp--master-path master))))
	     (cl-loop for child in (tp--master-children master)
		      do
		      (setq str (concat str (format "|--> %s\n" child)))))
    (if (string-equal str "")
	(message "No active typst-preview buffers.")
      (message str)))
  )

;;;###autoload
(defun typst-preview-clear-active-files ()
  "Clear all active typst files."
  (interactive)
  (setq tp--active-masters '()))

;;;;; Private

;; TODO: optionally display verbose error message on CompileError

(defun tp--parse-message (sock frame)
  "React to message `FRAME' received from typst-preview server `SOCK'."
  (let* ((msg (json-parse-string (websocket-frame-text frame)))
	 (event (gethash "event" msg)))
    ;; (message "typst preview file: %s" tp--file)
    (pcase event
      ("editorScrollTo"
       (tp--editor-scroll-to sock msg))
      ("compileStatus"
       (if (string= "CompileError" (gethash "kind" msg))
	   (message "Compile error")))
      ("syncEditorChanges"
       (typst-preview-sync-memory)))))

(defun tp--editor-scroll-to (sock msg)
  "Helper function for receving scroll message `MSG' from server `SOCK'."
  (let ((file-name (gethash "filepath" msg))
	(position (gethash "start" msg))
	(master
	 (car (cl-remove-if-not
	       (lambda (master)
		 (eq sock
		     (tp--master-socket master))) tp--active-masters))))
    (tp--goto-file-position file-name position)
    (when (and typst-preview-start-subfile-preview-automatically
	     (not (equal tp--local-master master)))
	  ;; (message "Prompted by websocket, starting typst-preview-mode in %s" file-name)
	  (setq tp--local-master master)
	  (setq tp--master-file (tp--master-path master))
	  (if (not (bound-and-true-p typst-preview-mode))
	      (typst-preview-mode))
	  (typst-preview-start nil))))

(defun tp--connect-browser (browser hostname)
  "Open browser `BROWSER' at websocket URL `HOSTNAME'."
  (let ((full-url (concat "http://" hostname)))
    (pcase browser
      ("xwidget" (xwidget-webkit-browse-url full-url))
      ("default" (browse-url full-url))
      ("eaf-browser" (eaf-open-browser-other-window full-url))
      (_
       (let* ((browse-url-generic-program (executable-find browser))
	      (browse-url-browser-function 'browse-url-generic))
	 (browse-url (concat "http://" hostname)))))))


(defun tp--find-server-address (str)
  "Find server address for typst-preview: `STR' should be either `tp--control-host' or `tp--static-host'."
  (interactive)
  (with-current-buffer tp--ws-buffer
    (save-excursion
      (end-of-buffer)
      (search-backward (concat str " server listening on:") nil)
      (car (last (split-string (thing-at-point 'line 'no-properties)))))))

(defun tp--get-buffer-position ()
  "Get position in buffer as a vector."
  (interactive)
  (vector (line-number-at-pos) (current-column)))

(defun tp--goto-file-position (file-name vec)
  "Go to a specific position `VEC' = (line, column) in file `FILE-NAME'."
  (message "Going to %s in %s" vec file-name)
  (pop-to-buffer (find-file file-name))
  
  (goto-char (point-min))
  (forward-line (aref vec 0))
  (forward-char (aref vec 1))
  (if typst-preview-center-src
      (recenter-top-bottom)))

(defun tp--stringify-buffer ()
  "Return contents of current buffer as string."
  (save-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun tp--send-buffer ()
  "Send buffer to typst-preview server."
  (let ((content
	 (json-encode `(("event" . "updateMemoryFiles")
			("files"  (,tp--file-path
				   . ,(tp--stringify-buffer)))))))
    (websocket-send-text tp--control-socket content)))

;; according to SO, https://stackoverflow.com/questions/31079099/emacs-after-change-functions-are-not-executed-after-buffer-modification
;; need to wrap this, and provide input variables
(defun tp--send-buffer-on-type (begin end length)
  "Helper function to send buffer to server on typing.

Needs to include variables `BEGIN', `END' and `LENGTH' to be compatible with `after-change-functions'."
  (let ((inhibit-modification-hooks nil))
    (tp--send-buffer)))

;;;; Footer

(provide 'typst-preview)
;;; typst-preview.el ends here
