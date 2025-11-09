;;; typst-preview.el --- Live preview of typst -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Håvard Damm-Johnsen

;; Author: Håvard Damm-Johnsen <havard-dj@proton.me>
;; URL: https://github.com/havarddj/typst-preview.el
;; Version: 1.0.0-alpha.2
;; Package-Requires: ((emacs "28.1") (websocket "1.15") (compat "30.1"))
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
;; https://github.com/Myriad-Dreamin/tinymist

;;;; Installation

;;;;; MELPA

;; Currently not on Melpa

;;;;; Manual

;; Install the tinymist binary from
;; https://github.com/Myriad-Dreamin/tinymist/releases
;; and make sure it's in your $PATH. To test this, create test.typ and run
;; typst-preview test.typ
;; 

;; Include typst-preview.el in your load-path, and put this in your init
;; file:

;; (require 'typst-preview)

;;;; Usage

;; Run typst-preview-mode and typst-preview-start in a typst buffer.

;;;; Tips

;; + You can customize settings in the typst-preview group.

;;; Code:

;;;; Requirements

(require 'websocket)
(require 'json)

;;;; Customization

(defgroup typst-preview nil
  "Settings for `typst-preview-mode'."
  :group 'Languages
  :link '(url-link "https://github.com/havarddj/typst-preview.el"))

;;;; Variables

;; PUBLIC

(defcustom typst-preview-browser "default"
  "Default browser for previewing Typst files.
Options: \"xwidget\", \"eaf-browser\", \"default\"."
  :type 'string
  :group 'typst-preview)

(defcustom typst-preview-invert-colors "auto"
  "Invert colors in preview. Options: \"never\", \"always\", \"auto\"."
  :type 'string
  :group 'typst-preview)

(defcustom typst-preview-executable "tinymist"
  "Path to tinymist binary. Can be relative or absolute."
  :type 'string
  :group 'typst-preview)

(defcustom typst-preview-ask-if-pin-main t
  "If non-NIL, ask if main file should be saved as commented file variable."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-start-subfile-preview-automatically t
  "If non-NIL, start `typst-preview-mode' automatically in subfiles.
More precisely, when prompted from a previewed file to jump to a
buffer currently not running typst-preview, activate `typst-preview-mode'
 and add buffer to children of the relevant master.
This is intended for multi-file projects where a file is included
  using e.g. #include."

  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-open-browser-automatically t
  "If non-NIL, open browser automatically when `typst-preview-start' is run."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-autostart t
  "If non-NIL, start preview automatically when `typst-preview-mode' is activated."
  :type 'boolean
  :group 'typst-preview)

(defcustom typst-preview-center-src t
  "If non-NIL, center typst preview source buffer when jumping to source." :type 'boolean :group 'typst-preview)

(defcustom typst-preview-partial-rendering nil
  "If non-NIL, only render part of the document which is visible."
  :type 'boolean :group 'typst-preview)

(defvar typst-preview-host "127.0.0.1:0"
  "Default address for typst websocket.")

(defcustom typst-preview-cmd-options '()
  "Additional command line options for preview program.
Should be a list of strings."
  :type '(repeat string) :group 'typst-preview)

(defcustom typst-preview-default-dir "."
  "Default root directory for preview. Can be a relative path."
  :type 'string :group 'typst-preview)

;; PRIVATE

(defvar typst-preview--active-buffers '()
  "List of active typst-preview buffers.")


(cl-defstruct typst-preview--master (path :string) process children socket static-host
  control-host)

(defvar typst-preview--active-masters '()
  "List of active typst-preview masters.")

;;;;; Keymaps

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
  :lighter " typst-preview"
  :global nil

  (if typst-preview-mode
      (if typst-preview-autostart
	  (progn
	    (typst-preview-start typst-preview-open-browser-automatically))
	(message "Typst-preview-mode enabled, run typst-preview-start to start preview"))
    (typst-preview-stop))
  :key-map typst-preview-mode-map)


(defvar-local typst-preview--master-file nil
  "Path of typst preview master file for current buffer.")

;; stop emacs from complaining about unsafe buffer local variables
(put 'typst-preview--master-file 'safe-local-variable #'stringp)

(defvar-local typst-preview--local-master nil
  "Typst preview master struct for current buffer.")

;;;; Functions

;;;;; Public

;;;###autoload
(defun typst-preview-start (&optional open-browser)
  "Start typst-preview server and connect current buffer.

If OPEN-BROWSER is set and non-NIL, then it will automatically
open a default browser window."
  (interactive)

  (unless (executable-find typst-preview-executable)
    (error "Typst-preview executable not found. Please install tinymist or modify typst-preview-executable"))
  
  (if (typst-preview-connected-p)
      (message "Typst-preview already connected")

    (unless typst-preview--master-file
      (setq typst-preview--master-file
	    (file-truename
	     (read-file-name (format "Master file (default: %s): " (buffer-file-name)) nil (buffer-file-name))))
      (unless typst-preview--master-file
	(error "No valid master file found for current buffer"))
      (if (and typst-preview-ask-if-pin-main
	       (y-or-n-p "Save master file as local variable?"))
	  (add-file-local-variable 'typst-preview--master-file typst-preview--master-file)))

    (cl-loop for master in typst-preview--active-masters
	     if (string-equal typst-preview--master-file (typst-preview--master-path master))
	     do
	     (push (file-truename buffer-file-name) (typst-preview--master-children master))
	     (setq typst-preview--local-master master))
    
    (unless typst-preview--local-master
      (setq typst-preview--local-master (make-typst-preview--master :path typst-preview--master-file))
      
      (let ((preview-args
	     `(,(executable-find typst-preview-executable) ; get absolute path
	       "preview"
	       ,@(typst-preview--partial-rendering-parameter)
	       "--no-open"
	       "--host" ,typst-preview-host
	       "--control-plane-host" "127.0.0.1:0"
	       "--data-plane-host" "127.0.0.1:0"
	       "--root" ,(file-truename typst-preview-default-dir)
	       "--invert-colors" ,typst-preview-invert-colors
	       ,@typst-preview-cmd-options ,typst-preview--master-file)))
	(message "%s" preview-args)
	(setf (typst-preview--master-process typst-preview--local-master)
	      (apply #'start-process "typst-preview-proc" (get-buffer-create "*ws-typst-server*") preview-args)))

      ;; requires lexical scoping!
      (add-function :before (process-filter (typst-preview--master-process typst-preview--local-master)) #'typst-preview--find-server-filter)

      ;; wait for response from tinymist process to define host addresses
      (let ((timeout-ctr 1))
	(while (or (not (typst-preview--master-control-host typst-preview--local-master))
		   (not (typst-preview--master-static-host typst-preview--local-master)))
	  (sleep-for .01)
	  (if (> (cl-incf timeout-ctr) 300) ; wait 3 seconds
	      (error "Timeout waiting for host addresses from tinymist. See *ws-typst-server* buffer for details"))))

      ;; remove filter to prevent resetting addresses
      (remove-function (process-filter (typst-preview--master-process typst-preview--local-master)) #'typst-preview--find-server-filter)
      
      ;; connect to typst-preview socket
      (setf (typst-preview--master-socket typst-preview--local-master)
	    (websocket-open (concat "ws://" (typst-preview--master-control-host typst-preview--local-master))
			    :custom-header-alist '(("origin" . "vscode-webview://emacs"))
			    :on-message (lambda (socket frame) (typst-preview--parse-message socket frame))
			    :on-close (lambda (_) (message "Typst-preview-mode websocket closed."))))
      (setf (typst-preview--master-children typst-preview--local-master) (list (file-truename buffer-file-name)))
      (push typst-preview--local-master typst-preview--active-masters))

    (add-hook 'after-change-functions
	      #'typst-preview--send-buffer-on-type nil t)

    (if (or open-browser (and typst-preview-open-browser-automatically (called-interactively-p 'any)))
	(typst-preview--connect-browser typst-preview-browser (typst-preview--master-static-host typst-preview--local-master))
      (message "Typst-preview (%s) started, navigate to %s in your browser or run typst-preview-open-browser."
	       (typst-preview--get-tinymist-version-string)
	       (typst-preview--master-static-host typst-preview--local-master)))
    
    (push (current-buffer) typst-preview--active-buffers)
    (add-hook 'kill-buffer-hook #'typst-preview-stop nil t)))

;;;###autoload
(defun typst-preview-connected-p ()
  "Return t if websocket is connected to typst-preview server."
  (and
   (bound-and-true-p typst-preview--local-master)
   (process-live-p (typst-preview--master-process typst-preview--local-master))
   (member (file-truename buffer-file-name) (typst-preview--master-children typst-preview--local-master))))

;;;###autoload
(defun typst-preview-stop ()
  "Stop typst-preview buffer by killing websocket connection."
  (interactive)
  (if (not (typst-preview-connected-p))
      (message "No active typst preview in this buffer.")

    (setq typst-preview--active-buffers (remove (current-buffer) typst-preview--active-buffers))
    (let ((global-master (car (member typst-preview--local-master typst-preview--active-masters))))
      (if (not (string-equal typst-preview--master-file (file-truename buffer-file-name)))
	  (setf (typst-preview--master-children global-master)
		(delete (file-truename buffer-file-name) (typst-preview--master-children global-master)))
	(delete-process (typst-preview--master-process typst-preview--local-master))
	(setf typst-preview--active-masters (delete typst-preview--local-master typst-preview--active-masters))
	(if (eq '() typst-preview--active-masters)
	    (kill-buffer (get-buffer "*ws-typst-server*"))
	  (websocket-close (typst-preview--master-socket typst-preview--local-master))))
      (kill-local-variable 'typst-preview--local-master)
      (remove-hook 'after-change-functions #'typst-preview--send-buffer-on-type t)))
  ;; Make sure only active buffers are in the list typst-preview--active-buffers.
  (setq typst-preview--active-buffers
	(cl-remove-if-not 'buffer-live-p typst-preview--active-buffers)))

;;;###autoload
(defun typst-preview-restart ()
  "Restart typst-preview server."
  (interactive)
  (typst-preview-stop)
  ;; (sleep-for 1)
  (typst-preview-start))

;;;###autoload
(defun typst-preview-send-position ()
  "Send position in buffer to preview server."
  (interactive)
  (let ((msg (json-encode `(("event" . "panelScrollTo")
			    ("filepath" . ,(file-truename buffer-file-name))
			    ("line" . ,(1- (line-number-at-pos)))
			    ("character" . ,(max 1 (current-column)))))))
    (websocket-send-text (typst-preview--master-socket typst-preview--local-master) msg)
    (message "Sending position to typst-preview server")))

(defun typst-preview-sync-memory ()
  "Sync typst-preview memory."
  (interactive)
  (let ((msg (json-encode `(("event" . "syncMemoryFiles")
			    ("files"  (,(file-truename buffer-file-name)
				       . ,(typst-preview--stringify-buffer)))))))
    (websocket-send-text (typst-preview--master-socket typst-preview--local-master) msg)
    ;; (message "syncing memory files")
    ))


;;;###autoload
(defun typst-preview-open-browser ()
  "Open typst-preview browser interactively."
  (interactive)
  (let* ((browser-list '("default" "xwidget" "eaf-browser"))
	 (browser (completing-read "Browser: " browser-list nil nil)))
    (typst-preview--connect-browser browser (typst-preview--master-static-host typst-preview--local-master))))

;;;###autoload
(defun typst-preview-list-active-files ()
  "List active typst files."
  (interactive)
  (let ((str ""))
    (cl-loop for master in typst-preview--active-masters
	     do
	     (setq str (concat str (format "*master:* %s\n"
					   (typst-preview--master-path master))))
	     (cl-loop for child in (typst-preview--master-children master)
		      do
		      (setq str (concat str (format "|--> %s\n" child)))))
    (if (string-equal str "")
	(message "No active typst-preview buffers.")
      (message str))))

;;;###autoload
(defun typst-preview-clear-active-files ()
  "Clear all active typst files."
  (interactive)
  (cl-loop for buffer in typst-preview--active-buffers
	   do (with-current-buffer buffer
	     (typst-preview-stop)))
  (setq typst-preview--active-masters '())
  (setq typst-preview--active-buffers '()))


;;;;; Private

(defun typst-preview--parse-message (sock frame)
  "React to message FRAME received from typst-preview server SOCK."
  (let* ((msg (json-parse-string (websocket-frame-text frame)))
	 (event (gethash "event" msg)))
    (pcase event
      ("editorScrollTo"
       (typst-preview--editor-scroll-to sock msg))
      ("compileStatus"
       (if (string= "CompileError" (gethash "kind" msg))
	   (message "Compile error")))
      ("syncEditorChanges"
       (typst-preview-sync-memory)))))

(defun typst-preview--editor-scroll-to (sock msg)
  "Helper function for receving scroll message MSG from server SOCK."
  (let ((file-name (gethash "filepath" msg))
	(position (gethash "start" msg))
	(master
	 (car (cl-remove-if-not
	       (lambda (master)
		 (eq sock
		     (typst-preview--master-socket master))) typst-preview--active-masters))))
    (typst-preview--goto-file-position file-name position)
    (when (and typst-preview-start-subfile-preview-automatically
	     (not (equal typst-preview--local-master master)))
	  ;; (message "Prompted by websocket, starting typst-preview-mode in %s" file-name)
	  (setq typst-preview--local-master master)
	  (setq typst-preview--master-file (typst-preview--master-path master))
	  (typst-preview-start nil)))
	  (if (not (bound-and-true-p typst-preview-mode))
	      (typst-preview-mode)))

(defun typst-preview--connect-browser (browser hostname)
  "Open browser BROWSER at websocket URL HOSTNAME."
  (let ((full-url (concat "http://" hostname)))
    (pcase browser
      ("xwidget" (xwidget-webkit-browse-url full-url))
      ("eaf-browser" (and (fboundp 'eaf-open-browser-other-window)
			  (eaf-open-browser-other-window full-url)))
      ("default" (browse-url full-url)))))


(defun typst-preview--find-server-filter (_proc input)
  "Filter INPUT to find static and control host addresses."
  (if (string-match "Static file server listening on: \\(.+\\)" input)
      (setf (typst-preview--master-static-host typst-preview--local-master) (match-string 1 input)))
  (if (string-match "Control panel server listening on: \\(.+\\)" input)
      (setf (typst-preview--master-control-host typst-preview--local-master) (match-string 1 input))))

(defun typst-preview--get-buffer-position ()
  "Get position in buffer as a vector."
  (interactive)
  (vector (line-number-at-pos) (current-column)))

(defun typst-preview--goto-file-position (file-name vec)
  "Go to a specific position VEC = (line, column) in file FILE-NAME."
  (message "Going to %s in %s" vec file-name)
  (if (get-file-buffer file-name)
      (pop-to-buffer (get-file-buffer file-name))
    (find-file file-name))
  
  (goto-char (point-min))
  (forward-line (aref vec 0))
  (forward-char (aref vec 1))
  (if typst-preview-center-src
      (recenter-top-bottom)))

(defun typst-preview--stringify-buffer ()
  "Return contents of current buffer as string."
  (save-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun typst-preview--send-buffer ()
  "Send buffer to typst-preview server."
  (let ((content
	 (json-encode `(("event" . "updateMemoryFiles")
			("files"  (,(file-truename buffer-file-name)
				   . ,(typst-preview--stringify-buffer)))))))
    (websocket-send-text (typst-preview--master-socket typst-preview--local-master) content)))

;; according to SO, https://stackoverflow.com/questions/31079099/emacs-after-change-functions-are-not-executed-after-buffer-modification
;; we need to wrap this, and provide input variables
(defun typst-preview--send-buffer-on-type (_begin _end _length)
  "Helper function to send buffer to server on typing.

Needs to include variables BEGIN, END and LENGTH to be
compatible with `after-change-functions'."
  (let ((inhibit-modification-hooks nil))
    (typst-preview--send-buffer)))

(defun typst-preview--get-tinymist-version ()
  "Get version of tinymist in `typst-preview-executable' as list of numbers."
  (mapcar #'string-to-number
	  (split-string
	   (shell-command-to-string (concat typst-preview-executable " -V")) "\\." t)))

(defun typst-preview--get-tinymist-version-string ()
  "Get version of tinymist in `typst-preview-executable' as string."
  (shell-command-to-string (concat typst-preview-executable " -V")))

(defun typst-preview--partial-rendering-parameter ()
  "Return --partial-rendering with or without argument.

This depends on tinymist version. Before v0.13.17, the parameter
was used without argument."
  (if (value< (typst-preview--get-tinymist-version) '(0 13 17) )
      (if typst-preview-partial-rendering '("--partial-rendering") '())
    (list "--partial-rendering" (if typst-preview-partial-rendering "true" "false"))))

;;;; Footer

(provide 'typst-preview)
;;; typst-preview.el ends here
