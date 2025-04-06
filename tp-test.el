;; tp-test.el                    -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(require 'typst-preview)


(ert-deftest preview-test ()
    (save-window-excursion
      (let ((test-file (concat (file-name-directory (locate-library "typst-preview")) "test.typ")))
	(find-file test-file)
	(should test-file)		 ; test file present
	(setq tp--master-file test-file) ; prevent asking for main file
	(typst-preview-mode)
	(typst-preview-start)
	(message "typst preview started succesfully, %s" tp--local-master)
	(should (typst-preview-connected-p))
	(sleep-for .5)
	(typst-preview-stop)
	(should (not tp--local-master))
	(kill-this-buffer))))

(ert-deftest include-test ()
    (save-window-excursion
      (let* ((dir (file-name-directory (locate-library "typst-preview")))
	    (test-file (concat dir "test-include.typ"))
	    (main-file (concat dir "test.typ")))
	(find-file test-file)
	(should test-file)		 ; test file present
	(setq tp--master-file main-file) ; prevent asking for main file
	(typst-preview-mode)
	(typst-preview-start)
	(message "typst preview started succesfully, %s" tp--local-master)
	(should (typst-preview-connected-p))
	(sleep-for .5)
	(typst-preview-stop)
	(should (not tp--local-master))
	(kill-this-buffer))))
