;; tp-test.el                    -*- lexical-binding: t; -*-

;; To run tests, eval buffer then M-x ert
(setq load-prefer-newer t)
(require 'typst-preview)


(ert-deftest preview-test ()
    (save-window-excursion
      (let ((test-file (concat (file-name-directory (locate-library "typst-preview")) "test/test.typ")))
	(find-file test-file)
	(should test-file)		 ; test file present
	(setq typst-preview--master-file test-file) ; prevent asking for main file
	(typst-preview-mode)
	(typst-preview-start)
	(message "typst preview started succesfully, %s" typst-preview--local-master)
	(should (typst-preview-connected-p))
	(sleep-for 1)
	(typst-preview-stop)
	(should (not typst-preview--local-master))
	(kill-this-buffer))))

(ert-deftest include-test ()
    (save-window-excursion
      (let* ((dir (file-name-directory (locate-library "typst-preview")))
	    (test-file (concat dir "test/test-include.typ"))
	    (main-file (concat dir "test/test.typ")))
	(find-file test-file)
	(should test-file)		 ; test file present
	(setq typst-preview--master-file main-file) ; prevent asking for main file
	(typst-preview-mode)
	(typst-preview-start)
	(message "typst preview started succesfully, %s" typst-preview--local-master)
	(should (typst-preview-connected-p))
	(sleep-for 1)
	(typst-preview-stop)
	(should (not typst-preview--local-master))
	(kill-this-buffer))))

(ert-deftest old-binary-test ()
    (save-window-excursion
      (let ((test-file (concat (file-name-directory (locate-library "typst-preview")) "test/test.typ"))
	    (typst-preview-executable (file-truename  "tinymist-v0.13.16/tinymist")))
	(unless (executable-find typst-preview-executable)
	  (ert-fail "Install tinymist v0.13.16 to folder specified in old-binary-test in tp-test.el." ))
	(message "%s" (typst-preview--get-tinymist-version))
	(should (equal (typst-preview--get-tinymist-version) '(0 13 16)))
	(find-file test-file)
	(should test-file)		 ; test file present
	(setq typst-preview--master-file test-file) ; prevent asking for main file
	(typst-preview-mode)
	(typst-preview-start)
	(message "typst preview started succesfully, %s" typst-preview--local-master)
	(should (typst-preview-connected-p))
	(sleep-for 1)
	(typst-preview-stop)
	(should (not typst-preview--local-master))
	(kill-this-buffer))))
