;;;-*-EMACS-LISP-*-
;;; http://www.gnu.org/software/emacs/manual/html_node/flymake/Example-_002d_002d-Configuring-a-tool-called-directly.html

(require 'flymake)

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (setq foo temp-file)
    (list "perl" (list "-wc " local-file))))


(add-to-list 'flymake-allowed-file-name-masks
      '(".+\\.pl$"
	flymake-perl-init
	flymake-simple-cleanup
	flymake-get-real-file-name))

(add-to-list 'flymake-err-line-patterns
      '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]"
                   2 3 nil 1))


(provide 'flymake-perl)
