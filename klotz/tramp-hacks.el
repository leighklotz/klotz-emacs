;;;-*-EMACS-LISP-*-

(require 'tramp)

;; from https://www.emacswiki.org/emacs/TrampMode
;; was called find-alternative-file-wtihn-sudo
(defun sudo-edit-file ()
  (interactive)
  (let ((fname (or buffer-file-name
		   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
	  (setq fname (replace-regexp-in-string
		       "^/sudo:root@localhost:" ""
		       fname))
	(setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

(provide 'tramp-hacks)

