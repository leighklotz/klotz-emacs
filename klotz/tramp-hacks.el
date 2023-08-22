;;;-*-EMACS-LISP-*-

(require 'tramp)

;;;
;;; Tramp
;;;

;;; C-x C-f
;;; /ssh:root@otherhost:/etc/motd
;;; /ssh:user@host:/homes/user/src/
(customize-set-variable 'tramp-default-method "ssh")


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
