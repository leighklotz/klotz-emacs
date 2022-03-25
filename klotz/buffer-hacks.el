;;;-*-EMACS-LISP-*-

(defun buffer-trivial-p (buffer)
  (not (buffer-non-trivial-p buffer)))

(defun buffer-non-trivial-p (buffer)
   (and
    (not (string= (substring (buffer-name buffer) 0 1) " "))
    (not (member (buffer-name buffer) '("*scratch*" "*Messages")))
    (not (null (buffer-file-name buffer)))
    ;;(buffer-modified-p buffer)
    ))

(defun count-nontrivial-buffers () 
  (seq-count #'buffer-non-trivial-p (buffer-list)))

(defun exit-emacs-only-if-trivial ()
  (interactive)
  (if (> (count-nontrivial-buffers) 1)
      (beep)
    (kill-emacs)))

(define-key ctl-x-map "\C-c" 'exit-emacs-only-if-trivial)

(provide 'buffer-hacks)
