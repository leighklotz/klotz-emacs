;;; -*-EMACS-*-
;;; Oh my this is old

; doesn't do fontification for some reason
;(defun perldoc (man-args)
;  (interactive "sPerldoc: ")
; (require 'man)
;  (let ((manual-program "perldoc"))
;    (man man-args)))

(defun perldoc (module)
  "Get perldoc on MODULE"
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((buffer-name (concat "*Perldoc " module "*")))
    (save-excursion 
      (set-buffer (get-buffer-create buffer-name))
      (delete-region (point-min) (point-max))
      (shell-command (concat "perldoc " (shell-quote-argument module)) buffer-name)
      (goto-char (point-min))
      (Man-fontify-manpage)
      (goto-char (point-min))
      (replace-regexp "." ""));there are still some left.
    (switch-to-buffer-other-window buffer-name)))
