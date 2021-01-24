(setq flymake-gui-warnings-enabled nil)

(defvar my-flymake-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'flymake-goto-prev-error)
    (define-key map (kbd "C-c C-n") 'flymake-goto-next-error)
    map)
  "Keymap for my flymake minor mode.")

(define-minor-mode my-flymake-minor-mode
  "Simple minor mode which adds some key bindings for moving to the next and previous errors.

Key bindings:

\\{my-flymake-minor-mode-map}"
  nil
  nil
  :keymap my-flymake-minor-mode-map)

(add-hook 'flymake-mode-hook 'my-flymake-minor-mode)

(provide 'my-flymake)
