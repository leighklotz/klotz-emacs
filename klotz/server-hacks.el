;;;-*-EMACS-LISP-*-

(require 'server)

(add-hook 'server-switch-hook
              (lambda nil
                (let ((server-buf (current-buffer)))
                  (bury-buffer)
                  (switch-to-buffer-other-frame server-buf))))

(if (not (server-running-p))
    (server-start)
  (message "Another emacs is already running the emacs-client server."))

(provide 'server-hacks)
