;;;-*-EMACS-*-

;;; Flymake PHP Extension

(require 'flymake)
(require 'php-mode)

(define-key php-mode-map '[c-c c-p] 'flymake-goto-prev-error)
(define-key php-mode-map '[c-c c-n] 'flymake-goto-next-error)
(define-key php-mode-map '[c-c c-f] 'flymake-mode)

(provide 'flymake-php)
