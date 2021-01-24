(require 'smtpmail)

(define-mail-user-agent 'smtpmail 
  'sendmail-user-agent-compose
  'smtpmail-send-it)
  
(setq mail-user-agent 'smtpmail)
(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq smtpmail-smtp-server "mail.graflex.org")
(setq smtpmail-local-domain "graflex.org")
(setq smtpmail-debug-info t)            ; only to debug problems

(setq mail-mode-hook 
      (function (lambda () 
		  (local-set-key "i" 'etach-attach))))


(provide 'smtpmail-hacks)
