;;;-*-EMACS-LISP-*-

(require 'ack)

(setq ack-guess-type t)

(if (file-exists-p "~/.ackrc")
    (setq ack-command "ack"))           ;assume it has  --nocolor --nogroup in it.

(provide 'ack-hacks)
