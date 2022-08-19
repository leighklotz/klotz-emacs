;;;-*-EMACS-LISP-*-

(push "~klotz/.emacs.d/klotz" load-path)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(require 'keybinding-hacks)
(require 'smtpmail-hacks)
(require 'text-hacks)
(require 'case-hacks)
(require 'shell-hacks)
(require 'buffer-hacks)
(require 'ack-hacks)
(require 'tramp-hacks)
(require 'desktop-hacks)
(require 'hex)
(require 'how-do-i)
(require 'uniquify)
(require 'ace-window)

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Turn on font-lock mode for Emacs
(if (not running-xemacs)
    (global-font-lock-mode t))

;;;
;;; General/text
;;;

(setq text-mode-hook #'(lambda () (setq require-final-newline 'ask)))

(setq dabbrev-case-replace 'case-replace)
(setq line-move-visual nil)
(setq next-line-add-newlines nil)
(setq require-final-newline 'ask)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; when left at the default, if you type a single lowercase letter and it matches a camel-case word, it lowercases the word.
(setq dabbrev-case-fold-search nil)
(setq comint-input-ring-size 1000)
(setq inhibit-startup-message t)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;;
;;; Mode stuff
;;;

;;; m-x compile
;;; grab the output from a compilation command in a shell
(defvar my-bash-prompt (format "\n\\[klotz@%s[^\\$\\]*]$ " (system-name)))

;;;
;;; Arduino
;;;
(setq auto-mode-alist (cons '("\\.ino" . c++-mode) auto-mode-alist))

;;;
;;; Java
;;;
(add-hook 'java-mode-hook
	  #'(lambda ()
	      (c-set-style "gnu")
	      (setq require-final-newline 'ask)
	      (local-set-key (kbd "C-x #") 'copy-line-number-for-jdb)))


;;; Bell
(setq ring-bell-function
      #'(lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))


;;;
;;; Custom
;;;
;;; Maintain custom-set-variables, custom-set-faces, etc.
;;; in a well-known file, one for each hostname.  Custom commnads
;;; in emacs will respect this variable and edit that file instead of
;;; editing ~/.emacs
(setq custom-file (concat "~/.emacs.d/klotz/custom-" (system-name) ".el"))
(if (not (file-exists-p custom-file))
    (message "Please create empty custom file %s" custom-file)
  (load custom-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ack string-inflection yaml-mode tldr scala-mode pig-mode php-mode markdown-mode json-mode graphviz-dot-mode go-mode flymake-json exec-path-from-shell csv-mode csv company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
