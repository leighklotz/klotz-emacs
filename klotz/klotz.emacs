;;;-*-EMACS-LISP-*-

(push "~klotz/.emacs.d/klotz" load-path)

(when (>= emacs-major-version 25)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

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
; (require 'ace-window)

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Turn on font-lock mode for Emacs
(if (not running-xemacs)
    (global-font-lock-mode t))

;;; Bell
(setq ring-bell-function
      #'(lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;;;
;;; General/text
;;;

(setq text-mode-hook #'(lambda () (setq require-final-newline 'ask)))

;;; Newlines
;; Always end a file with a newline
(setq require-final-newline 'ask)
;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)
(setq line-move-visual nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
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

;;;
;;; PHP
;;;
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

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

;;; if you ever see custom-set-variables below here, move it to the correct
;;; file above
