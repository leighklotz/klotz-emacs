;;;-*-EMACS-*-
(push "~klotz/.emacs.d/klotz" load-path)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
;;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(require 'keybinding-hacks)
(require 'smtpmail-hacks)
(require 'text-hacks)
(require 'case-hacks)
(require 'shell-hacks)
(require 'ack-hacks)
(require 'tramp-hacks)
(require 'desktop-hacks)
(require 'hex)


;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
	(global-font-lock-mode t)
))

;; Always end a file with a newline
(setq require-final-newline 'ask)
;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Require text files with a newline
(setq text-mode-hook 
      (function (lambda ()
		  (setq require-final-newline 'ask))))

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(add-hook 'java-mode-hook
	  '(lambda ()
	     (c-set-style "gnu")
	     (setq require-final-newline 'ask)
	     (local-set-key (kbd "C-x #") 'copy-line-number-for-jdb)))

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(setq dabbrev-case-replace 'case-replace)
(global-set-key "\e " 'dabbrev-expand)
(global-set-key (quote [-67108832]) 'just-one-space)    

(require 'qra)

(put 'narrow-to-region 'disabled nil)

(put 'set-goal-column 'disabled nil)

(setq line-move-visual nil)

;;; Maintain custom-set-variables, custom-set-faces, etc.
;;; in a well-known file, one for each hostname.  Custom commnads
;;; in emacs will respect this variable and edit that file instead of
;;; editing ~/.emacs
(setq custom-file (concat "~/.emacs.d/klotz/custom-" (system-name) ".el"))
(if (not (file-exists-p custom-file))
    (message "Please create empty custom file %s" custom-file)
  (load custom-file))

(global-set-key "%" 'query-replace-regexp)

(put 'set-goal-column 'disabled nil)

(global-set-key [mouse-5] 'scroll-down)
(global-set-key [mouse-4] 'scroll-up)

;;; abbrev
;; when left at the default, if you type a single lowercase letter and it matches a camel-case word, it lowercases the word.
(setq dabbrev-case-fold-search nil)

; comint
(setq comint-input-ring-size 1000)

;; m-x compile is nice but i need to set up the compilation environment in a shell
;; and then grab the output.
(defvar my-bash-prompt (format "\n\\[klotz@%s[^\\$\\]*]$ " (system-name)))

(setq auto-mode-alist (cons '("\\.pde" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ino" . c++-mode) auto-mode-alist))

(add-hook 'flyspell-mode-hook 'my-flyspell-mode-hook)
(defun my-flyspell-mode-hook ()
  (setq a 3)
  (define-key flyspell-mode-map (kbd "C-c C-j") 'flyspell-check-previous-highlighted-word))

(require 'how-do-i)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


