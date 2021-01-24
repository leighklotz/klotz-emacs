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


;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
	(global-font-lock-mode t)
))

;; Always end a file with a newline
(setq require-final-newline 'ask)

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
(require 'schema)

(put 'narrow-to-region 'disabled nil)

(put 'set-goal-column 'disabled nil)

(setq line-move-visual nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Blue")))))

(global-set-key "%" 'query-replace-regexp)
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

; doesn't do fontification for some reason
;(defun perldoc (man-args)
;  (interactive "sPerldoc: ")
; (require 'man)
;  (let ((manual-program "perldoc"))
;    (man man-args)))
(defun hex-string-to-ascii (string) 
  (do ((new (make-string (/ (length string) 2) 0))
       (i 0 (+ i 1))
       (len (/ (length string) 2)))
      ((= i len) new)
    (setf (aref new i)
	    (+ (* 16 (hexify (aref string (* i 2))))
	            (hexify (aref string (+ 1 (* i 2))))))))

(defun hexify (ascii) 
  (cond ((< ascii ?0) (error "digit %s not understood" ascii))
	((<= ascii ?9) (- ascii ?0))
	((<= ascii ?F) (- ascii (- ?A 10)))
	((<= ascii ?f) (- ascii (- ?a 10)))
	(t (error "digit %s not understood" ascii))))

(put 'set-goal-column 'disabled nil)

(global-set-key [mouse-5] 'scroll-down)
(global-set-key [mouse-4] 'scroll-up)

;;; abbrev
;; when left at the default, if you type a single lowercase letter and it matches a camel-case word, it lowercases the word.
(setq dabbrev-case-fold-search nil)

; comint
(setq comint-input-ring-size 1000)

;; m-x compile is nice but i need to set up the compilation environment in a shell
;; and then grab the output
(defvar my-bash-prompt "\n\\[klotz@pinstripe[^\\$\\]*]$ ")
(defun fix-bugs () 
  (interactive)
  (let ((errors (buffer-substring (save-excursion (beginning-of-line 0) (if (re-search-backward my-bash-prompt nil t) (point) (point-min))) (point))))
    (switch-to-buffer "*compilation*")
    (kill-region (point-min) (point-max))
    (insert errors))
  (compilation-mode)
  (next-error))

(load "~klotz/emacs/rnc-mode")

(fset 'xml-mode 'nxml-mode)
(add-hook 'flyspell-mode-hook
          '(lambda ()
             (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

; (push "~klotz/emacs/nxml-mode-20041004/" load-path)

; (load-library "nxml-mode")
; (load "~klotz/emacs/nxml-mode-20041004/rng-auto.el")
(setq auto-mode-alist
        (cons '("\\.\\(xslt\\|vdf\\|html\\|xhtml\\|xml\\|xsl\\|rng\\|xhtml\\|xsd\\)\\'" . nxml-mode)
	      auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pde" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ino" . c++-mode) auto-mode-alist))

(load "~klotz/emacs/xforms.el")

(defun line-number-region () 
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (let ((i 0))
	(while (<= (point) (point-max))
	  (beginning-of-line 1)
	  (insert (format "%d " i))
	  (setq i (1+ i))
	  (next-line 1))))))

(add-hook 'flyspell-mode-hook 'my-flyspell-mode-hook)
(defun my-flyspell-mode-hook ()
  (setq a 3)
  (define-key flyspell-mode-map (kbd "C-c C-j") 'flyspell-check-previous-highlighted-word))

;; (require 'flymake-python)
;; (require 'flymake-cursor)
;; (require 'my-flymake)
(require 'how-do-i)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))



(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;;; Raman's Google Hacks
;;; (push "~klotz/emacs/g-client/" load-path)
;;; (load-library "g")
;;; (load-library "time-date")
;;;
;;;(custom-set-variables
;;; '(browse-url-browser-function (quote w3m-browse-url))
;;; '(gblogger-user-email "leigh.klotz@gmail.com")
;;; '(gblogger-user-password nil))

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(isearch-lax-whitespace nil t)
 '(nodemcu-debug-io t)
 '(nodemcu-interface (quote nodemcu-interface-serial))
 '(nodemcu-target-device "/dev/ttyUSB1")
 '(package-selected-packages
   (quote
   ;; php+-mode
   ;; flycheck
    (string-inflection json-mode ham-mode emojify aggressive-indent ack)))
 '(safe-local-variable-values (quote ((backup-inhibited . t)))))

(defun newpw ()
  (interactive)
  (let ((pw
	 (with-temp-buffer
	   (shell-command "pwgen -s 12 | head -1" (current-buffer))
	   (buffer-substring (point-min) (1- (point-max))))))
    (insert pw)))
    

