;; Red Hat Linux default .emacs initialization file

(setq load-path (cons "d:/cygwin/usr/local/share/emacs/site-lisp/" load-path))

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

(global-set-key [(control ?x) (?%)] 'query-replace-regexp)
(global-set-key "\e=" 'compare-windows)
(global-set-key "\e`" 'next-error)

(global-unset-key [(control z)]) ;why would I want this?
(global-unset-key [(control x)(control z)]) ;why would I want this?

; someone turned M-g into a prefix command for font stuff, making it impossible to define in local modes.
(global-unset-key "\eg")
(global-set-key "\eg" 'goto-line)


;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
	(global-font-lock-mode t)
))

(setq c-default-style "java")
(setq java-mode-hook
      '(lambda () 
	 (setq c-default-style "java")
	 (define-key "[(control meta x)]" 'copy-line-number-for-jdb)
	 (c-set-style c-default-style)))


;; Always end a file with a newline
(setq require-final-newline 'ask)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)



(require 'smtpmail)
(define-mail-user-agent 'smtpmail 
  'sendmail-user-agent-compose
  'smtpmail-send-it)
  
(setq mail-user-agent 'smtpmail)
(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq smtpmail-smtp-server "mailhost.adoc.xerox.com")
(setq smtpmail-local-domain "adoc.xerox.com")
(setq smtpmail-debug-info t) ; only to debug problems

(setq explicit-shell-file-name "c:/ds/cygwin/bin/bash.exe")
(add-hook 'comint-mode-hook
	  (function 
	   (lambda ()
	     (add-hook 
	      'kill-buffer-hook
		   (function
			(lambda () 
			  (if
			      (and (get-buffer-process (current-buffer)) (not (yes-or-no-p "Process running -- kill anyway?")))
			      (error "You cannot kill buffer %s because the process is still running." (buffer-name))))))
	     (setq comint-input-ring-size 1000)
	     (if (not (member 'comint-watch-for-password-prompt comint-output-filter-functions))
		 (setq comint-output-filter-functions (cons 'comint-watch-for-password-prompt comint-output-filter-functions)))
	     (local-set-key "\e`" 'fix-bugs)
	     (local-set-key "\ep" 'comint-previous-matching-input-from-input)
	     (local-set-key "\en" 'comint-next-matching-input-from-input)
	     (local-set-key [(control a)] 'comint-bol))))

(global-set-key "\e " 'dabbrev-expand)
(global-set-key (quote [-67108832]) 'just-one-space)    


(load "~klotz/emacs/etach-1.1.4/etach")
(load "~klotz/emacs/schema")

(put 'narrow-to-region 'disabled nil)


(put 'set-goal-column 'disabled nil)

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

;;; abbrev
;; when left at the default, if you type a single lowercase letter and it matches a camel-case word, it lowercases the word.
(setq dabbrev-case-fold-search nil)

; comint
(setq comint-input-ring-size 1000)

;; Visual Basic mode
;; downloaded from http://www.gest.unipd.it/~saint/visual-basic-mode.el.gz
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq visual-basic-capitalize-keywords-p nil)
(setq auto-mode-alist (append '(("\\.\\(vbs\\|bas\\|cls\\)$" . visual-basic-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cs$" . java-mode)) auto-mode-alist))


;; m-x compile is nice but i need to set up the compilation environment in a shell
;; and then grab the output
(defvar my-bash-prompt "\nbash-3.00$ ")
(defun fix-bugs () 
  (interactive)
  (let ((errors (buffer-substring (save-excursion (beginning-of-line 0) (if (search-backward my-bash-prompt nil t) (point) (point-min))) (point))))
    (switch-to-buffer "*compilation*")
    (kill-region (point-min) (point-max))
    (insert errors))
  (compilation-mode)
  (next-error))


(defun nodos ()
  (interactive)
  (set-buffer-file-coding-system 'iso-8859-1-unix))

(defun insert-date ()
  (interactive)
  (insert (save-excursion (shell-command "date") (set-buffer  "*Shell Command Output*") (goto-char (point-min)) (replace-string "\n" "") (buffer-string))))


(load "~klotz/emacs/rnc-mode")
;(load "~klotz/emacs/nxml-mode-20030912/rng-auto.el")
;(load "~klotz/emacs/nxml-mode-20030915/rng-auto.el")
;(load "~klotz/emacs/nxml-mode-20030928/rng-auto.el")
;(load "~klotz/emacs/nxml-mode-20030929/rng-auto.el")
;(load "~klotz/emacs/nxml-mode-20030929/rng-auto.el")
;(load "~klotz/emacs/nxml-mode-20031031/rng-auto.el")
;(load "~klotz/emacs/nxml-mode-20040908/rng-auto.el")
(load "~klotz/emacs/nxml-mode-20041004/rng-auto.el")
(setq auto-mode-alist
        (cons '("\\.\\(vdf\\|html\\|xhtml\\|xml\\|xsl\\|rng\\|xhtml\\|xsd\\)\\'" . nxml-mode)
	      auto-mode-alist))
					; (load "~/emacs/my-psgml")
;; (defun xhtml-mode ()
;;   (interactive)
;;   (html-mode)
;;   (xml-lite-mode))
;; 
;; (setq auto-mode-alist (append '(("\\.xhtml\\'" . xhtml-mode)) auto-mode-alist))
;; (setq auto-mode-alist (append '(("\\.xml\\'" . xhtml-mode)) auto-mode-alist))
;; (setq auto-mode-alist (append '(("\\.xsd\\'" . xhtml-mode)) auto-mode-alist))
;; 
;; (load "~/emacs/xml-lite")
;; 
;; (setq xml-lite-indent-offset 2)
;; (setq-default xml-lite-indent-offset 2)
;; (setq xml-lite-electric-slash nil)
;; (setq-default xml-lite-electric-slash nil)
;; 
(load "~klotz/emacs/xforms.el")

(put 'downcase-region 'disabled nil)



(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(mail-host-address "sushi.adoc.xerox.com")
 '(archive-zip-use-pkzip nil)
 '(user-mail-address "leigh.klotz@xerox.com"))

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

(load-library "shell")
(defun shell-replace-in-string (string regexp newtext)
  "In STRING, replace all matches for REGEXP with NEWTEXT.
Hack to get a common function for all Emacsen.  Note that Oort Gnus has
`gnus-replace-in-string', but we don't want to load Gnus."
  (cond
   ;; Emacs 21 and later
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp newtext string))
   ;; Emacs < 21; XEmacs
   (t
    ;; Code duplicated from `subr.el' revision 1.423 of Emacs.  Neither
    ;; `replace-in-string' from XEmacs 21.4.15 nor the Gnus replacement works
    ;; correctly when an empty string is matched.
    (let ((rep newtext)
	  (l (length string))
	  (start 0) ;; (or start 0) in `subr.el'
	  fixedcase literal subexp
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0 str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb)	; unmatched prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ; leftover
	(apply #'concat (nreverse matches)))))))

(defun shell-cd (dir)
  "Do normal `cd' to DIR, and set `list-buffers-directory'."
  (setq dir (shell-replace-in-string dir "^/cygdrive/\\(.\\)" "\\1:"))
  (if shell-dirtrackp
      (setq list-buffers-directory (file-name-as-directory
				    (expand-file-name dir))))
  (cd dir))


  
  
;;; From MLY

;; Like in $es/buffer.c, except that I don't allow the creation 
;;  of new buffers interactively unless a prefix arg is given.
;;  Saves lossage, makes Return completion win.
(or (fboundp 'original-switch-to-buffer)
     (define-function 'original-switch-to-buffer
         (symbol-function 'switch-to-buffer)))

(defun switch-to-buffer (buffer &optional norecord)
  "Select buffer BUFFER in the current window.
BUFFER may be a buffer or a buffer name.
Optional second arg NORECORD non-nil means do not put this buffer at the front of the list of recently selected ones.

WARNING: This is NOT the way to work on another buffer temporarily within a Lisp program!  Use `set-buffer' instead.  That avoids messing with the window-buffer correspondances."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer (current-buffer))
		      (null current-prefix-arg))))
  (original-switch-to-buffer buffer norecord))

;; like in $el/files.el, expect that I won't create a new 
;;  buffer unless a prefix arg is given 
(or (fboundp 'original-switch-to-buffer-other-window)
    (define-function 'original-switch-to-buffer-other-window
      (symbol-function 'switch-to-buffer-other-window)))

(defun switch-to-buffer-other-window (buffer)
  "Select buffer BUFFER in another window."
  (interactive
   (list (read-buffer "Switch to buffer in other window: "
		      (other-buffer (current-buffer))
		      (null current-prefix-arg))))
  (original-switch-to-buffer-other-window buffer))


(defun copy-line-number-for-jdb () 
  "Copy the current fully-qualified class name and line number to the kill buffer for use with JDB"
  (interactive)
  (let ((package
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-min))
	     (re-search-forward "^\\S *package \\([a-zA-Z0-9_.]+\\)\\S *;")
	     (match-string 1))))
	(class (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))) ;not right for non-public classes
	(line-number (1+ (count-lines 1 (point)))))
    (kill-new (format "%s.%s:%d" package class line-number))))


