;;;-*-EMACS-LISP-*-
;;; Leigh Klotz <klotz@klotz.me>
;;; Keybindings and general global key fixes
;;;

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

(global-set-key (kbd "C-M-SPC") 'just-one-space)
(global-set-key (kbd "C-X %") 'query-replace-regexp)
(global-set-key (kbd "C-%") 'query-replace-regexp)
(global-set-key (kbd "M-=") 'compare-windows)
(global-set-key (kbd "M-`") 'next-error)
(global-set-key (kbd "C-z") 'undo)
(global-unset-key (kbd "M-h")) 		;don't want iconify-frame
(global-unset-key (kbd "s-w")) 		;don't want delete-frame
(global-unset-key (kbd "s-h")) 		;don't want ns-do-hide-emacs
(global-unset-key (kbd "C-x C-z"))

;; Mouse
(global-set-key [mouse-4] 'my-scroll-down)
(global-set-key [mouse-5] 'my-scroll-up)

;; someone turned M-g into a prefix command for font stuff,
;; making it impossible to define in local modes.
(global-unset-key (kbd "M-g"))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-l") 'recenter)

;; emacs font stuff has never intrigued me
(global-unset-key (kbd "M-o"))
; this is too painful
; (global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o") 'other-window)

;; https://karthinks.com/software/batteries-included-with-emacs/#dwim-commands--upcase-downcase-and-more
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(defun my-scroll-down ()
  (interactive)
  (scroll-down 3))

(defun my-scroll-up ()
  (interactive)
  (scroll-up 3))

;;; Bucky Bits!
(when (memq system-type '(windows-nt cygwin ms-dos))
  (setq w32-pass-lwindow-to-system nil
	w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)	; lwindow acts as super
  (setq w32-rwindow-modifier 'super)    ; rwindow acts as super, not hyper
  )

;;; Meta-space to predictive text
;;; Switch from dabbrev-expand to hippie-expand
(cond ((fboundp 'hippie-expand) 
       (global-set-key (kbd "M-SPC") 'hippie-expand))
      (t 
       (global-set-key (kbd "M-SPC") 'dabbrev-expand)
       ;; when left at the default, if you type a single lowercase letter
       ;; and it matches a camel-case word, it lowercases the word.
       (setq dabbrev-case-replace 'case-replace)
       (setq dabbrev-case-fold-search nil)))

; (global-set-key "\eP" 'my-crash-sound)

(global-set-key [mouse-5] 'scroll-down)
(global-set-key [mouse-4] 'scroll-up)
(global-set-key (kbd "C-M-SPC") 'just-one-space)
;;(global-set-key "\e " 'dabbrev-expand)
(global-set-key "\e " 'hippie-expand)
(global-set-key "%" 'query-replace-regexp)
(define-key global-map [?\s-p] 'query-replace-regexp) ;; just prevent jarring ns-print-buffer, find a key later
(define-key global-map [?\s-a] 'back-to-indentation) ;; also on Meta-M
(define-key global-map [?\s-e] 'move-to-end-of-code-line) ;; RMS wouldn't let me add this in 1981 until I found a key

(provide 'keybinding-hacks)
