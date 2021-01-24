(tool-bar-mode 0)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

(global-set-key [(control ?x) (?%)] 'query-replace-regexp)
(global-set-key "\e=" 'compare-windows)
(global-set-key "\e`" 'next-error)
(global-set-key [mouse-4] 'my-scroll-down)
(global-set-key [mouse-5] 'my-scroll-up)
(global-set-key [(control z)] 'undo)
(global-unset-key [(control x)(control z)]) ;why would I want this?

                                        ; someone turned M-g into a prefix command for font stuff, making it impossible to define in local modes.
(global-unset-key "\eg")
(global-set-key "\eg" 'goto-line)
(global-set-key [control l] 'recenter)

(defun my-scroll-down ()
  (interactive)
  (scroll-down 3))

(defun my-scroll-up ()
  (interactive)
  (scroll-up 3))

;;; Bucky Bits!
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil)
(setq w32-lwindow-modifier 'super)  ; lwindow acts as super
(setq w32-rwindow-modifier 'super)  ; rwindow acts as super, not hyper

; (global-set-key "\eP" 'my-crash-sound)

(provide 'keybinding-hacks)
