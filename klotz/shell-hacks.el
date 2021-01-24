;;;-*-EMACS-*-

;;;
;;; You will need to change this to whatever your bash prompt looks like
;;;
;;; (defvar shell-hack-bash-prompt "\\(\\(^bash-[0-9.$]+ +\\)\\|\\(^> \\)\\)\\|\\(^[a-zA-Z0-9-]+\\[[0-9]+\\] \\)")
;;; (defvar shell-hack-jdb-prompt  "\\(^[^]]+] \\)")
;;; (defvar shell-hack-jdb-or-bash-prompt (concat shell-hack-jdb-prompt  "\\|" shell-hack-bash-prompt))
;;; 
;;; (defun shell-hack-set-prompt-both ()
;;;   (interactive)
;;;   (shell-hack-set-prompt shell-hack-jdb-or-bash-prompt))
;;; 
;;; (defun shell-hack-set-prompt-bash ()
;;;   (interactive)
;;;   (shell-hack-set-prompt shell-hack-bash-prompt))
;;; 
;;; (defun shell-hack-set-prompt-jdb ()
;;;   (interactive)
;;;   (shell-hack-set-prompt shell-hack-jdb-prompt))
;;; 
;;; (defun shell-hack-set-prompt (prompt)
;;;   (setq shell-prompt-pattern prompt)
;;;   (setq comint-prompt-regexp shell-prompt-pattern
;;;         comint-use-prompt-regexp t))
;;; 
; (shell-hack-set-prompt-bash)
;;;
;;; You will need to change this to wherever your have installed cygwin.
;;;
;; (setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")

;;;
;;; This keeps the shell window from putting the current line
;;; at the bottom of the screen whenever you type.
(setq comint-scroll-show-maximum-output nil)



;;;
;;; Mode hook for shell mode
;;;
(defun my-comint-mode-hook ()
  "This defines m-` as \\[[fix-bugs]] (which defaults to \\[[fix-warnings]] or \\[[fix-errors]] depending on which you did last
It defines m-n and m-p to bring in line matching what you've typed so far.
It defines c-a to go to beginning of line, not beginning of prompt, though this may be the default now in emacs 22."
  (if  ;; not necessary in emacs-23
      (or (not (boundp 'kill-buffer-query-functions)) (not (memq 'process-kill-buffer-query-function kill-buffer-query-functions)))
      (add-hook 
       'kill-buffer-hook
       (function
        (lambda () 
          (if
              (and (get-buffer-process (current-buffer)) (not (yes-or-no-p "Process running -- kill anyway?")))
              (error "You cannot kill buffer %s because the process is still running." (buffer-name)))))))
  (setq comint-input-ring-size 1000)
;;  (if (not (member 'comint-watch-for-password-prompt comint-output-filter-functions))
;;      (setq comint-output-filter-functions (cons 'comint-watch-for-password-prompt comint-output-filter-functions)))
;;; Makes meta-p not repeatedly give yout the same thing
  (setq comint-input-ignoredups t)
  (local-set-key "\e`" 'fix-bugs)
  (local-set-key "\ep" 'comint-previous-matching-input-from-input)
  (local-set-key "\en" 'comint-next-matching-input-from-input)
  (local-set-key [(control a)] 'comint-bol))

(add-hook 'comint-mode-hook 'my-comint-mode-hook)


(setq comint-input-ring-size 1000)

;;;
;;; This section makes it so that m-x shell gives you a new shell buffer with a unique name based on the current directory.
;;;
(require 'shell) ;; should do this with advice
(if (not (fboundp 'old-shell))
    (fset 'old-shell (symbol-function 'shell)))

(defun shell (&optional BUFFER)
  "Run an inferior shell, with I/O through BUFFER (which defaults to \\[next-shell-buffer-name]).
This is a customized function which calls \\[old-shell]."
  (interactive
   (list
    (read-buffer "Shell buffer: " (suggest-shell-buffer-name))))
  (old-shell BUFFER))

(defun suggest-shell-buffer-name () 
  (let* ((last-component (file-name-nondirectory (substring default-directory 0 (1- (length default-directory)))))
         (name (concat "*" last-component "*")))
    (if (not (get-buffer name))
        name
      (next-shell-buffer-name 0))))    

(defun next-shell-buffer-name (i)
  (let ((name (if (zerop i) "*shell*" (format "*shell-%c*" (+ ?a i -1)))))
    (if (not (get-buffer name))
        name
      (next-shell-buffer-name (1+ i)))))



;;;
;;; Do you want beeps from your emacs shells?
; (set-message-beep 'silent)

;; m-x compile is nice but i need to set up the compilation environment in a shell
;; and then grab the output
(defun fix-bugs () 
  (interactive)
  (let ((errors (buffer-substring 
                 (save-excursion
                   (comint-previous-prompt 1)
                   (point))
                 (point))))
    (switch-to-buffer "*compilation*")
    (kill-region (point-min) (point-max))
    (insert errors))
  (compilation-mode)
  (next-error))


;;;
;;; Start with fix-errors then do fix-bugs
;;; Then when all errors are fixed kill the *compilation* buffer and start over with fix-warnings then fix-bugs.
;;; The kill *compilation* buffer and compile again.
;;;
(defun fix-errors()
  (interactive)
  "Make `next-error` go to next error, skipping warnings. See `fix-warnings`"
  (setq compilation-skip-threshold 2)
  (fix-bugs))

(defun fix-warnings()
  (interactive)
  "Make `next-error` go to next warning, not just next error. See `fix-errors`"
  (setq compilation-skip-threshold 1)
  (fix-bugs))

(provide 'shell-hacks)
