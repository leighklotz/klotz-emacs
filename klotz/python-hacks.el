
;; Pyflakes for python
;; (when (load "flymake" t)
;;   (defun flymake-pychecker-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "/home/klotz/emacs/pyflakespep8.py" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pychecker-init)))
;; 
;; (when (load "flymake" t)
;;    (defun flymake-pylint-init ()
;;      (message "flymake-pylint-init")
;;      (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                         'flymake-create-temp-inplace))
;;             (local-file (file-relative-name
;;                          temp-file
;;                          (file-name-directory buffer-file-name))))
;;        ;;(list "epylint" (list local-file))
;;        (setq a (list "/home/klotz/emacs/virtual-epylint" (list (or python-shell-virtualenv-path ".") local-file)))))
;;    (add-to-list 'flymake-allowed-file-name-masks
;;                 '("\\.py\\'" flymake-pylint-init)))
;; 

        

;; set python-shell-virtualenv-path in .dir-locals.el

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            python-indent-offset 4
                            tab-width 4))))

;;; This function is from http://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;;; It removes unused imports from the current buffer.
;;; It works by finding all imports, and then checking if they are used.
;;; If they are not used, they are removed.
;;; it checks to see if they are used by searching for the module name in the buffer.
(defun python-remove-unused-imports ()
  "Removes unused imports from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^import \\([^ ]+\\)$" nil t)
      (let ((module (match-string 1)))
	(when (not (save-excursion
		     (goto-char (point-max))
		     ;; go to the last import statement
		     ;; if there are imports after that, sorry.
		     (re-search-backward "^import \\([^ ]+\\)$" nil t)
		     (or 
		      ;; check to see if the module name is referenced in the buffer
		      (re-search-forward (concat "\\([^a-zA-Z0-9]\\|^\\)" module "\\([^a-zA-Z0-9]\\|$\\)") nil t)
		      (re-search-forward (concat "^from " module " import") nil t))))
	  (kill-whole-line))))))

(provide 'python-hacks)
