
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

(provide 'python-hacks)
