(require 'flymake)
  
(defun flymake-pyflakes-init () 
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 
		     'flymake-create-temp-inplace)) 
	 (local-file (file-relative-name 
		      temp-file 
		      (file-name-directory buffer-file-name)))) 
    (list "pyflakes" (list "--ignore=E111" local-file))))

(defun flymake-pep8-init () 
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 
		     'flymake-create-temp-inplace)) 
	 (local-file (file-relative-name 
		      temp-file 
		      (file-name-directory buffer-file-name)))) 
    (list "pep8" (list "--ignore=E111,E401,E501" "--repeat" local-file)))) 

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init))

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
             'flymake-create-temp-inplace))
     (local-file (file-relative-name
              temp-file
              (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(defun flymake-python-init () (flymake-pyflakes-init))
;(defun flymake-python-init () (flymake-pylint-init))
;(defun flymake-python-init () (flymake-pep8-init))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(add-hook 'python-mode-hook
      '(lambda ()
         (define-key python-mode-map (kbd "C-S-N") 'show-next-error)
         (define-key python-mode-map (kbd "C-S-P") 'show-previous-error)
         (define-key python-mode-map (kbd "C-S-D") 'credmp/flymake-display-err-minibuf)
         (define-key python-mode-map (kbd "C-S-F") 'flymake-mode)
         (flymake-mode 1)))


(provide 'flymake-python)
