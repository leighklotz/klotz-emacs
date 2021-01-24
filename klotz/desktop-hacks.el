;;;
;;; Desktop save - multiple emacs session management w/autosave and restore
;;;
;;; Ties invocation-name to deskop-save mode directory name under
;;; ~/.emacs.d/.sessions/{desktop-dirname}
;;; - Make sure desktop-globals-to-save  doesn't have kill-ring since that
;;; might save secrets.
;;;

(defvar my-desktop-dirname "~/.emacs.d/.sessions/")
(defvar desktop-dirname nil)
(defvar desktop-path nil)

(defun name-this-emacs (new-invocation-name)
  (interactive "sPersist emacs name: ")
  (if (and (not (string= invocation-name "emacs"))
	   (not (string= invocation-name new-invocation-name)))
      (error "invocation-name is already set to %s" invocation-name))
  (let ((new-desktop-dirname (named-emacs-desktop-dirname new-invocation-name)))
    (make-directory new-desktop-dirname)
    (setq desktop-dirname new-desktop-dirname)
    (setq invocation-name new-invocation-name)
    (setq desktop-path (list desktop-dirname))))

(defun save-this-emacs ()
  (interactive)
  (if (string= "emacs" invocation-name)
      (error "invocation-name emacs is default and shouldn't be saved
Use \[M-X name-this-emacs\] first"))
  (desktop-save-mode 1)
  (desktop-save desktop-dirname))

(defun restore-named-emacs (new-invocation-name)
  (interactive
   (list
    (completing-read "emacs name: " (named-emacs-list))))
  (if (string= invocation-name new-invocation-name)
      (error "invocation-name is already set to %s" invocation-name))
  (if (string= "emacs" new-invocation-name)
      (error "invocation-name emacs is default and shouldn't be saved"))
  (setq invocation-name new-invocation-name)
  (desktop-change-dir (named-emacs-desktop-dirname new-invocation-name))
  (desktop-save-mode 1))

;;; You might want to do restore-named-emacs and autocomplete instead
(defun list-named-emacs ()
  (interactive)
  (message (mapconcat 'identity (named-emacs-list) "\n"))) 

(defun named-emacs-desktop-dirname (name)
  (concat (file-name-as-directory my-desktop-dirname) name))
  
(defun named-emacs-list ()
  (mapcan (lambda (entry)
	    (if (or (string= "." entry) (string= ".." entry))
		nil
	      (list entry)))
	  (directory-files my-desktop-dirname)))

(provide 'desktop-hacks)
