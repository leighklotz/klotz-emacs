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
  (if (and (not (string-prefix-p "emacs" (downcase invocation-name)))
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
  (message (mapconcat 'identity (named-emacs-list-with-dates) "\n"))) 

(defun named-emacs-desktop-dirname (name)
  (concat (file-name-as-directory my-desktop-dirname) name))
  
(defun named-emacs-list ()
  (mapcan (lambda (entry)
	    (if (or (string= "." entry) (string= ".." entry))
		nil
	      (list entry)))
	  (directory-files my-desktop-dirname)))

(defun named-emacs-list-with-dates ()
  ;; like named-emacs-list but with dates
  (mapcar (lambda (entry)
            (let ((file (concat (file-name-as-directory my-desktop-dirname) entry)))
              (concat entry " " (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 (file-attributes file))))))
          (named-emacs-list)))


(defvar desktop-hacks-old-frame-title-format)
(when (or (not (listp frame-title-format))
	  (not (member '(:EVAL invocation-name) frame-title-format)))
  (setq desktop-hacks-old-frame-title-format frame-title-format)
  (setq frame-title-format
	(append
	 (if (listp frame-title-format) frame-title-format (list frame-title-format))
	 '(" [" (:EVAL invocation-name) "]"))))

(provide 'desktop-hacks)
