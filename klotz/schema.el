;;;-*-EMACS-LISP-*-
;;; Leigh L. Klotz, Jr. <klotz@graflex.org>

(defvar java-executable "c:/j2sdk1.4.2/bin/java")
(defvar xerces-classpath "k:/java/xerces-2_6_2/xercesImpl.jar\;k:/java/xerces-2_6_2/xercesSamples.jar")
(defun validate-schema (schema-file instance-file)
  (interactive "FSchema File: 
FInstance File (erase if none): ")
  (setq a schema-file b instance-file)
  (let ((compilation-buffer-name-function 'validate-schema-buffer-name))
    (compile (concat java-executable " "
		     "-classpath " xerces-classpath " "
		     "xni.XMLGrammarBuilder" " "
		     "-a " (expand-file-name schema-file) " "
		     (if (> (length instance-file) 0) (concat "-i " (expand-file-name instance-file)) "")))))

(defun validate-schema-buffer-name (mode) "*validate-schema*")

(defun validate-schema-compilation-finish-function (buffer how)
  (if (equal (buffer-name buffer) (validate-schema-buffer-name nil))
      (save-excursion
	(switch-to-buffer buffer)
	(goto-char (point-min))
	(replace-string "[Error] " "")
	(goto-char (point-min))
	(replace-string "[Fatal Error] " ""))))

(add-hook 'compilation-finish-functions 'validate-schema-compilation-finish-function)

(provide 'schema)
