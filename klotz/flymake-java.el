(require 'flymake)

(defvar flymake-java-automagic-find-file t)

(defconst ecj-jar-path "/home/klotz/emacs/ecj.jar")

(defvar flymake-java-version "1.6")
(setq flymake-java-jdk-lib "/usr/lib/jvm/java-6-openjdk/lib/")

(defvar flymake-java-classpath ".")

(defvar flymake-java-classpath)
(defvar flymake-java-temp-classes-directory)
(defvar flymake-java-subsystem)

(make-variable-buffer-local 'flymake-java-classpath)
(make-variable-buffer-local 'flymake-java-temp-classes-directory)
(make-variable-buffer-local 'flymake-java-subsystem)

(defvar flymake-java-version)

(defvar flymake-java-jdk-lib)

(defvar flymake-java-classpaths nil)
(defvar flymake-java-temp-classes-dirs nil)
(defvar flymake-java-tree-dirs nil)

(defvar flymake-java-warning-flags
  "conditionAssign,dep-ann,emptyBlock,enumIdentifier,enumSwitch,fallthrough,finalBound,localHiding,fieldHiding,typeHiding,maskedCatchBlock,hashCode,includeAssertNull,javadoc,null,over-ann,paramAssign,semicolon,static-access,syncOverride,syntheticAccess,unnecessaryElse,unused,uselessTypeCheck")

(defun flymake-setup-java (subsystem)
  (interactive (list (completing-read "Subsystem: " (mapcar 'car flymake-java-classpaths))))
  ;; pushd amber; ant show_classpath_for_tomcat6.0 -Dtomcat6.0=true  -emacs -Dtomcat.home=/home/klotz/ds/trunk/DocuShare3//DocuShare3/dist/tomcat; popd
  (setq flymake-java-subsystem subsystem)
  (setq flymake-java-temp-classes-directory (flymake-java-get-temp-classes-dir subsystem))
  (setq flymake-java-classpath 
        (concat
         ;; add this first 
         flymake-java-temp-classes-directory ":"
         ;(flymake-stopgap-classpaths)
         (flymake-java-get-classpath subsystem)))
  (message subsystem)
  nil)

(defun flymake-java-setup-p ()
  (and flymake-java-subsystem
       (and flymake-java-temp-classes-directory (file-accessible-directory-p flymake-java-temp-classes-directory))
       flymake-java-classpath))


(defmacro flymake-add-to-alist (module-name value alist)
  `(let ((a (assoc ,module-name ,alist)))
     (if (not (null a))
         (setf (cdr a) ,value)
       (push (cons ,module-name ,value) ,alist))))

;; pass  dist/classes as classes-subdir.  assumes tree-dir/classpath.txt
;; todo -- make flymake-setup-java re-read classpath.txt file.
(defun def-flymake-java-module-automatic (module-name tree-dir classes-subdir)
  (let ((classpath-txt (concat tree-dir "/classpath.txt")))
    (if (file-exists-p classpath-txt)
        (def-flymake-java-module
          module-name
          tree-dir
          (concat tree-dir "/" classes-subdir)
          (read-classpath-file classpath-txt)))))

(defun def-flymake-java-module (module-name tree-dir temp-dir classpath)
  (if (equal path-separator ":")
      (setq classpath (flymake-replace-regexp-in-string ";" ":" classpath)))
  (flymake-add-to-alist module-name classpath flymake-java-classpaths)
  (flymake-add-to-alist module-name temp-dir flymake-java-temp-classes-dirs)
  (flymake-add-to-alist module-name tree-dir flymake-java-tree-dirs)
  nil)

;; reads specified classpath.txt file and removes trailing "dist/classes" if present.
;; does not change path-separator character
(defun read-classpath-file (filename) 
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (if (re-search-forward ";dist/classes$" nil t)
        (replace-match "" nil nil))
    (flymake-replace-regexp-in-string "\n+$" "" (buffer-string))))

(defun flymake-java-get-classpath (subsystem)
  (or (cdr (assoc subsystem flymake-java-classpaths)) ""))


(defun flymake-java-ecj-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-ecj-create-temp-file))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "java" (list "-jar" ecj-jar-path "-Xemacs" "-d" "none" ; "-warn:none"
		       "-source" flymake-java-version "-target" flymake-java-version "-proceedOnError"
		       "-classpath" flymake-java-classpath
					; "-log" "log.xml"
		       local-file))))

(defun flymake-java-ecj-cleanup ()
  "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))

(defun flymake-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory."
  (file-truename (expand-file-name (file-name-nondirectory file-name)
				   (expand-file-name (int-to-string (abs (random))) (flymake-get-temp-dir)))))

(push '(".+\\.java$" flymake-java-ecj-init flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)

(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n"
	1 2 nil 2 3 (6 compilation-error-face))
      compilation-error-regexp-alist)

(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n"
	1 2 nil 1 3 (6 compilation-warning-face))
       compilation-error-regexp-alist)

(defun flymake-goto-next-error-and-show ()
  "Goes to the next error/warning and displays it in the minibuffer"
  (interactive)
  (flymake-goto-next-error)
  (credmp/flymake-display-err-minibuf))

(defun flymake-goto-prev-error-and-show ()
  "Goes to the previous error/warning and displays it in the minibuffer"
  (interactive)
  (flymake-goto-prev-error)
  (credmp/flymake-display-err-minibuf))

(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))


(defun my-current-line-error-info () 
  (let* ((result "")
         (line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (setq result (concat result text (if (> count 1) "\n" "")))))
      (setq count (1- count)))
    result))




(defun remove-unused-imports ()
  (interactive)
  (flymake-stop-all-syntax-checks)
  (let ((n (catch 'done
             (save-excursion
               (let ((count 0))
                 (while t
                   (cond ((looking-at "^\\s *$")
                          (forward-line 1))
                         ((looking-at "^import[ \t]")
                          (let ((line-errors (my-current-line-error-info)))
                            (if (and line-errors (string-match "warning: The import [a-zA-Z0-9_.]+ is never used" line-errors))
                                (progn
                                  (setq count (1+ count))
                                  (beginning-of-line 1)
                                  (insert "//*DELETE=")
                                  (beginning-of-line 2))
                              (forward-line 1))))
                         (t (throw 'done count)))))))))
    ;; must do in two passes because flymake uses line number, which changes
    (save-excursion
      (replace-regexp-noninteractive "^//\\*DELETE=import\\s .*\n" ""))        
    (message "%d unused imports removed" n)))
    
(defun replace-regexp-noninteractive (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun flymake-java-find-file-hook () 
  (when (and flymake-java-automagic-find-file buffer-file-name (eq major-mode 'java-mode))
    (let ((best (flymake-java-find-best-module buffer-file-name)))
      (if (not (null best))
          (progn
            (flymake-setup-java best)
            (flymake-mode))))))

(defun flymake-java-find-best-module (filename) 
  (let ((max 0)
        (best nil))
    (dolist (node flymake-java-tree-dirs)
      (let* ((mode (car node))
             (dir (cdr node))
             (mode-substring-regexp (concat "^" (regexp-quote dir))))
        (if (> (length dir) max)
            (if (string-match mode-substring-regexp buffer-file-name)
                (setq max (length dir)
                      best mode)))))
    best))

;; overwrite this function from flymake.el because if it's too long it makes too big a dialog box and crashes
(defun flymake-display-warning (warning)
  "Display a warning to user."
  (if (> (length warning) 128)
      (message-box (concat (substring warning 0 128) "..."))
    (message-box warning)))


(defun my-java-flymake-mode ()
  "Call [flymake-mode] unless the classpath is not yet set up, in which case call [flymake-setup-java]"
  (interactive)
  (if (flymake-java-setup-p)
      (flymake-mode)
    (let ((best (flymake-java-find-best-module buffer-file-name)))
      (cond ((not (null best))
             (flymake-setup-java best)
             (flymake-mode))
            (t (call-interactively 'flymake-setup-java))))))

(add-hook 'find-file-hook 'flymake-java-find-file-hook)

(provide 'flymake-java)
