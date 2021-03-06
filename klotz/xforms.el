;;;-*-EMACS-LISP-*-
;;; Leigh L. Klotz, Jr. <klotz@graflex.org>
;;;
(defvar xforms-elements '("model" "instance" "submission" "bind" "label" "hint" "help" "alert" "extension" "choices" "value" "item" "itemset" "copy" "filename" "mediatype" "input" "textarea" "secret" "upload" "select1" "select" "range" "trigger" "output" "submit" "action" "dispatch" "rebuild" "revalidate" "recalculate" "setfocus" "load" "setvalue" "send" "reset" "insert" "delete" "setindex" "toggle" "message" "repeat" "group" "switch" "case"))
(defvar xforms-namespace "http://www.w3.org/2002/xforms")
(defvar xforms-namespace-prefix "xf")
(defvar xhtml-elements '("html"))
(defvar xhtml-namespace "http://www.w3.org/1999/xhtml")
(defvar xhtml-namespace-prefix nil)
(defvar xml-declaration "<?xml version=\"1.0\"?>\n")
(defvar xsd-namespace "http://www.w3.org/2001/XMLSchema")
(defvar xsd-namespace-prefix "xsd")
(defvar xsd-elements '("schema"))
(defvar ev-namespace "http://www.w3.org/2001/xml-events")
(defvar ev-namespace-prefix "ev")
(defvar ev-elements nil)


(defun xformify-buffer ()
  "Given a buffer that contains XForms elements without namespace prefixes on them, put in the namespace 
Beware element name conflicts in inline instance data (label, value, etc.)"
  (interactive)
  (goto-char (point-min))
  (if (not (looking-at (regexp-quote "<?xml")))
      (insert xml-declaration))
  (namespaceify-buffer xforms-namespace xforms-namespace-prefix xforms-elements)
  (namespaceify-buffer xhtml-namespace xhtml-namespace-prefix xhtml-elements)
  (namespaceify-buffer xsd-namespace xsd-namespace-prefix xsd-elements)
  (namespaceify-buffer ev-namespace ev-namespace-prefix ev-elements))

(defun namespaceify-buffer (namespace namespace-prefix elements)
  "Given a buffer that contains XForms elements without namespace prefixes on them, put in the namespace 
Beware element name conflicts in inline instance data (label, value, etc.)"
  (interactive)
  ;; See if there is a namespace prefix for XForms already.
  ;; If not, add one named namespace-prefix.  If so, use it.
  (let ((prefix namespace-prefix))
    (if (null namespace-prefix)
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward (concat "xmlns=\\([^= \t'\"<>]+\\)=['\"]" (regexp-quote namespace) "['\"]") nil t)
	      (setq prefix (match-string 1))
	    (goto-char (point-min))
	    (re-search-forward "<[^ ?!>\t]+[ \t>]" nil t)
	    (backward-char 1)
	    (insert (format " xmlns=\"%s\"" namespace))))
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward (concat "xmlns:\\([^= \t'\"<>]+\\)=['\"]" (regexp-quote namespace) "['\"]") nil t)
	      (setq prefix (match-string 1))
	    (goto-char (point-min))
	    (re-search-forward "<[^ ?!>\t]+[ \t>]" nil t)
	    (backward-char 1)
	    (insert (format " xmlns:%s=\"%s\"" namespace-prefix namespace)))))
    (save-restriction
      (dolist (element elements)
	(let ((case-replace nil))
	  (goto-char (point-min))
	  (while (re-search-forward (concat "<" element "\\(\\>\\)") nil t)
	    (replace-match (concat "<" (if prefix (concat prefix ":") "") element "\\1") nil nil))
	  (goto-char (point-min))
	  (while (re-search-forward(concat "</" element ">") nil t)
	    (replace-match (concat "</" (if prefix (concat prefix ":") "") element ">") nil nil)))))))

