;;;-*-EMACS-LISP-*-
;;;
;;; Leigh L. Klotz, Jr. <klotz@graflex.org>
;;; Simple major mode for editing APL and running A+ interpreter.
;;;
;;; I hereby place this in the public domain.
;;;

;; Are we running XEmacs or Emacs?
(defvar running-xemacs )
(defvar aplus-mode-hook nil)

(defvar aplus-font-emacs "kaplcour")
(defvar aplus-font-xemacs 
  '(default ((t (:size "14pt" :family "Kapl"))) t))


(defun aplus-mode ()
  "Major mode for editing APLus code or running aplus.
To run, use \\[shell] to invoke a+ and then type \\[aplus-mode].
To edit, just use \\[aplus-mode].
Not yet a major mode, but getting close."
  (interactive)
  (aplus-mode-initialize)
  (aplus-mode-variables)
  (run-hooks 'aplus-mode-hook))

(defun aplus-mode-variables ()
  (make-local-variable 'comment-start)
  (setq comment-start "�")
  ; eventually do different fonts for input and output
  ; (make-local-variable 'font-lock-defaults)
  )

(defun aplus-mode-initialize ()
  (local-set-key "\ei" 'aplus-insert)
  (local-set-key "\eh" 'aplus-help)
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (custom-set-faces aplus-font-xemacs)
    (set-face-font 'default aplus-font-emacs)))

(defun aplus-insert ()
  "Insert apl character by name.
Type ? for a list of names"
  (interactive)
  (let ((name (completing-read "Name: " aplus-symbols nil t)))
    (if (not (equal name ""))
	(let ((val (cadr (assoc name aplus-symbols))))
	  (if val (insert val))))))

(defun aplus-help ()
  "Describe apl character.  
Starts with character at cursor, or you can type in a name of a character."
  (interactive)
  (let* ((str (and (>= (point-max) (1+ (point)))
		   (buffer-substring (point) (1+ (point)))))
	 (initial-name (do ((a aplus-symbols (cdr a)))
			   ((null a) nil)
			 (if (equal str (cadr (car a))) (return (car (car a))))))
	 (name (completing-read "Name: " aplus-symbols nil t initial-name))
	 (def (assoc name aplus-symbols)))
    (message (concat "init: " initial-name
	      (if (equal name "") (concat str " ") "")
	      (or (and (nth 0 def) (concat "(" (nth 0 def) ") ")) "")
	      (or (and (nth 2 def) (concat "Monadic: " (nth 2 def) " ")) "")
	      (or (and (nth 3 def) (concat "Dyadic: "  (nth 3 def) " ")) "")
	      (or (nth 4 def) "")))))

(defvar aplus-symbols
'(
; C	NAME		MONADIC FUNCTION	DYADIC FUNCTION		NOTES
("high-bar"	"�"	nil			nil			"Negative Number")
("bang"		"!"	"Item Ravel"   		"Restructure"		nil)
("number-sign"	"#"	nil			"Choose")
("dollar"	"$"	nil			nil "For system commands - abandon current function execution")
("percent"	"%"	"Value"			"Value in Context")
("ampersand"	"&"	"Stack References"	nil)
("asterisk"	"*"	"Exponential"		"Power")
("plus"		"+"	"Identity"		"Add")
("comma"	","	"Ravel"			"Catenate")
("mid-bar"	"-"	"Negate"		"Minus")
("dot"		"."	nil			"Inner Product"		"The derived function is Dyadic")
("slash"	"/"	nil			"Compress"		"MONADIC OPERATOR, DERIVING A DYADIC FUNCTION Reduce")
("colon"	":"	nil			nil			"used to create Defined Functions - used to create Dependencies")
("less-than"	"<"	"Box / Enclose"		"Less than")
("less-equal"   "�"     "Grade Up"                 "Less than or equal to")
("equals"	"="	nil			"Equals")
("greater-than"	">"	"Unbox / Disclose"	"Greater than")
("query"	"?"	"Roll"			"Deal")
("at"		"@"	"Rank"			nil			"The derived function may be Monadic or Dyadic")
("backslash"	"\\"	"Expand"          	nil			"MONADIC OPERATOR, DERIVING A DYADIC FUNCTION Scan")
("caret"	"^"	"Stop"			"And")
("back-quote"	"`"	nil			nil			"Used to create symbols")
("verticalbar"	"|"	"Absolute Value"	"Residue")
("tilde"	"~"	"Not"			nil)
("greater-equal""�"	"Stop"			"Greater than or equal to")
("not-equal"	"�"	nil			"Not equal to")
("down-caret"	"�"	"Type"			"Cast (symbol as left argument) Or   (boolean)")
("cross"	"�"	"Sign"			"Times")
("domino"	"�"	"Matrix Inverse"	"Solve")
("dotted-del"	"�"	"Bitwise"   		nil			"The derived function may be Monadic or Dyadic")
("tri-bar"	"�"	"Depth"			"Match")
("up-tack"	"�"	"Pack"			"Decode Base")
("downstile"	"�"	"Floor"			"Min")
("epsilon"	"�"	"Rake"			"Member")
("iota"		"�"	"Interval"		"Find Index of")
("jot"		"�"	nil			nil			"PLACEHOLDER FOR DYADIC OPERATOR Used with Dot for Outer Product The derived function is Dyadic")
("down-tack"	"�"	"Unpack"		"Encode Representation")
("circle"	"�"	"Pi times"		"Circle")
("rho"		"�"	"Shape"			"Reshape")
("upstile"	"�"	"Ceiling"		"Max")
("down-arrow"	"�"	"Print"			"Drop")
("down-shoe"	"�"	"Separate Symbols"	"Combine Symbols")
("right-shoe"	"�"	"Raze"			"Pick")
("up-arrow"	"�"	"Signal"		"Take")
("left-shoe"	"�"	"Partition Count"	"Partition")
("right-tack"	"�"	"Right"			nil)
("left-tack"	"�"	"null"			"Left")
("divide"	"�"	"Reciprocal"		"Divide")
("i-beam"	"�"	"Map In"		"Map")
("hydrant"	"�" 	"Execute"		"Protected Execute")
("lamp"		"�"	nil			nil			"Used for comments")
("del-stile"	"�"	"Grade Down"		nil)
("delta-stile"	"�"	"Grade Up"		"Bins")
("thorn"	"�" 	"Default Format"	"Format")
("circle-star"	"�"	"Natural Log"		"Log")
("circle-backslash"	"�" "Transpose"		"Transpose Axes")
("circle-stile"	"�"	"Reverse"		"Rotate")
("left-arrow"	"�"	"execution"          	"Assignment")
("right-arrow"	"�"	nil			nil			"current function execution - clear most-recent suspension")
("underscore"	"_"	nil			nil			" used as a separator in names")
("semicolon"	";"     nil			nil			"Statement Separator:  - used to separate multiple statements on one line - used to end statements in defined functions - used to separate elements for nested arrays - used to separate dimensions within bracket indexing")

))
