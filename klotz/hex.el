;;; -*-EMACS-*-

(defun hex-string-to-ascii (string) 
  (do ((new (make-string (/ (length string) 2) 0))
       (i 0 (+ i 1))
       (len (/ (length string) 2)))
      ((= i len) new)
    (setf (aref new i)
	    (+ (* 16 (hexify (aref string (* i 2))))
	            (hexify (aref string (+ 1 (* i 2))))))))

(defun hexify (ascii) 
  (cond ((< ascii ?0) (error "digit %s not understood" ascii))
	((<= ascii ?9) (- ascii ?0))
	((<= ascii ?F) (- ascii (- ?A 10)))
	((<= ascii ?f) (- ascii (- ?a 10)))
	(t (error "digit %s not understood" ascii))))

(defun hex-encode ()
  (interactive)
  (while
      (let ((p (point)))
        (and (re-search-forward "\\s-*\\([a-fA-F0-9][a-fA-F0-9]\\)" nil t)
             (= p (match-beginning 0))))
    (replace-match (char-to-string
                    (string-to-int (match-string 1) 16))
                   t t)))

(provide 'hex)
