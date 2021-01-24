(defun hex-encode ()
  (interactive)
  (while
      (let ((p (point)))
        (and (re-search-forward "\\s-*\\([a-fA-F0-9][a-fA-F0-9]\\)" nil t)
             (= p (match-beginning 0))))
    (replace-match (char-to-string
                    (string-to-int (match-string 1) 16))
                   t t)))
