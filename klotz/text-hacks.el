;;;-*-EMACS-LISP-*-

(defun nodos ()
  (interactive)
  (set-buffer-file-coding-system 'iso-8859-1-unix))

(defun insert-date ()
  (interactive)
  (insert (save-excursion (shell-command "date") (set-buffer  "*Shell Command Output*") (goto-char (point-min)) (replace-string "\n" "") (buffer-string))))

(defun rational-call-last-kbd-macro (&optional PREFIX)
  (interactive "P")
  (let ((blink-matching-delay 0)
        (blink-matching-paren nil))
    (call-last-kbd-macro PREFIX)))

(global-set-key [(control ?x) ?e] 'rational-call-last-kbd-macro)

(defun notabs ()
  (interactive)
  (save-excursion
    (progn
      (message "Whitespace cleanup")
      (setq whitespace-check-buffer-indent nil) ;whitespace-check-buffer-indent is the opposite of notabs!
      (whitespace-cleanup))
    (progn
      (message "Untabify")
      (untabify (point-min) (point-max)))
    (progn
      (message "Indent")
      (goto-char (point-min))
      (search-forward "{")
      (backward-char 1)
      (c-indent-exp))))

(defun deampify()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "&lt;" "<")
    (goto-char (point-min))
    (replace-string "&gt;" ">")
    (goto-char (point-min))
    (replace-string "&amp;" "&")))

;;; Copyright (C) 1997, 1998 Thien-Thi Nguyen
(defun another-line ()
  "Copy line, preserving cursor column, and increment any numbers found.
This should probably be generalized in the future."
  (interactive)
  (let* ((col (current-column))
	 (bol (progn (beginning-of-line) (point)))
	 (eol (progn (end-of-line) (point)))
	 (line (buffer-substring bol eol)))
    (beginning-of-line)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-int (buffer-substring
				  (match-beginning 0) (match-end 0)))))
	(replace-match (int-to-string (1+ num)))))
    (beginning-of-line)
    (insert line "\n")
    (move-to-column col)))

;;; by Alex Schroeder
;;; Number regex improved by Charlie Hethcoat
(defun calculator-sum-column (start end)
  "Adds all integer, decimal, and floating-point numbers found in the
selected rectangle."
  (interactive "r")
  (save-excursion
    (kill-rectangle start end)
    (exchange-point-and-mark)
    (yank-rectangle)
    (set-buffer (get-buffer-create "*calc-sum*"))
    (erase-buffer)
    (yank-rectangle)
    (exchange-point-and-mark)
    (let ((sum 0))
      (while (re-search-forward
              "[-+]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?"
              nil t)
        ;; Examples of numbers it reads (nonexhaustive):  2 +2 -2
        ;; 2. +2. -2. 2.0 +2.0 -2.0 2e0 +2e0 -2e0 2E0 2e+0 2e-0,
        ;; 2.e0, 2.0e0, etc.
        (setq sum (+ sum (string-to-number (match-string 0)))))
      (message "Sum: %f" sum))))

;08.04.2003: Kai Groﬂjohann
(defun increment-number-at-point (amount)
  "Increment number at point by given AMOUNT."
  (interactive "pIncrement by: ")
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (old-num (number-at-point)))
    (unless old-num
      (error "No number at point"))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%d" (+ old-num amount)))))

;;;dedicated mode 13.04.2000
;;;http://www.xsteve.at/prg/emacs/xsteve-functions.el
(defvar dedicated-mode nil
  "Mode variable for dedicated minor mode.")
(make-variable-buffer-local 'dedicated-mode)

(defun dedicated-mode (&optional arg)
  "Dedicated minor mode."
  (interactive "P")
  (setq dedicated-mode (not dedicated-mode))
  (set-window-dedicated-p (selected-window) dedicated-mode)
  (if (not (assq 'dedicated-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(dedicated-mode " D")
                  minor-mode-alist))))

(defun fix-blank-lines ()
  "Eliminate lines of just whitespace and eliminate multiple newlines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "^[ 	]+$" "")
    (goto-char (point-min))
    (replace-regexp "
+" 
"
")))

;; I often want the whole buffer, and this makes the short name work.
(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(defun line-number-region () 
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (let ((i 0))
	(while (<= (point) (point-max))
	  (beginning-of-line 1)
	  (insert (format "%d " i))
	  (setq i (1+ i))
	  (next-line 1))))))


;;; https://www.blogbyben.com/2010/08/handy-emacs-function-url-decode-region.html
(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end) t)))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace a region with the same contents, only URL encoded."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))



(provide 'text-hacks)
