;     etach is an Emacs extension for handling MIME mail.
;     Copyright (C) 2000 John M. Rulnick

;     This program is free software; you can redistribute it and/or
;     modify it under the terms of the GNU General Public License as
;     published by the Free Software Foundation; either version 2 of
;     the License, or (at your option) any later version.

;     This program is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.

;     You should have received a copy of the GNU General Public
;     License along with this program; if not, write to the Free
;     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;     MA 02111-1307 USA

;     Contact: John M. Rulnick, PO Box 299, Charlton, MA 01507-0299
;     USA, email: etach at rulnick dot com

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;			   This is etach.el
;			   Version number:
		    (defvar etach-version "1.1.4")
;			   Date: 2000-07-19

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following macro is for compatibility with Emacs versions that do
; not have defcustom:

(defmacro etach-defcustom (a b c &rest d)
  (if (or (string-match "^[0-9][.]" emacs-version)
	  (string-match "^1[0-9][.]" emacs-version))
      (list 'defvar a b c)
    (append (list 'defcustom a b c) d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Please set the variable "etach-debug" to "t" if you need to send a
; bug report or have problems using etach:
;
;  M-x set-variable RET etach-debug RET t RET
;
; This will create a buffer called "etach-debug" to which a transcript
; of etach's subsequent operation will be sent.

(etach-defcustom etach-debug nil
  "*Set etach-debug to t to write etach troubleshooting messages to a
buffer named \'etach-debug\'."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If your version of Emacs doesn't have base64-encode-region and
; base64-decode-region then you should set "etach-use-mimencode" to
; "t" by putting the following (or equivalent custom-set-variables
; entry) in your startup file:
;
;  (setq etach-use-mimencode t)
;
; You will then need to have a working "mimencode" command on your
; system.  The mimencode utility is freely and widely available.

(etach-defcustom etach-use-mimencode nil
  "*Set etach-use-mimencode to t to use the external command
\"mimencode\" instead of Emacs and etach native base64- and
quoted-printable- encoders and decoders.  Setting to t is not
necessary unless calls to the encode or decode functions generate
errors that indicate that the functions are unavailable in your
version of Emacs."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Put this line:
;
;  (setq etach-prompt-me-for-file-names t)
;
; (or equivalent custom-set-variables entry) into your startup file if
; you want etach to ask you to confirm file names for detachments.

(etach-defcustom etach-prompt-me-for-file-names nil
  "*Set etach-prompt-me-for-file-names to t to be prompted for
detachment file names when using the detach function.  This also
permits cancellation of individual detachments with C-g.  Leaving this
variable set to nil means detachments can proceed with default file
names and, in general, no additional user intervention."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Put this line:
;
;  (setq etach-adjust-decoded-plain-text nil)
;
; (or equivalent custom-set-variables entry) into your startup file if
; you DO NOT want etach to fill (via the command fill-region) areas of
; base64-encoded or quoted-printable-encoded plain text that are
; displayed in place after decoding.

(etach-defcustom etach-adjust-decoded-plain-text t
  "*Set etach-adjust-decoded-plain-text to nil to make etach NOT fill
encoded regions of plain text viewed in place after decoding, and NOT
replace their control-M characters with newlines.  In other words, set
this variable to nil to leave the decoded text exactly as it was sent,
instead of trying to make it more pleasant for viewing."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Put this line:
;
;  (setq etach-detachment-default-directory "/your/preferred/directory/")
;
; (or equivalent custom-set-variables entry) into your startup file if
; you want etach to put detachments into /your/preferred/directory/
; instead of the "detached/" subdirectory of the RMAIL buffer's
; current working directory.

(etach-defcustom etach-detachment-default-directory "detached/"
 "*Set etach-detachment-default-directory to a string containing your
preferred directory path for detachments if you prefer not to use the
default.  The default is to place detachments in a subdirectory named
\"detached\" of the current directory.  Etach will attempt to create
the directory if it doesn't already exist."
 :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Put this line:
;
;  (setq etach-restore-buffer-after-detach t)
;
; (or equivalent custom-set-variables entry) into your startup file if
; you want etach to simply detach any attached files and then restore
; the contents of the current RMAIL message when you invoke the detach
; command.  This can also be set via M-x set-variable RET, of course.
; See also etach-restore-attachments-after-detach.

(etach-defcustom etach-restore-buffer-after-detach nil
  "*Set etach-restore-buffer-after-detach to t to make etach replace
the (new) contents of your RMAIL buffer entirely with the original
(pre-detach) contents.  This applies when you invoke the detach
command."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Put this line:
;
;  (setq etach-restore-attachments-after-detach t)
;
; (or equivalent custom-set-variables entry) into your startup file if
; you want etach to restore the original content of any MIME parts
; that were written to files.  This can also be set via M-x
; set-variable RET, of course.  See also
; etach-restore-buffer-after-detach.

(etach-defcustom etach-restore-attachments-after-detach nil
  "*Set etach-restore-attachments-after-detach to t to make etach
restore the original content of any MIME parts that were written to
files.  This allows you to use detach to decode inline
quoted-printable- or base64-encoded text and effectively copy any
attached files out to disk, but otherwise not change the content of
the current message.  It makes sense to leave this set to nil if you
have etach-restore-buffer-after-detach set to t, since that setting
will cause the entire buffer to be restored."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Customize the variable etach-spam-message via setq or
; custom-set-variables if you would like to use a different text
; message when you forward spam for handling by email postmasters and
; the like.

(etach-defcustom etach-spam-message
  "I have received unsolicited commercial or bulk email (spam) from, or
alleging to be from, your domain.  I would appreciate if you would
take steps to prevent this from happening in the future.

Any claims made by the sender that the message was requested by me are
false.  I have never directly or indirectly (by subscription, opt-in,
non-opt-out, or any other means) requested any such contact from the
sender.

The full content of the offending email message, including mail
transport headers, is attached below.

Thank you for your time and attention."
  "*Message to insert into spam reports."
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You should definitely customize the variable
; etach-spam-host-exclude-list via setq or custom-set-variables by
; adding more strings to the etach-spam-host-exclude-list if you plan
; to use the spam-handling functionality of etach-mime-forward.  The
; strings to add should include your own host and domain; for example:
;
; (setq etach-spam-host-exclude-list 
;       '("localhost" "mydomain.com" "myhost.mydomain.com"))
;
; Use lowercase name strings.

(etach-defcustom etach-spam-host-exclude-list '("localhost")
  "*Specific hosts or domains to skip when sending a spam report."
  :type 'sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Set the variable etach-spam-message-subject-string via setq or
; custom-set-variables if you prefer a different default "Subject:"
; line preface to be included in your spam reports.

(etach-defcustom etach-spam-message-subject-string "mail abuse report"
  "*Subject line preface to use when sending a spam report."
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Immediately following is a list of associations for file name
; extensions and MIME types.  Add entries to the list as you like, but
; be cognizant of the specifications in RFCs 2045-9 (see
; http://www.cis.ohio-state.edu/htbin/rfc/rfc-index.html).
;
; Also, change the order within the groups of entries having the same
; MIME type if you wish.  The first entry provides the default file
; name extension.  For example, if "txt" is the first with association
; "text/plain" then ".txt" will be the default extension for detached
; files of type text/plain.

(defvar etach-mime-type-alist '(

  ("txt"  "text/plain")
  ("asc"  "text/plain")
  ("text" "text/plain")

  ("html" "text/html")
  ("htm"  "text/html")

  ("aif"  "audio/x-aiff")
  ("aifc" "audio/x-aiff")
  ("aiff" "audio/x-aiff")

  ("cdf" "application/x-netcdf")
  ("nc"  "application/x-netcdf")

  ("exe" "application/x-msdos-program")
  ("bat" "application/x-msdos-program")
  ("com" "application/x-msdos-program")

  ("ics" "text/calendar")
  ("ifb" "text/calendar")

  ("jpg"  "image/jpeg")
  ("jpe"  "image/jpeg")
  ("jpeg" "image/jpeg")

  ("ltx"   "application/x-latex")
  ("latex" "application/x-latex")

  ("mid"  "audio/midi")
  ("midi" "audio/midi")

  ("mov" "video/quicktime")
  ("qt"  "video/quicktime")

  ("mpg"  "video/mpeg")
  ("mp2"  "video/mpeg")
  ("mpe"  "video/mpeg")
  ("mpeg" "video/mpeg")

  ("pfb" "application/x-font")
  ("gsf" "application/x-font")
  ("pfa" "application/x-font")

  ("php"   "application/x-httpd-php")
  ("pht"   "application/x-httpd-php")
  ("phtml" "application/x-httpd-php")

  ("pl" "application/x-perl")
  ("pm" "application/x-perl")

  ("ps"  "application/postscript")
  ("eps" "application/postscript")

  ("ram" "audio/x-pn-realaudio")
  ("ra"  "audio/x-pn-realaudio")
  ("rm"  "audio/x-pn-realaudio")

  ("roff" "application/x-troff")
  ("t"    "application/x-troff")
  ("tr"   "application/x-troff")

  ("texi" "application/x-texinfo")
  ("texinfo" "application/x-texinfo")

  ("tif"  "image/tiff")
  ("tiff" "image/tiff")

  ("uri" "text/uri-list")
  ("uris" "text/uri-list")

  ("xml" "text/xml")
  ("dtd" "text/xml")

  ("au" "audio/ulaw")
  ("avi" "video/x-msvideo")
  ("bcpio" "application/x-bcpio")
;  ("bin" "application/octet-stream") ; leave this commented unless you want a default of ".bin"
  ("bmp" "application/x-ms-bmp")
  ("c" "text/x-csrc")
  ("cgm" "image/cgm")
  ("cpio" "application/x-cpio")
  ("csh" "application/x-csh")
  ("css" "text/css")
  ("csv" "text/comma-separated-values")
  ("deb" "application/x-debian-package")
  ("doc" "application/msword")
  ("dvi" "application/x-dvi")
  ("etx" "text/x-setext")
  ("ez" "application/andrew-inset")
  ("g3fax" "image/g3fax")
  ("gif" "image/gif")
  ("gpg" "application/gnupg")
  ("gtar" "application/x-gtar")
  ("gz" "application/x-gunzip")
  ("hdf" "application/x-hdf")
  ("ief" "image/ief")
  ("man" "application/x-troff-man")
  ("mdb" "application/msaccess")
  ("me" "application/x-troff-me")
  ("mif" "application/x-mif")
  ("movie" "video/x-sgi-movie")
  ("ms" "application/x-troff-ms")
  ("naplps" "image/naplps")
  ("o" "application/x-object")
  ("oda" "application/oda")
  ("pbm" "image/x-portable-bitmap")
  ("pdf" "application/pdf")
  ("pgm" "image/x-portable-graymap")
  ("pgn" "application/x-chess-pgn")
  ("pgp" "application/pgp")
  ("php3" "application/x-httpd-php3")
  ("php3p" "application/x-httpd-php3-preprocessed")
  ("phps" "application/x-httpd-php3-source")
  ("png" "image/png")
  ("pnm" "image/x-portable-anymap")
  ("ppm" "image/x-portable-pixmap")
  ("ppt" "application/powerpoint")
  ("ras" "image/x-cmu-raster")
  ("rgb" "image/x-rgb")
  ("rtf" "application/rtf")
  ("rtx" "text/richtext")
  ("sgml" "text/sgml")
  ("sh" "application/x-sh")
  ("shar" "application/x-shar")
  ("snd" "audio/basic")
  ("sv4cpio" "application/x-sv4cpio")
  ("sv4crc" "application/x-sv4crc")
  ("tar" "application/x-tar")
  ("tcl" "application/x-tcl")
  ("tex" "application/x-tex")
  ("tgz" "application/x-gtar")
  ("tsv" "text/tab-separated-values")
  ("ustar" "application/x-ustar")
  ("wav" "audio/x-wav")
  ("wpd" "application/x-wordperfect")
  ("xbm" "image/x-xbitmap")
  ("xls" "application/excel")
  ("xpm" "image/x-xpixmap")
  ("xwd" "image/x-xwindowdump")
  ("zip" "application/zip")

  )
  "Association list of file name extensions and MIME types recognized 
by etach.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; USERS SHOULD BE PARTICULARLY CAUTIOUS MAKING CHANGES BEYOND THIS POINT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'attach 'etach-attach)
(defalias 'detach 'etach-detach)
(defalias 'mime-forward 'etach-mime-forward)
(defalias 'quoted-printable-encode-region 'etach-quoted-printable-encode-region)
(defalias 'kill-label-detached 'etach-kill-label-detached)

(defvar etach-content-type)
(defvar etach-content-transfer-encoding)
(defvar etach-content-id)
(defvar etach-content-description)
(defvar etach-content-disposition)
(defvar etach-boundary)

; Here are the interactive (user-visible) functions:

(defun etach-version ()
  "Show the etach version number."
  (interactive)
  (message (concat "etach version " etach-version)))

(defun etach-attach (file-to-attach)
  "Attach a file to the present outgoing email message.  Use in Mail mode."
  (interactive "fFile to attach: ")
  (etach-debug-msg (concat "========== attach called with file \'" file-to-attach "\' =========="))
  (save-excursion
    (let ((etach-content-type "")
	  (etach-content-transfer-encoding "")
	  (etach-content-id "")
	  (etach-content-description "")
	  (etach-content-disposition "")
	  (case-fold-search t))
      (etach-mime-validate-minimal-headers)
      (goto-char (point-min))
      (etach-mime-get-content-headers)
      (let ((etach-boundary ""))
	(etach-prep-outgoing-mime-message)
	(insert "Content-Type: ")
	(let ((lext ""))
	  (if (string-match "\\.\\([a-zA-Z0-9]+\\)$" file-to-attach)
	      (setq lext (match-string 1 file-to-attach)))

	  (if (assoc lext etach-mime-type-alist)
	      (progn
		(insert (car (cdr (assoc lext etach-mime-type-alist))))
		(if (string-match "text/" (car (cdr (assoc lext etach-mime-type-alist))))
		    (insert "; charset=us-ascii")))
	    (insert "application/octet-stream")))
	(let ((name-sans-path file-to-attach))
	  (if (string-match "/\\([^/]*\\)$" file-to-attach) ; could use file-name-nondirectory
	      (setq name-sans-path (match-string 1 file-to-attach)))
	  (insert (if (> (length name-sans-path) 22)
		      ";\n\t"
		    "; ")
		  "name=\"" name-sans-path "\"\nContent-Transfer-Encoding: "
		  (if (string-match "\.te?xt$" file-to-attach)
		      "quoted-printable\n\n"
		    "base64\n\n")))
	(insert-file-contents-literally file-to-attach)
	(if (string-match "\.te?xt$" file-to-attach)
	    (progn
	      (if etach-use-mimencode
		  (shell-command-on-region (point) (point-max) "mimencode -q" t t)
		(etach-quoted-printable-encode-region (point) (point-max)))
	      (goto-char (point-max)))
	  (if etach-use-mimencode
	      (shell-command-on-region (point) (point-max) "mimencode -b" t t)
	    (base64-encode-region (point) (point-max)))
	  (goto-char (point-max))
	  (or (bolp) (insert "\n")))
	(insert "--" etach-boundary "--\n")))))

(defun etach-detach ()
  "Detach the encoded attachments from the present email message.  Use in RMAIL mode."
  (interactive)
  (etach-debug-msg (concat "========== detach called =========="))
  (save-excursion
    ; if in the x-summary buffer, switch to the x buffer:
    (set-buffer (substring (buffer-name) 0 (string-match "\-summary$" (buffer-name))))
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (default-directory default-directory)
	  (require-final-newline require-final-newline)
	  (insert-default-directory insert-default-directory)
	  (copy-of-rmail-text (buffer-string))
	  (my-current-buffer (current-buffer)))
      (condition-case err
	  (progn
	    (etach-mime-detach "\\'")
	    (if etach-restore-buffer-after-detach
		(etach-restore-buffer "user preference" my-current-buffer copy-of-rmail-text)))
	((quit error)
	 (etach-restore-buffer (concat "error: " (error-message-string err))
			       my-current-buffer
			       copy-of-rmail-text))))))

(defun etach-mime-forward (&optional this-is-spam)
  "Use MIME message/rfc822 format to forward a message.  Use in RMAIL mode."
  (interactive "P")
  (if this-is-spam
      (etach-debug-msg (concat "========== mime-forward (spam) called =========="))
    (etach-debug-msg (concat "========== mime-forward called ==========")))
  ; if in the x-summary buffer, switch to the x buffer:
  (set-buffer (substring (buffer-name) 0 (string-match "\-summary$" (buffer-name))))
  (let ((msg-to-be-forwarded ""))
    (if this-is-spam
	(if (etach-rmail-msg-is-pruned)
	    (rmail-toggle-header)))
    (setq msg-to-be-forwarded (buffer-string))
    (if this-is-spam
	(if (not (etach-rmail-msg-is-pruned))
	    (rmail-toggle-header)))
    (rmail-set-attribute "forwarded" t)
    (rmail-mail)
    (delete-other-windows)
    (save-excursion
      (let ((etach-content-type "")
	    (etach-content-transfer-encoding "")
	    (etach-content-id "")
	    (etach-content-description "")
	    (etach-content-disposition "")
	    (case-fold-search t))
	(etach-mime-validate-minimal-headers)
	(goto-char (point-min))
	(etach-mime-get-content-headers)
	(let ((etach-boundary "")
	      (to-list '()))
	  (etach-prep-outgoing-mime-message)
	  (if this-is-spam
	      (save-excursion
		(goto-char (point-min))
		(re-search-forward "^$") ; first blank line should be text/plain "writing area"
		(forward-line)
		(insert (concat etach-spam-message "\n"))))
	  (insert "Content-Type: message/rfc822\nContent-Transfer-Encoding: 7bit\n\n")
	  (save-excursion
	    (insert msg-to-be-forwarded))
	  (if this-is-spam
	      (let ((host ""))
                ; first scan all email addresses appearing in message:
		(save-excursion
		  (while (re-search-forward "@\\([^ \]\t\n>\)\'\";,]+\\)" nil t)
		    (setq to-list (etach-extract-hosts (match-string 1) to-list))))
                ; now scan all "Received: from" header lines:
		(save-excursion
		  (while (re-search-forward "Received: from \\(.+\\)$" nil t)
		    (setq to-list (etach-extract-hosts (match-string 1) to-list))))
		(save-excursion
		  (goto-char (point-min))
		  (re-search-forward "^To: ")
		  (let ((host ""))
		    (while to-list
		      (setq host (car to-list))
		      (setq to-list (cdr to-list))
		      (insert
		       "abuse@" host ", postmaster@" host
		       (if to-list
			   ",\n\t"
			 "")))))))
	  (let ((sender "")
		(orig-subject ""))
	    (save-excursion
	      (re-search-forward "^From:[ \t]*\\(.*\\)$" nil t)
	      (setq sender (match-string 1))
	      (if (string-match "\<\\([^ ]+\\)\>" sender)
		  (setq sender (match-string 1 sender))
		(if (string-match "\\([^ ]+\\)[ \t]+\(.*\)" sender)
		    (setq sender (match-string 1 sender)))))
	    (save-excursion
	      (re-search-forward "^Subject: \\(.*\\)$" nil t)
	      (setq orig-subject (match-string 1)))
	    (save-excursion
	      (goto-char (point-min))
	      (re-search-forward "^Subject: ")
	      (if this-is-spam
		  (insert etach-spam-message-subject-string " "))
	      (insert "[" sender ": " orig-subject "]")))
	  (goto-char (point-max))
	  (or (bolp) (insert "\n"))
	  (insert "--" etach-boundary "--\n"))))))

(defun etach-quoted-printable-encode-region (rbeg rend)
  "Quoted-printable-encode region."
  (interactive "r")
  (etach-debug-msg (concat "quoted-printable-encode-region called"))
  (require 'font-lock)
  (let ((mime-bad-char		; all but ascii 33-60, 62-126, and \n
	 "[^\]!\"#\$%&'()\*\+,\./0-9;<:>\?@A-Z_^\[`a-z{|}~\\\n-]")
	(case-fold-search t)
	(my-global-font-lock-mode global-font-lock-mode))
    (global-font-lock-mode 0)
    (save-excursion
      (save-restriction
      (narrow-to-region rbeg rend)

      (goto-char (point-min))
      (while (re-search-forward mime-bad-char nil t)
	(cond
	 ((or (char-equal (preceding-char) ? )
	      (char-equal (preceding-char) ?\t))
	  (if (char-equal (following-char) ?\n)
	      (insert "=\n")))
	 (t 
	  (replace-match (format "=%02X" (preceding-char))))))

      (goto-char (point-min))
      (while (re-search-forward "^[.]" nil t)
	(replace-match "=2E" t t))

      (goto-char (point-min))
      (while (re-search-forward "^F\\(rom\\)" nil t)
	(beginning-of-line)
	(replace-match (concat (format "=%02X" (following-char)) (match-string 1)) t t))

      (goto-char (point-min))
      (while (re-search-forward		; look for 77+ chars on a line, split 73/4
	      "^\\(.........................................................................\\)\\(=[A-F0-9][A-F0-9].\\)" 
	      nil t)
	(replace-match (concat (match-string 1) "=\n" (match-string 2)) t t)
	(beginning-of-line))

      (goto-char (point-min))
      (while (re-search-forward		; look for 77+ chars on a line, split 74/3
	      "^\\(..........................................................................\\)\\(=[A-F0-9][A-F0-9]\\)" 
	      nil t)
	(replace-match (concat (match-string 1) "=\n" (match-string 2)) t t)
	(beginning-of-line))

      (goto-char (point-min))
      (while (re-search-forward		; look for 77+ chars on a line, split 75/2
	      "^\\(...........................................................................\\)\\(..\\)" 
	      nil t)
	(replace-match (concat (match-string 1) "=\n" (match-string 2)) t t)
	(beginning-of-line))

      (goto-char (point-max))
      (if (/= (preceding-char) ?\n)
	  (insert "=\n"))))
    (if my-global-font-lock-mode
	(global-font-lock-mode 1))))

(defun etach-kill-label-detached ()
  "Remove the \"detached\" attribute from this message."
  (interactive)
  (set-buffer (substring (buffer-name) 0 (string-match "\-summary$" (buffer-name))))
  (rmail-set-attribute "detached" nil))

; Here are the utility (code-visible, not user-visible) functions:

(defun etach-extract-hosts (host to-list)
  "Extract hosts from candidate strings for spam to-list."
  (if (or (string-match "\\(.*\\) by[ \t\n]" host)
	  (string-match "\\(.*\\) for[ \t\n]" host))
      (setq host (match-string 1 host)))
  (if (string-match "\(really [\[]?\\([^\] ]*\\).*\)" host) ; if "really," reduce to the really part
      (setq host (match-string 1 host)))
  (if (string-match "^\\([^ ]+\\) [\(\[]+\\([^\] \)]+\\)" host)
      (let ((hosta (match-string 1 host))
	    (hostb (match-string 2 host)))
	(setq host (if (or (string-match "^[0-9.]+$" hostb)
			   (not (string-match ".+[.].+$" hostb)))
		       hosta
		     hostb))))
  (if (string-match "@\\(.+\\)" host)
      (setq host (match-string 1 host)))
  (if (string-match "^[ \t]*\\(.+\\)[ \t]*$" host) ; remove leading/trailing whitespace
      (setq host (match-string 1 host)))
  (if (string-match ".+[.]\\(.+[.].+[.].+[.].+\\)$" host)
      (setq host (match-string 1 host)))
  (etach-downcase host)
  (if (not (string-match "^[0-9.]+$" host))
      (while (string-match "[.]\\(.+\\)" host)
	(if (or (member host etach-spam-host-exclude-list)
		(member host to-list))
	    nil
	  (setq to-list (cons host to-list)))
	(setq host (match-string 1 host))))
  to-list)

(defun etach-restore-buffer (msg my-current-buffer copy-of-rmail-text)
  "Restore contents of RMAIL buffer."
  (etach-debug-msg (concat "restoring buffer due to " msg "..."))
  (message (concat "restoring buffer due to " msg "..."))
  (set-buffer my-current-buffer)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (insert copy-of-rmail-text)
  (etach-mime-decode-cleanup)
  (setq buffer-read-only t)
  (etach-debug-msg (concat "restoring buffer due to " msg "...done"))
  (message (concat "restoring buffer due to " msg "...done")))

(defun etach-prep-outgoing-mime-message ()
  "Prepare outgoing message headers and boundaries (called by
etach-attach and etach-mime-forward)."
  (if (etach-mime-part-is-multipart etach-content-type)
      (progn
	(setq etach-boundary (etach-mime-get-boundary-string etach-content-type))
	(search-forward (concat "--" etach-boundary "--\n"))
	(replace-match (concat "--" etach-boundary "\n")))
    (let ((oldE "")
	  (oldT ""))
      (goto-char (point-min))
      (if (re-search-forward "^Content-Transfer-Encoding:\\(.*\\)\n" (etach-mail-header-end) t)
	  (progn
	    (setq oldE (match-string 1))
	    (replace-match "" nil t)))
      (goto-char (point-min))
      (if (re-search-forward "^Content-Type:\\(.*\\)" (etach-mail-header-end))
	  (progn
	    (setq oldT (match-string 1))
	    (replace-match "Content-Type: multipart/mixed;\n\tboundary=\"" nil t)))
      (setq etach-boundary (etach-mime-create-boundary-marker))
      (insert etach-boundary "\"\nContent-Transfer-Encoding: 7bit")
      (search-forward (concat mail-header-separator "\n") nil)
      (insert "This is a multi-part message in MIME format.\n--" etach-boundary
	      "\nContent-Type:" oldT "\nContent-Transfer-Encoding:" oldE "\n\n")
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "--" etach-boundary "\n"))))

(defun etach-who-is-this-from ()
  "Who is this RMAIL message from?"
  (let ((sender ""))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^From:[ \t]*\\(.*\\)$" nil t)
      (setq sender (match-string 1))
      (if (string-match "\<\\([^ ]+\\)\>" sender)
	  (setq sender (match-string 1 sender))
	(if (string-match "\\([^ ]+\\)[ \t]+\(.*\)" sender)
	    (setq sender (match-string 1 sender)))))
    (if (string-match "[.]com$" sender)
	(setq sender (concat sender "_"))) ; do this so that ".com" isn't seen as a filename extension
    sender))

(defun etach-create-unique-string ()
  "Return a unique string of numbers of the form YMDHMS-R where
YMDHMS is date/time and R is random."
  (concat (format-time-string "%Y%m%d%H%M%S" (current-time)) "-" (format "%09d" (abs (random)))))

(defun etach-mime-create-boundary-marker ()
  "Return a string suitable as a mime boundary marker."
  (concat "++----------" (etach-create-unique-string) "----------++"))

(defun etach-mime-validate-minimal-headers ()
  "Confirm that default headers are in place (for mail composition)."
  (goto-char (point-min))
  (if (not (re-search-forward "^Mime-Version:" (etach-mail-header-end) t))
      (progn
	(goto-char (etach-mail-header-end))
	(insert "Mime-Version: 1.0\n")))
  (goto-char (point-min))
  (if (not (re-search-forward "^Content-Type:" (etach-mail-header-end) t))
      (progn
	(goto-char (etach-mail-header-end))
	(insert "Content-Type: text/plain; charset=us-ascii\n")))
  (goto-char (point-min))
  (if (not (re-search-forward "^Content-Transfer-Encoding:" (etach-mail-header-end) t))
      (progn
	(goto-char (etach-mail-header-end))
	(insert "Content-Transfer-Encoding: 7bit\n"))))

(defun etach-mime-get-content-headers ()
  "Assign values to variables corresponding to MIME Content- headers."
  (let ((mime-header-end (point)))
    (save-excursion
      (re-search-forward "^\\([ \t]*$\\|--\\)") ; look for end of headers (blank line or "^--")
      (beginning-of-line)
      (setq mime-header-end (point)))
    (while (re-search-forward "^Content-\\([^:]*\\):[ \t]*\\(.*\\)[ \t]*$" mime-header-end t)
      (let ((field-name (match-string 1))
	    (field-body (match-string 2)))
	(while (looking-at "\n[ \t]+")
	  (re-search-forward "\n[ \t]+\\(.*\\)[ \t]*$" mime-header-end)
	  (setq field-body (concat field-body (match-string 1)))
	  )
	(etach-debug-msg (concat "found Content-" field-name ": " field-body))
	(cond
	 ((string-match "type" field-name) (setq etach-content-type field-body))
	 ((string-match "transfer-encoding" field-name) (setq etach-content-transfer-encoding field-body))
	 ((string-match "id" field-name) (setq etach-content-id field-body))
	 ((string-match "description" field-name) (setq etach-content-description field-body))
	 ((string-match "disposition" field-name) (setq etach-content-disposition field-body))
	 (t 
	  (progn
	    (etach-debug-msg (concat "unrecognized MIME 1.0 header: [" field-name "]"))
	    (message (concat "unrecognized MIME 1.0 header: [" field-name "]")))))))
    (goto-char mime-header-end))
  )

(defun etach-mime-part-is-multipart (content-type-body)
  "Return t if this type is multipart, nil otherwise."
  (string-match "^[^;]*multipart/" content-type-body))

(defun etach-mime-part-is-message (content-type-body)
  "Return t if this type is message, nil otherwise."
  (string-match "^[^;]*message/" content-type-body))

(defun etach-mime-get-boundary-string (content-type-body)
  "Return boundary string from \"Content-Type: multipart; boundary=...\" body."
  (or (string-match "boundary[ \t]*=[ \t]*\"\\([^;]*\\)\"[ \t]*\\(;\\|$\\)" content-type-body)
      (string-match "boundary[ \t]*=[ \t]*\\(.*\\)[ \t]*\\(;\\|$\\)" content-type-body))
  (match-string 1 content-type-body))

(defun etach-mime-decode (rbeg rend T E I Desc Disp hbeg)
  "Mime-decode the region rbeg to rend based on \"content-\" headers, starting at hbeg."
  (etach-debug-msg (concat "etach-mime-decode called with T=\'" T "\', E=\'" E "\'"))
  (cond 
   ((and (string-match "text/plain" T) 
	 (not (string-match "name=" T))
	 (not (string-match "filename=\"[^;]*\"" Disp)))
    (progn
      (setq buffer-read-only nil)
      (cond
       ((string-match "quoted-printable" E)
	(progn
	  (etach-debug-msg (concat "quoted-printable-decoding MIME type \'" T "\'..."))
	  (message (concat "quoted-printable-decoding MIME type \'" T "\'..."))
	  (if etach-use-mimencode
	      (progn
		(shell-command-on-region rbeg rend "mimencode -u -q" t t)
		(exchange-point-and-mark))
	    (rmail-decode-quoted-printable rbeg rend))
	  (if (/= (preceding-char) ?\n)
	      (insert "\n"))
	  (if etach-adjust-decoded-plain-text
	      (progn
		(fill-region rbeg (point))
		(subst-char-in-region rbeg (point) ?\r ?\n)))
	  (save-excursion
	    (if (re-search-backward "^Content-Disposition:" hbeg t)
		(replace-match "X-Former-Content-Disposition:" t)))
	  (save-excursion
	    (if (re-search-backward "^Content-Transfer-Encoding:" hbeg t)
		(replace-match "Content-Transfer-Encoding: 7bit\nX-Former-Content-Transfer-Encoding:" t)))
	  ))
       ((string-match "base64" E)
	(progn
	  (etach-debug-msg (concat "base64-decoding MIME type \'" T "\'..."))
	  (message (concat "base64-decoding MIME type \'" T "\'..."))
	  (if etach-use-mimencode
	      (progn
		(shell-command-on-region rbeg rend "mimencode -u" t t)
		(exchange-point-and-mark))
	    (base64-decode-region rbeg rend))
	  (if (/= (preceding-char) ?\n)
	      (insert "\n"))
	  (if etach-adjust-decoded-plain-text
	      (progn
		(fill-region rbeg (point))
		(subst-char-in-region rbeg (point) ?\r ?\n)))
	  (save-excursion
	    (if (re-search-backward "^Content-Disposition:" hbeg t)
		(replace-match "X-Former-Content-Disposition:" t)))
	  (save-excursion
	    (if (re-search-backward "^Content-Transfer-Encoding:" hbeg t)
		(replace-match "Content-Transfer-Encoding: 7bit\nX-Former-Content-Transfer-Encoding:" t)))
	  ))
       (t
	(progn
	  (etach-debug-msg (concat "leaving as-is MIME type \'" T "\' encoding \'" E "\'"))
	  (message (concat "leaving as-is MIME type \'" T "\' encoding \'" E "\'")))))
      (etach-mime-decode-cleanup)
      (setq buffer-read-only t)))

   (t
    (let ((F nil) ; file name
	  (Fcopy nil) ; copy of file name
	  (Ff nil) ; file name sans extension
	  (Fe nil) ; file name extension (including the dot)
	  (Ftag 1) ; file tag (in case FfFe exists)
	  (require-final-newline nil)
	  (insert-default-directory t)
	  (default-directory default-directory)
	  (skip-this-file nil)
	  (my-write-error nil)
	  (stuff-to-yank nil))
      ; discard directory (path) portion of file names;
      ; notice that the leading dots on file names are discarded, too:
      (if (or (string-match "name=\"\\([^;]*/\\)*\\.*\\([^;]*\\)\""               T)
	      (string-match   "name=\\([^;]*/\\)*\\.*\\([^;]*\\)\\([ \t;]\\|$\\)" T))
	  (setq F (match-string 2 T))
	(if (string-match "filename=\"\\([^;]*/\\)*\\.*\\([^;]*\\)\"" Disp)
	    (setq F (match-string 2 Disp))
	  (if etach-prompt-me-for-file-names
	      (setq F "FILE")
	    (setq F (or (etach-who-is-this-from) (format-time-string "%Y-%m-%d"))))))
      (etach-debug-msg (concat "raw file name: [" F "]"))
      (if (string-match "^[ \t]*$" (file-name-nondirectory F))
	  (setq F "FILE"))
      (etach-safe-clean F) ; get rid of funny characters
      (if (not (string-match "\\.[a-zA-Z0-9]+$" F))
	  (let ((major-type)
		(minor-type))
	    (if (string-match "^\\(.*\\)/\\([^ \t;]*\\)" T)
		(progn
		  (setq major-type (match-string 1 T))
		  (setq minor-type (match-string 2 T))
		  ; add a file name extension based on Content-Type:
		  (if (rassoc (list (concat major-type "/" minor-type)) etach-mime-type-alist)
		      (setq F (concat F 
				      "."
				      (car (rassoc 
					    (list (concat major-type "/" minor-type))
					    etach-mime-type-alist)))))))))
      (if (string-match "^\\(.*\\)\\(\\.[a-zA-Z0-9]+\\)$" F)
	  (progn
	    (setq Ff (match-string 1 F))
	    (setq Fe (match-string 2 F)))
	(setq Ff F)
	(setq Fe ""))
      (setq Fcopy (concat Ff Fe))
      (setq default-directory 
	    (concat
	     (if (string-match "^/" etach-detachment-default-directory)
		 ""
	       default-directory)
	     etach-detachment-default-directory
	     (if (string-match "/$" etach-detachment-default-directory)
		 ""
	       "/")))
      (etach-debug-msg (concat "default directory set to: " default-directory))
      (if (not (file-exists-p default-directory))
	  (progn
	    (etach-debug-msg (concat "creating directory: " default-directory))
	    (make-directory default-directory t)))
      (if (file-exists-p (concat Ff Fe))
	  (progn
	    (while (and (<= Ftag 9999) (file-exists-p (concat Ff (format "_%04d" Ftag) Fe)))
	      (setq Ftag (1+ Ftag)))
	    (setq Ff (concat Ff (format "_%04d" Ftag)))))
      (if (> Ftag 9999)
	  (setq F Fcopy)
	(setq F (concat Ff Fe)))
      (setq Fcopy F)
      (etach-debug-msg (concat "offered for detachment: " F))
      (if etach-prompt-me-for-file-names
	  (save-excursion
	    (goto-char hbeg) ; do this just to make visual connection between part and name
	    (setq F
		  (condition-case err
		      (read-file-name "Save as: " 
				      default-directory
				      nil 
				      nil 
				      Fcopy)
		    ((quit error)
		     (setq skip-this-file t)
		     (etach-debug-msg (concat "skipping " Fcopy ": " (error-message-string err)))
		     (message (concat "skipping " Fcopy ": " (error-message-string err)))
		     Fcopy)))))
      (if (string-match "^[ \t]*$" (file-name-nondirectory F))
	  (progn
	    (setq F Fcopy)
	    (etach-debug-msg (concat "cannot accept blank filename, using " F " instead"))
	    (message (concat "cannot accept blank filename, using " F " instead"))
	    ))
      (setq buffer-read-only nil)
      (if skip-this-file
	  nil
	; here's where the action is:
	(setq F (expand-file-name F))
	(etach-debug-msg (concat "detaching: " F))
	(setq stuff-to-yank (buffer-substring rbeg rend))
	(kill-region rbeg rend)
	; this save-excursion is necessary if using write-file, since write-file 
	; visits the buffer after writing
	(save-excursion
	  (let ((buffer (get-buffer-create " *temp*")))
	    (set-buffer buffer)
	    (unwind-protect
		(progn
		  (insert stuff-to-yank) ; use this instead of (yank) to avoid setting mark
		  (cond
		   ((string-match "base64" E)
		    (progn
		      (etach-debug-msg (concat "base64-decoding MIME type \'" T "\'..."))
		      (message (concat "base64-decoding MIME type \'" T "\'..."))
		      (if etach-use-mimencode
			  (shell-command-on-region (point-min) (point-max) "mimencode -u" t t)
			(base64-decode-region (point-min) (point-max)))))
		   ((string-match "quoted-printable" E)
		    (progn
		      (etach-debug-msg (concat "quoted-printable-decoding MIME type \'" T "\'..."))
		      (message (concat "quoted-printable-decoding MIME type \'" T "\'..."))
		      (if etach-use-mimencode
			  (shell-command-on-region (point-min) (point-max) "mimencode -u -q" t t)
			(rmail-decode-quoted-printable (point-min) (point-max)))))
		   (t
		    (progn
		      (etach-debug-msg (concat "leaving as-is MIME type \'" T "\' encoding \'" E "\'"))
		      (message (concat "leaving as-is MIME type \'" T "\' encoding \'" E "\'")))))

		  (condition-case err
		      (let ((jka-compr-compression-info-list nil))
			(if (or (string-match "^[0-9][.]" emacs-version)
				(string-match "^1[0-9][.]" emacs-version)
				(string-match "^20[.][0-2][.]" emacs-version))
			    (write-region (point-min) (point-max) F nil nil nil)
			  (write-region (point-min) (point-max) F nil nil nil t)))
		    ((quit error)
		     (setq my-write-error t)
		     (set-buffer-modified-p nil)
		     (etach-debug-msg (concat "un-detaching: " (error-message-string err)))
		     (message (concat "un-detaching: " (error-message-string err)))
		     (sit-for 1)
		     )))
	      (kill-buffer buffer))))

	(if my-write-error
	    (progn
	      (etach-debug-msg (concat "restoring MIME attachment due to error"))
	      (insert stuff-to-yank))
	  (if etach-restore-attachments-after-detach
	      (progn
		(etach-debug-msg (concat "restoring MIME attachment per user preference"))
		(save-excursion
		  (if (re-search-backward "^[ \t]*$" hbeg t)
		      (replace-match (concat "X-Detachment: [file:" F "]\n"))))
		(insert stuff-to-yank))
	    (insert "[file:" F "]\n")
	    (save-excursion
	      (if (re-search-backward "^Content-Disposition:" hbeg t)
		  (replace-match "X-Former-Content-Disposition:" t)))
	    (save-excursion
	      (if (re-search-backward "^Content-Transfer-Encoding:" hbeg t)
		  (replace-match "Content-Transfer-Encoding: 7bit\nX-Former-Content-Transfer-Encoding:" t)))
	    (save-excursion
	      (if (re-search-backward "^Content-Type:" hbeg t)
		  (replace-match "Content-Type: text/plain; charset=us-ascii\nX-Former-Content-Type:" t)))))
	) ; end of stuff to do if skip-this-file is nil
      (etach-mime-decode-cleanup)
      (setq buffer-read-only t))))
  (etach-debug-msg (concat "exiting etach-mime-decode with default directory: " default-directory)))

(defun etach-mime-detach (bboundary)
  "Detach the encoded attachments from the MIME part or message starting at point."
  (etach-debug-msg (concat "etach-mime-detach called with bboundary [" bboundary "]"))
  (let ((etach-content-type "text/plain; charset=us-ascii")
	(etach-content-transfer-encoding "7bit")
	(etach-content-id "")
	(etach-content-description "")
	(etach-content-disposition "")
	(hbeg (point))
	(hend (point))
	(header-chars "[\]!\"#\$%&'()\*\+,\./0-9;<=>\?@A-Z_^\[`a-z{|}~-]")) ; ascii 33-126 except 58
    ; advance to start of first header candidate
    (save-excursion
      (re-search-forward "^[ \t]*$") ; the Content- lines can be absent, but there should always be a blank line
      (beginning-of-line)
      (setq hend (point)))
    (beginning-of-line)
    ; we need the following in case the bboundary looks like a header
    ; (e.g., non-compliant sender using colon in MIME separator):
    (if (looking-at (regexp-quote bboundary))
	(forward-line))
    (re-search-forward (concat "^" header-chars "+:") hend t)
    (beginning-of-line)
    (etach-debug-msg (concat "etach-mime-detach: header lines begin here"))
    (setq hbeg (point))
    ; get headers
    (etach-mime-get-content-headers)
    (beginning-of-line)
    ; advance to first line after separator
    (forward-line)
    (cond 

     ((etach-mime-part-is-multipart etach-content-type)
      (let* ((b (etach-mime-get-boundary-string etach-content-type))
	     (bb (concat "--" b)))
	(etach-debug-msg (concat "etach-mime-detach processing (multipart): " etach-content-type))
	(re-search-forward (regexp-quote bb) nil t)
	(beginning-of-line)
	(while (not (looking-at (regexp-quote (concat bb "--"))))
	  (etach-mime-detach bb))
	; we should be at a boundary, but in case there are blank lines we better make sure:
	(re-search-forward (if (string= bboundary "\\'") "\\'" (regexp-quote bboundary)))
	(beginning-of-line)))

     ((etach-mime-part-is-message etach-content-type)
      (progn
	(etach-debug-msg (concat "etach-mime-detach processing (message): " etach-content-type))
	(etach-mime-detach bboundary)
	; we should be at a boundary, but in case there are blank lines we better make sure:
	(re-search-forward (if (string= bboundary "\\'") "\\'" (regexp-quote bboundary)))
	(beginning-of-line)))

     (t
      (let ((ebeg (point))
	    (eend (point)))
	(etach-debug-msg (concat "etach-mime-detach processing: " etach-content-type))
	(re-search-forward (if (string= bboundary "\\'") "\\'" (regexp-quote bboundary)))
	(beginning-of-line)
	(setq eend (point))
	(etach-mime-decode ebeg eend 
			   etach-content-type 
			   etach-content-transfer-encoding 
			   etach-content-id
			   etach-content-description
			   etach-content-disposition hbeg)
	; we should be at a boundary, but in case there are blank lines we better make sure:
	(re-search-forward (if (string= bboundary "\\'") "\\'" (regexp-quote bboundary)))
	(beginning-of-line)
	))

     )))

(defun etach-mime-decode-cleanup () ; see rmail-cease-edit in rmailedit.el
  "Clean up the RMAIL structure before returning to etach-mime-detach."
  (save-excursion
    (goto-char (point-max))
    (if (/= (preceding-char) ?\n)
	(insert "\n"))
    (set-marker (aref rmail-message-vector (1+ rmail-current-message))
		(point))
    (rmail-set-attribute "detached" t)
    (if (boundp 'rmail-summary-vector)
	(progn
	  (aset rmail-summary-vector (1- rmail-current-message) nil)
	  (save-excursion
	    (rmail-widen-to-current-msgbeg
	     (function (lambda ()
			 (forward-line 2)
			 (if (looking-at "Summary-line: ")
			     (delete-region (point)
					    (progn (forward-line 1)
						   (point))))))))))))


(defun etach-downcase (s)
  "Take string as argument, return lowercase version."
  (let ((x 65))
    (while (<= x 90)
      (etach-subst-char-in-string x (+ x 32) s t)
      (setq x (+ 1 x)))))

(defun etach-safe-clean (f)
  "Clean up a string to make it suitable as a safe one-word file name."
  (let ((x 0))
    (while (<= x 44)
      (etach-subst-char-in-string x ?_ f t)
      (setq x (+ 1 x)))
    (setq x 58)
    (while (<= x 64)
      (etach-subst-char-in-string x ?_ f t)
      (setq x (+ 1 x)))
    (setq x 91)
    (while (<= x 94)
      (etach-subst-char-in-string x ?_ f t)
      (setq x (+ 1 x)))
    (setq x 96)
    (while (<= x 96)
      (etach-subst-char-in-string x ?_ f t)
      (setq x (+ 1 x)))
    (setq x 123)
    (while (<= x 255)
      (etach-subst-char-in-string x ?_ f t)
      (setq x (+ 1 x)))))

(defun etach-debug-msg (msg-string)
  "Write a debug message."
  (if etach-debug
      (let ((debug-msg 
	     (concat
	      (buffer-name)
	      " (line "
	      (number-to-string (+ 1 (count-lines (point-min) (point))))
	      " ["
	      (if (char-after (1- (point)))

		  (if (= (preceding-char) ?\n)
		      "\\n"
		    (char-to-string (char-after (1- (point))))))

	      "/"
	      (if (char-after (point))
		  (if (= (following-char) ?\n)
		      "\\n"
		    (char-to-string (char-after (point)))))
	      "]): " msg-string "\n")))
	(get-buffer-create "etach-debug")
	(save-excursion
	  (set-buffer "etach-debug")
	  (goto-char (point-max))
	  (insert debug-msg)))))

; The following are local copies of functions that may be absent from
; some Emacs versions or installations (name has "etach-" prepended).

; The following is nicked from sendmail.el (and modified in form but
; not function):

(defun etach-mail-header-end () 
  "Return the buffer location of the end of headers, as a number."
   (save-restriction
     (widen)
     (save-excursion
       (goto-char (point-min))
       (while (looking-at "^[^: \n]+:\\|^[ \t]")
 	(forward-line 1))
       (point))))

; The following is taken from subr.el:

(defun etach-subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

; The following is taken from rmail.el:

(defun etach-rmail-msg-is-pruned ()
  (rmail-maybe-set-message-counters)
  (save-restriction
    (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (= (following-char) ?1))))

; let (provide 'etach) be the last line:

(provide 'etach)

; etach.el ends here
