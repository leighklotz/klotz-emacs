;;;-*-EMACS-LISP-*-

;;; Leigh L. Klotz, Jr. WA5ZNU &lt;klotz@graflex.org&gt;

(defvar rig-device "/dev/ttyS0")

;;; Command definitions
(defconst k2-next-if-filter "FW0000")
(defconst k2-next-af-filter "SW29")
(defconst k2-dsp-notch-toggle "SW80")
(defconst k2-dsp-noise-reduction-toggle "SW81")
(defconst k2-qsy-vfo-a "FA")
(defconst k2-qsy-vfo-b "FB")
(defconst k2-set-mode "MD")

(defconst k2-modes
  '(("LSB" . 1) ("USB" . 2) ("CW" . 3) ("RTTY" . 6) ("CW-R" . 7) ("RTTY-R" . 9)))

;;; Command functions
(defun k2-next-filter ()
  "Goes to the next K2 Filter"
  (interactive)
  (k2-rig-command-0 k2-next-if-filter)
  (k2-rig-command-0 k2-next-af-filter))

(defun k2-qsy-vfo-a (khz)
  "Sets the frequency of K2 VFO A"
  (interactive "NFrequency: ")
  (k2-rig-command-1 k2-qsy-vfo-a (format "00%06d000" khz)))

(defun k2-qsy-vfo-b (khz)
  "Sets the frequency of K2 VFO B"
  (interactive "NFrequency: ")
  (k2-rig-command-1 k2-qsy-vfo-b (format "00%06d000" khz)))

(defun k2-set-mode ()
  "Sets the K2 Mode"
  (interactive)
  (let* ((completion-ignore-case t)
	 (mode-name (upcase (completing-read "Mode: " k2-modes nil t nil)))
	 (mode-num (and mode-name (cdr (assoc mode-name k2-modes)))))
    (if mode-num
	(k2-rig-command-1 k2-set-mode mode-num))))
  

(defun k2-dsp-notch-toggle ()
  "Toggles the K2 DSP notch filter"
  (interactive)
  (k2-rig-command-0 k2-dsp-notch-toggle))

(defun k2-dsp-noise-reduction-toggle ()
  "Toggles the K2 DSP noise reduction filter"
  (interactive)
  (k2-rig-command-0 k2-dsp-noise-reduction-toggle))

(defun k2-get-vfo-a ()
  (k2-rig-command-0-read k2-qsy-vfo-a 14))



;;; I/O

(defun k2-rig-command-0 (command)
  (shell-command (format "echo -n \"%s;\" &gt; %s" command rig-device)))

(defun k2-rig-command-0-read (command n)
  (shell-command (format "echo -n \"%s;\" &gt; %s; dd if=%s bs=1 count=%d" command rig-device rig-device n)))


(defun k2-rig-command-1 (command arg1)
  (shell-command (format "echo -n \"%s%s;\" &gt; %s" command arg1 rig-device)))

;;; Key Bindings
(easy-mmode-defmap k2-minor-mode-map
  '(("\e`" . k2-next-filter)
    ([(control meta ?m)] . k2-set-mode)
    ([(control meta ?g)] . k2-qsy-vfo-a)
    ([(control meta ?n)] . k2-dsp-noise-reduction-toggle)
    ([(control meta ?m)] . k2-set-mode))
  "Keymap for `k2-minor-mode'.")

(define-minor-mode k2-minor-mode
  "Minor mode for k2-rig-control context diffs.
\\{k2-minor-mode-map}"
  nil " K2" nil)

;;; Todo:  
;;; Reading status (use Linux rigctl or read directly?)
;;; Mode line (use display-time-hook?)
