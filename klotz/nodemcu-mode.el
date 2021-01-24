;;; -*- lexical-binding: nil -*-
;;; https://github.com/katyo/nodemcu-mode/blob/master/nodemcu-mode.el

(require 'cl)

(if (not (fboundp 'string-suffix-p))
    (defun string-suffix-p (str1 str2 &optional ignore-case)
      (let ((begin2 (- (length str2) (length str1)))
	    (end2 (length str2)))
	(when (< begin2 0) (setq begin2 0))
	(eq t (compare-strings str1 nil nil
			       str2 begin2 end2
			       ignore-case)))))

(defgroup nodemcu '()
  "Helper mode for NodeMCU users.")

(defcustom nodemcu-interface
  'nodemcu-interface-network
  "The interface for communication with device."
  :type '(radio
          (const nodemcu-interface-serial :tag "Serial device")
          (const nodemcu-interface-network :tag "Network socket")
          ))

(defcustom nodemcu-target-device
  "/dev/ttyUSB1"
  "The target device for communication (ex. /dev/ttyUSB1)."
  :type 'string)

(defcustom nodemcu-target-speed
  9600
  "The target device communication baudrate (ex. 9600)."
  :type 'integer)

(defcustom nodemcu-target-host
  "ESP_01234"
  "The target host or IP for network communication."
  :type 'string)

(defcustom nodemcu-target-port
  23
  "The target port for network communication."
  :type 'integer)

(defcustom nodemcu-compile-on-upload
  't
  "Compile modules on upload."
  :type 'boolean)

(defcustom nodemcu-no-compile
  '("init.lua")
  "File patterns to skip compilation."
  :type '(repeat regexp))

(defcustom nodemcu-clear-on-upload
  't
  "Remove uploaded module from 'package.loaded' Lua table."
  :type 'boolean)

(defcustom nodemcu-restart-on-upload
  nil
  "Restart node after uploading Lua module."
  :type 'boolean)

(defcustom nodemcu-debug-io
  nil
  "Enable mode I/O debugging."
  :type 'boolean)

(defun nodemcu-open-connection ()
  (when (get-process "nodemcu-connection")
    (delete-process "nodemcu-connection"))
   (pcase nodemcu-interface
     (`nodemcu-interface-serial
      (make-serial-process :name "nodemcu-connection"
                           :noquery t
                           :port nodemcu-target-device
                           :speed nodemcu-target-speed))
     (`nodemcu-interface-network
      (make-network-process :name "nodemcu-connection"
                            :noquery t
                            :host nodemcu-target-host
                            :service nodemcu-target-port))
     ))

;;(let ((conn (nodemcu-open-connection))) (message "%s %s" (process-status conn) (process-status "nodemcu-connection")) (delete-process conn))

(defun nodemcu-get-interface (connection)
  (pcase (process-status connection)
    (`open 'nodemcu-interface-serial)
    (`connect 'nodemcu-interface-network)))

(defun nodemcu-input-output (connection input &optional timeout)
  (setq input (concat input "\n"))
  (let ((interface (nodemcu-get-interface connection))
        (output nil))
    (set-process-filter connection
                        (lambda (_ chunk)
                          (setq output (if output
                                           (concat output chunk)
                                         chunk))))
    (when nodemcu-debug-io
      (message "NodeMCU input: [=[%s]=]" input))
    (process-send-string connection input)
    (when (accept-process-output connection timeout)
      ;;(let ((output_ nil)) (while (not (eq output_ output)) (setq output_ output) (sleep-for 0.1)))
      (while (not (string-suffix-p "> " output)) (sleep-for 0.1))
      (pcase interface
        (`nodemcu-interface-serial
         (setq output (replace-regexp-in-string "\n\n" "\n" output))))
      (setq output (substring output 0 (- (length output) (length "> "))))
      (when nodemcu-debug-io
        (message "NodeMCU output: [=[%s]=]" output))
      (when (string-prefix-p input output)
        (setq output (substring output (length input))))
      (while (string-prefix-p "\n" output)
        (setq output (substring output 1)))
      (while (string-suffix-p "\n" output)
        (setq output (substring output 0 (- (length output) 1))))
      )
    (set-process-filter connection nil)
    output))

(defmacro nodemcu-do-request (input)
  "Do request with connection."
  `(nodemcu-input-output nodemcu-connection ,input))

(defmacro nodemcu-with-connection (&rest body)
  "Execute something with connection."
  (let ((result (make-symbol "result")))
    `(let ((nodemcu-connection (nodemcu-open-connection)))
       (cond ((eq (process-status nodemcu-connection) 'open)
              (let ((,result (progn ,@body)))
                (delete-process nodemcu-connection)
                ,result))))))

(defun nodemcu-at-point ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position) (line-end-position))))

(defmacro nodemcu-show-result (result)
  `(progn
     (when (called-interactively-p 'any)
       (message "%s" ,result))
     ,result))

(defun nodemcu-execute (stmt)
  (interactive "sNodeMCU execute: ")
  (nodemcu-show-result
   (nodemcu-with-connection
    (nodemcu-do-request stmt))))

(defun nodemcu-execute-at-point (beginning end)
  (interactive (nodemcu-at-point))
  (nodemcu-show-result
   (nodemcu-execute
    (buffer-substring-no-properties beginning end))))

(defun nodemcu-evaluate (expr)
  (interactive "sNodeMCU evaluate: ")
  (nodemcu-show-result
   (nodemcu-with-connection
    (nodemcu-do-request (format "print(%s)" expr)))))

(defun nodemcu-evaluate-at-point (beginning end)
  (interactive (nodemcu-at-point))
  (nodemcu-show-result
   (nodemcu-evaluate
    (buffer-substring-no-properties beginning end))))

(defun nodemcu-send-buffer (buffer)
  (interactive "bNodeMCU buffer to send: ")
  (nodemcu-execute
   (with-current-buffer buffer
     (buffer-substring-no-properties
      (point-min) (point-max)))))

(defun nodemcu-get-heap ()
  (interactive)
  (nodemcu-show-result
   (nodemcu-evaluate "node.heap()")))

(defun nodemcu-restart-node ()
  (interactive)
  (nodemcu-evaluate "node.restart()"))

(defun nodemcu-parse-file-list (list)
  (mapcar (lambda (line)
            (let ((x (split-string line)))
              (cons (car x) (string-to-number (cadr x) 10))))
  (split-string list "\n")))

(defun nodemcu-list-files (&optional what)
  (interactive)
  (funcall (pcase what
             (`names (lambda (files) (mapcar 'car files)))
             (_ (lambda (files) files)))
           (nodemcu-parse-file-list
            (nodemcu-show-result
             (nodemcu-execute "for n,s in pairs(file.list()) do print(n..\" \"..s) end")))))

(defun nodemcu-file-contents (file)
  "Get contents of local file."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun nodemcu-test-patterns (pat str)
  (some (lambda (pat) (string-match-p pat str)) pat))

(defun nodemcu-upload-file (file)
  (interactive "fNodeMCU upload file: ")
  (let ((name (file-name-nondirectory file))
        (base (file-name-base file))
        (ext (file-name-extension file)))
    (nodemcu-with-connection
     (when nodemcu-clear-on-upload
       (nodemcu-do-request (format "package.loaded[\"%s\"]=nil" name)))
     (nodemcu-do-request (format "file.remove(\"%s\")" name))
     (nodemcu-do-request (format "file.open(\"%s\", \"w\")" name))
     (mapc (lambda (line)
             (nodemcu-do-request (format "file.writeline([===[%s]===])" line)))
           (split-string (nodemcu-file-contents file) "\n"))
     (nodemcu-do-request (format "file.close()" name))
     (when (and nodemcu-compile-on-upload (string-equal "lua" ext) (not (nodemcu-test-patterns nodemcu-no-compile name)))
       (nodemcu-do-request (format "file.remove(\"%s.lc\")" base))
       (nodemcu-show-result (nodemcu-do-request (format "print(node.compile(\"%s\"))" name)))
       (nodemcu-do-request (format "file.remove(\"%s\")" name)))
     (when nodemcu-restart-on-upload
       (nodemcu-do-request "node.restart()"))
     )))

(defun nodemcu-format-fs ()
  (interactive)
  (nodemcu-show-result
   (nodemcu-execute "file.format()")))

(defun nodemcu-parse-fs-info (str)
  (split-string str "[ \t]+"))

(defun nodemcu-fs-info ()
  (interactive)
  (nodemcu-parse-fs-info
   (nodemcu-show-result
    (nodemcu-evaluate "file.fsinfo()"))))

(defun nodemcu-rename-file (from to)
  (interactive
   (let* ((files (nodemcu-list-files 'names))
          (from (completing-read "NodeMCU rename file: " files))
          (to (completing-read (format "NodeMCU rename file: %s to: " from) files)))
     (list from to)))
  (nodemcu-show-result
   (nodemcu-evaluate (format "file.rename(\"%s\", \"%s\")" from to))))

(defun nodemcu-remove-file (file)
  (interactive
   (list (completing-read "NodeMCU remove file: "
                          (nodemcu-list-files 'names))))
  (nodemcu-show-result
   (nodemcu-evaluate (format "file.remove(\"%s\")" file))))

(define-minor-mode nodemcu-mode
  "Helper mode for NodeMCU users."
  :lighter " NodeMCU"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-e") 'nodemcu-evaluate)
            (define-key map (kbd "C-c C-t") 'nodemcu-execute)
            (define-key map (kbd "C-x C-e") 'nodemcu-evaluate-at-point)
            (define-key map (kbd "C-c C-h") 'nodemcu-get-heap)
            (define-key map (kbd "C-c C-b") 'nodemcu-send-buffer)
            (define-key map (kbd "C-c C-r") 'nodemcu-restart-node)
            (define-key map (kbd "C-c C-k") 'nodemcu-format-fs)
            (define-key map (kbd "C-c C-a") 'nodemcu-fs-info)
            (define-key map (kbd "C-c C-s") 'nodemcu-list-files)
            (define-key map (kbd "C-c C-d") 'nodemcu-remove-file)
            (define-key map (kbd "C-c C-n") 'nodemcu-rename-file)
            (define-key map (kbd "C-c C-u") 'nodemcu-upload-file)
            map))
