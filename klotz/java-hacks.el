;;;-*-EMACS-LISP-*-

(require 'compile)
(require 'cl)

(setq my-c-default-style "gnu")

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-java-mode-hook ()
  (c-set-style my-c-default-style)
  ;; findbugs
  ;; (REGEXP FILE [LINE COLUMN TYPE HYPERLINK HIGHLIGHT...]).
  (add-to-list 'compilation-error-regexp-alist '("^\\([^()\n:]+\\):\\([0-9-]+\\)\\(:\\([0-9-]+\\)\\)?\\(.*\\)$" 1 (2 . 4) nil nil nil))
  (define-key (current-local-map) "\C-c\C-f" 'my-java-flymake-mode)
  (define-key (current-local-map) "\C-c\C-j" 'flymake-java-recompile-buffer)
  (define-key (current-local-map) "\C-c\C-n" 'flymake-goto-next-error)
  (define-key (current-local-map) "\C-c\C-p" 'flymake-goto-previous-error)
  (define-key (current-local-map) "\C-c\C-d" 'flymake-popup-current-error-menu)
  (define-key (current-local-map) "\C-c\C-i" 'remove-unused-imports)
  (define-key (current-local-map) (kbd "C-<") 'copy-last-generic-declaration)
  (define-key (current-local-map) (kbd "M-+") 'add-import)
  (define-key (current-local-map) (kbd "C-+") 'guice-inject-dependency)
  (define-key (current-local-map) (kbd "<C-tab>") 'complete-symbol)
  (define-key (current-local-map) (kbd "C-#") 'copy-line-number-for-jdb) ;\e# fails on the Mac
  (modify-syntax-entry ?@ ".")          ;make @Inject be just Inject in M-.
  (setq require-final-newline t)
  (c-set-style my-c-default-style)
  (setq show-trailing-whitepace t)
  '(my-java-add-menu
   "Recompile Buffer and Redefine Classes"
   "Recompile buffer and send new class file to running Java"
   'flymake-java-reload-class t)
  (my-java-add-menu
   "Recompile Buffer"
   "Recompile buffer but don't send new over class file"
   'flymake-java-recompile-buffer nil))

;;; Sadly this seems to fail
(defun my-java-add-menu (name help func sep)
  ;; if we don't check first we get one menu entry but multiple separators .
  (if (not (assoc (intern name) c-java-menu))
      (progn
        (if sep 
            (easy-menu-add-item c-java-menu nil "-" nil))
        (easy-menu-add-item c-java-menu nil (vector name func t) help))))

(defvar add-import-history-list nil)
(defun add-import-interactive ()
  (let ((class (find-tag-tag "Import Class: ")))
    (list class
          (let ((p (alist-of-packages-in-buffer class))
                (package-default (import-package-default class)))
            (completing-read 
             (if package-default (concat "Package: (default " package-default ") ") "Package: ")
             p nil nil "" 'add-import-history-list package-default t)))))

(defun import-package-default (class) 
  (let* ((p (alist-of-packages-in-buffer class))
         (canonical (cdr (assoc class canonical-java-packages)))
         (package-default (if (null canonical) (tags-find-java-class-package "Import " class) canonical)))
    package-default))

(defconst package-declaration-regexp "^\\W*package\\W+\\([a-zA-Z0-9_.]+\\)")
(defconst import-start-regexp "^\\s-*import\\s-+")

(defun add-import (class package)
  (interactive (add-import-interactive))
  (let ((search-fold-case nil)
        (add-at-beginning (string-match "^java" package)))
    (if (equal package "") (error "Package not specified"))
    (let ((package-and-class (concat package (if (/= ?. (aref package (1- (length package)))) "." "") class)))
      (if (not (save-excursion 
                 (goto-char (point-min))
                 (re-search-forward (concat import-start-regexp (regexp-quote package-and-class) ";") nil t)))
          (save-excursion
            (if add-at-beginning
                (progn
                  (goto-char (point-min))
                  (if (null (re-search-forward import-start-regexp nil t))
                      (re-search-forward package-declaration-regexp))
                  (beginning-of-line 1))
              (progn
                (goto-char (point-max))
                (if (null (re-search-backward import-start-regexp nil t))
                    (re-search-backward package-declaration-regexp))
                (end-of-line 1)
                (newline 2)))
            (delete-blank-lines)
            (insert "import " package-and-class ";\n"))))))

(defun tags-find-java-class-package (prompt classname)
  (interactive (list (find-tag-tag (concat prompt " Class: "))))
  (save-excursion 
    (let ((buffer (find-tag-java-class-ignore-errors classname)))
      (if buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward package-declaration-regexp)
            (match-string 1))))))

(defun find-tag-java-class-ignore-errors (classname) 
  (let ((tag (concat classname ".java")))
    ;; it would be nice to do regexp "^%s.java" but I can't get regexp to work at all here.
    (ignore-errors (find-tag-noselect tag nil))))

(defvar canonical-java-packages)
(setq canonical-java-packages
  (nconc
   (mapcar #'(lambda (x) (cons x "org.jdom2"))
           '("Element" "Document" "JDOMException"))
   (mapcar #'(lambda (x) (cons x "com.google.inject"))
           '("Inject" "Provider" "Singleton" "Provides"))
   (mapcar #'(lambda (x) (cons x "com.google.inject.assistedinject"))
           '("Assisted" "AssistedInject" "FactoryProvider" "FactoryModuleBuilder"))
   (mapcar #'(lambda (x) (cons x "com.google.inject.name"))
           '("Names" "Named"))
   (mapcar #'(lambda (x) (cons x "com.google.inject.multibindings"))
           '("MapBinder" "Multibinder"))
   (mapcar #'(lambda (x) (cons x "com.google.common.base"))
           '("Joiner"))
   (mapcar #'(lambda (x) (cons x "org.restlet"))
           '("Request" "Response" "Status"))
   (mapcar #'(lambda (x) (cons x "org.restlet.resource"))
           '("Get" "Post" "Put" "Delete" "ResourceException"))
   (mapcar #'(lambda (x) (cons x "org.restlet.representation"))
           '("Representation" "Resource"))
   (mapcar #'(lambda (x) (cons x "org.slf4j")) '("Logger" "LoggerFactory"))
   (mapcar #'(lambda (x) (cons x "java.net"))
           '("Authenticator" "BindException" "CacheRequest" "CacheResponse" "ConnectException" "ContentHandler" "ContentHandlerFactory" "CookieHandler" "CookieManager" "CookiePolicy" "CookieStore" "DatagramPacket" "DatagramSocket" "FileNameMap" "HttpCookie" "HttpRetryException" "HttpURLConnection" "IDN" "Inet4Address" "Inet6Address" "InetAddress" "InetSocketAddress" "InterfaceAddress" "JarURLConnection" "MalformedURLException" "MulticastSocket" "NetPermission" "NetworkInterface" "NoRouteToHostException" "PasswordAuthentication" "PortUnreachableException" "ProtocolException" "Proxy" "ProxySelector" "ResponseCache" "SecureCacheResponse" "ServerSocket" "Socket" "SocketAddress" "SocketException" "SocketInputStream" "SocketOptions" "SocketOutputStream" "SocketPermission" "SocketTimeoutException" "SocksConsts" "URI" "URISyntaxException" "URL" "URLClassLoader" "URLConnection" "URLDecoder" "URLEncoder" "URLStreamHandler" "URLStreamHandlerFactory" "UnknownHostException" "UnknownServiceException"))
   (mapcar #'(lambda (x) (cons x "java.util"))
           '("ArrayList" "Arrays" "BitSet" "Calendar" "Collection" "Collections" "Comparator" "ConcurrentModificationException" "Currency" "Date" "Dictionary" "DuplicateFormatFlagsException" "EmptyStackException" "EnumMap" "EnumSet" "Enumeration" "EventListener" "EventObject" "Formatter" "FormatterClosedException" "HashMap" "HashSet" "Hashtable" "IdentityHashMap" "Iterator" "LinkedHashMap" "LinkedHashSet" "LinkedList" "List" "ListIterator" "Locale" "Map" "NoSuchElementException" "Observable" "Observer" "PriorityQueue" "Properties" "PropertyPermission" "Queue"  "ResourceBundle" "Scanner" "ServiceLoader" "Set" "SortedMap" "SortedSet" "Stack" "TimeZone" "Timer" "TreeMap" "TreeSet" "UUID" "WeakHashMap"))
   (mapcar #'(lambda (x) (cons x "java.util.concurrent"))
           '("Callable" "Future" "FutureTask" "ExecutionException" "TimeoutException" "TimeUnit"))
   (mapcar #'(lambda (x) (cons x "java.io"))
           '("Bits" "BufferedInputStream" "BufferedOutputStream" "BufferedReader" "BufferedWriter" "ByteArrayInputStream" "ByteArrayOutputStream" "CharArrayReader" "CharArrayWriter" "CharConversionException" "DataInput" "DataInputStream" "DataOutput" "DataOutputStream" "EOFException" "ExpiringCache" "Externalizable" "File" "FileDescriptor" "FileFilter" "FileInputStream" "FileNotFoundException" "FileOutputStream" "FilePermission" "FileReader" "FileSystem" "FileWriter" "FilenameFilter" "FilterInputStream" "FilterOutputStream" "FilterReader" "FilterWriter" "IOException" "InputStream" "InputStreamReader" "InterruptedIOException" "InvalidClassException" "InvalidObjectException" "LineNumberInputStream" "LineNumberReader" "NotActiveException" "NotSerializableException" "ObjectInput" "ObjectInputStream" "ObjectInputValidation" "ObjectOutput" "ObjectOutputStream" "ObjectStreamClass" "ObjectStreamConstants" "ObjectStreamException" "ObjectStreamField" "OptionalDataException" "OutputStream" "OutputStreamWriter" "PipedInputStream" "PipedOutputStream" "PipedReader" "PipedWriter" "PrintStream" "PrintWriter" "PushbackInputStream" "PushbackReader" "RandomAccessFile" "Reader" "SequenceInputStream" "Serializable" "SerializablePermission" "StreamCorruptedException" "StreamTokenizer" "StringBufferInputStream" "StringReader" "StringWriter" "SyncFailedException" "UTFDataFormatException" "UnsupportedEncodingException" "WriteAbortedException" "Writer"))))


(defun parse-class-and-package-name (string)
  (let* ((list (nreverse (split-string string "\\.")))
         (class (car list))
         (package (reduce '(lambda (x y) (concat x "." y)) (nreverse (cdr list)))))
    (list class package)))
(parse-class-and-package-name "java.lang.Map")

(defun alist-of-packages-in-buffer (&optional class)
  (let ((search-fold-case nil)
        (packages 
         (let ((p nil))
           (save-excursion
             (goto-char (point-min))
             (while (< (point) (point-max))
               (if (looking-at "^\\s-*import\\s-+\\(\\(\\([A-Z0-9a-z_]+\\)\\.\\)+\\)+")
                   (let ((val (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
                     (setq p (cons (cons val val) p))))
               (forward-line 1)))
           p)))
    (if (and class (assoc class canonical-java-packages))
        (cons (cdr (assoc class canonical-java-packages)) nil) ;packages
      packages)))

(defun get-buffer-package-name () 
  (let ((search-fold-case nil))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^\\s-*package\\s-+")
      (beginning-of-line 1)
      (if (looking-at "^[ \t]*package\\s-+\\([A-Z0-9a-z_.]+\\)")
          (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun get-point-class-name ()
  (let ((search-fold-case nil))
    (save-excursion
      (let ((package-name (get-buffer-package-name)))
        (re-search-backward
         "^\\(public\\s-+\\|private\\s-+\\|protected\\s-+\\|static\\s-+\\|final\\s-+\\|abstract\\s-+\\)*\\(class\\|interface\\)\\s-+\\([A-Z0-9a-z_]+\\)")
        (concat package-name "." (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))))
    


(defun copy-line-number-for-jdb (&optional arg)
  (interactive "*P")
  "Copy string for breakpoint at current Java method name and line number to the kill buffer so you can insert it into JDB with \\[yank].
With prefix argument, copies string to clear breakpoint instead."
  (let ((line-number
         (let ((start (point-min))
               (n (line-number-at-pos)))
           (if (= start 1)
               n
             (save-excursion
               (save-restriction
                 (widen)
                 (+ n (line-number-at-pos start) -1)))))))
    (kill-new (format "%s %s:%d" (if arg "clear" "stop at") (get-point-class-name) line-number))))

(setq last-generic-declaration-point nil) ; crock should track and be buffer local
(defun copy-last-generic-declaration ()
  "Copy last <[A-Z][A-Z_0-9<>\\[\\]+> to point.
When invoked again, switch to previous one.
TODO: Wrap. (To start over, delete the inserted declaration.)"
  (interactive)
  (let* ((regexp "<[A-Z<][][ A-Za-z_0-9<>,]+>")
         (current (if (looking-at regexp) (match-string 0)))
         (match nil))
    (save-excursion
      (if (and last-generic-declaration-point (looking-at regexp))
          (goto-char last-generic-declaration-point))
      (catch 'break
        (while (re-search-backward regexp nil t)
          (setq last-generic-declaration-point (point) match (match-string 0))
          (if (or (null current) (not (equal (match-string 0) current))) (throw 'break nil)))))
    (save-excursion
      (if match
          (cond (current
                 (re-search-forward regexp nil t)
                 (replace-match match nil nil))
                (t 
                 (insert match)))))))

(defun my-current-line-error-info () 
  (let* ((result "")
         (line-no             (line-number-at-pos))
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

(defun replace-string-noninteractive (from-string to-string)
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun remove-trailing-brace-comments ()
  "Removes all // comments that occur right after a close brace, 
unless they have a */ in them as that might match an opening /*"
  (interactive)
  (save-excursion
    (goto-char (point-min))
  (while (re-search-forward "}[ \t]*//.*$" nil t)
    (beginning-of-line 1)
    (re-search-forward "}[ \t]//" nil t)
    (if (not (looking-at ".*\\*/")) 
        (progn                          ;do search again from BOL this time for replace
          (beginning-of-line 1)
          (re-search-forward "}[ \t]*//.*$" nil t)
          (replace-match "}" nil t)))
    (end-of-line 1))))


(defun java-intuit-package ()
  "Insert a package declaration for this file, heuristically."
  (interactive)
  (let* ((a (replace-regexp-in-string "^.*src/" "" (directory-file-name (file-name-directory buffer-file-name))))
         (b (replace-regexp-in-string "/" "." a)))
    (insert "package " b ";\n\n")))

(defun java-new-class ()
  (interactive)
  (goto-char (point-max))
  (java-intuit-package)
  (goto-char (point-max))
  (goto-char 
   (let ((p)
         (classname (replace-regexp-in-string "\\.java" "" (file-name-nondirectory buffer-file-name))))
     (insert "\n")
     (insert "public class " classname " {\n\n")
     (insert "  " classname "(")
     (setq p (point))
     (insert ") {\n")
     (insert "\n  }\n}")
     p)))

(defun java-sanitize () 
  "Fix spacing on some common Java idioms"
  (interactive)
  (dolist (pair '(("if(" . "if (")
                  ("while(" . "while (")
                  ("for(" . "for (")
                  ("catch(" . "catch (")
                  ("( " . "(")
                  (" )" . ")")
                  ("}catch" . "} catch")
                  ("mLogger" . "logger")))
    (save-excursion
      (goto-char (point-min))
      (replace-string-noninteractive (car pair) (cdr pair))))
  (save-excursion
    (goto-char (point-min))
    (delete-trailing-whitespace))
  (save-excursion
    (goto-char (point-min))
    (remove-trailing-brace-comments))
  (c-set-style "gnu")
  (save-excursion
    (indent-region (point-min) (point-max))))

(provide 'java-hacks)
