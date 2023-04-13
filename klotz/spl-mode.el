;;;-*-Emacs-Lisp-*-

;;; https://chat.openai.com/chat/50227b46-7629-49db-b6ba-49fe71df3ea3: Please show an implemention of an Emacs major mode for editing Splunk SPL queries. Provide at least syntax highlighting. 
;;; https://www.omarpolo.com/post/writing-a-major-mode.html
;; https://emacs.stackexchange.com/questions/20712/how-to-add-complex-syntax-highlighting-in-a-minor-mode

(require 'font-lock)

;; Define the keywords for SPL syntax highlighting
(setq spl-font-lock-keywords
  (let* ((keywords '("NOT" "abs" "abstract" "accum" "acos" "acosh" "addcoltotals" "addinfo" "addtotals" "analyzefields" "and" "anomalies" "anomalousvalue" "append" "appendcols" "appendpipe" "arules" "as" "asin" "asinh" "associate" "atan" "atan2" "atanh" "audit" "autoregress" "avg" "bucket" "bucketdir" "by" "case" "ceiling" "chart" "cidrmatch" "cluster" "coalesce" "collect" "commands" "concurrency" "contingency" "convert" "correlate" "cos" "cosh" "count" "crawl" "datamodel" "dbinspect" "dbxlookup" "dbxquery" "dedup" "delete" "delta" "diff" "dispatch" "distinct_count" "earliest" "earliest_time" "erex" "estdc" "estdc_error" "eval" "eventcount" "eventstats" "exact" "exp" "extract" "false" "false)\b" "fieldformat" "fields" "fieldsummary" "file" "filldown" "fillnull" "findtypes" "first" "floor" "folderize" "foreach" "format" "from" "gauge" "gentimes" "geostats" "head" "highlight" "history" "hypot" "if" "in" "input" "inputcsv" "inputlookup" "iplocation" "isbool" "isint" "isnotnull" "isnull" "isnum" "isstr" "join" "kmeans" "kvform" "last" "latest" "latest_time" "len" "like" "list" "ln" "loadjob" "localize" "localop" "log" "lookup" "lower" "ltrim" "makecontinuous" "makemv" "makeresults" "map" "match" "max" "md5" "mean" "median" "metadata" "metasearch" "min" "mode" "mstats" "multikv" "multisearch" "mvappend" "mvcombine" "mvcount" "mvdedup" "mvexpand" "mvfilter" "mvfind" "mvindex" "mvjoin" "mvrange" "mvsort" "mvzip" "nomv" "now" "null" "nullif" "or" "outlier" "output" "outputcsv" "outputlookup" "outputnew)" "outputnew)\b" "outputtext" "over" "overlap" "per_day" "per_hour" "per_minute" "per_second" "percentile" "pi" "pivot" "pow" "predict" "printf" "random" "range" "rangemap" "rare" "rate" "regex" "relative_time" "relevancy" "reltime" "rename" "replace" "rest" "return" "reverse" "rex" "round" "rtorder" "rtrim" "run" "savedsearch" "script" "scrub" "search" "searchmatch" "searchtxn" "selfjoin" "sendemail" "set" "setfields" "sha1" "sha256" "sha512" "sichart" "sigfig" "sin" "sinh" "sirare" "sistats" "sitimechart" "sitop" "sort" "spath" "split" "sqrt" "stats" "stdev" "stdevp" "strcat" "streamstats" "strftime" "strptime" "substr" "sum" "sumsq" "table" "tags" "tail" "tan" "tanh" "time" "timechart" "timewrap" "tonumber" "top" "tostring" "transaction" "transpose" "trendline" "trim" "true" "tscollect" "tstats" "typeahead" "typelearner" "typeof" "typer" "uniq" "untable" "upper" "urldecode" "validate" "values" "var" "varp" "where" "x11" "xmlkv" "xmlunescape" "xpath" "xyseries"))
         (keyword-regexp (regexp-opt keywords 'symbols)))
    `((,keyword-regexp . font-lock-keyword-face)
      ;; Highlight command options starting with a '-' or '|'
      ;; this doesn't work
      ("- *[a-zA-Z0-9_]+" . font-lock-keyword-face)
      ("| *[a-zA-Z0-9_]+" . font-lock-keyword-face))))

;; Define the mode map (keybindings) for Spl mode
(defvar spl-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `spl-mode'.")

;; Define the Splunk SPL major mode
(define-derived-mode spl-mode prog-mode "SPL"
  "Major mode for editing Splunk SPL queries."
  (setq-local font-lock-defaults
              '((spl-font-lock-keywords)
                nil nil nil nil
		;; https://emacs.stackexchange.com/questions/20712/how-to-add-complex-syntax-highlighting-in-a-minor-mode
		;; says to remove this
		; (font-lock-syntactic-face-function . (prog-mode-font-lock-syntactic-face-function)
		)))

(provide 'spl-mode)

