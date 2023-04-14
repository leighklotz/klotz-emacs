;;;-*- Mode: EMACS-LISP -*-

;;; https://chat.openai.com/c/7d4fde88-baa7-4288-9b1c-b3feb34713d2
;;; GPT-4
;;; https://github.com/arcsector/vscode-splunk-search-syntax/blob/master/syntaxes/splunk_search.tmLanguage
;;; list of SPL commands

(require 'font-lock)
(require 'smie)

;; Define the keywords for SPL syntax highlighting
(defvar splunk-font-lock-keywords
  (let* ((constants-regexp (regexp-opt (split-string "true false null") t))
	 ;; (macro-names (split-string "macro1 macro2 macro3"))
	 (builtin-regexp (regexp-opt (split-string "abstract accum addcoltotals addinfo addtotals analyzefields anomalies anomalousvalue append appendcols appendpipe arules associate audit autoregress bucket bucketdir chart cluster collect concurrency contingency convert correlate crawl datamodel dbinspect dbxquery dbxlookup dedup delete delta diff dispatch erex eval eventcount eventstats extract fieldformat fields fieldsummary file filldown fillnull findtypes folderize foreach format from gauge gentimes geostats head highlight history input inputcsv inputlookup iplocation join kmeans kvform loadjob localize localop lookup makecontinuous makemv makeresults map metadata metasearch multikv multisearch mvcombine mvexpand nomv outlier outputcsv outputlookup outputtext overlap pivot predict rangemap rare regex relevancy reltime rename replace rest return reverse rex rtorder run savedsearch script scrub search searchtxn selfjoin sendemail set setfields sichart sirare sistats sitimechart sitop sort spath stats strcat streamstats table tags tail timechart top transaction transpose trendline tscollect tstats typeahead typelearner typer uniq untable where x11 xmlkv xmlunescape xpath xyseries")))
	 (eval-functions-regexp (regexp-opt (split-string "abs acos acosh asin asinh atan atan2 atanh case cidrmatch ceiling coalesce commands cos cosh exact exp floor hypot if in isbool isint isnotnull isnull isnum isstr len like ln log lower ltrim match max md5 min mvappend mvcount mvdedup mvfilter mvfind mvindex mvjoin mvrange mvsort mvzip now null nullif pi pow printf random relative_time replace round rtrim searchmatch sha1 sha256 sha512 sigfig sin sinh spath split sqrt strftime strptime substr tan tanh time tonumber tostring trim typeof upper urldecode validate") 'words))
	 (macro-regexp "`\\(?:.\\ \n\\)*?`")
	 (transforming-functions (regexp-opt (split-string "avg count distinct_count estdc estdc_error eval max mean median min mode percentile range stdev stdevp sum sumsq var varp first last list values earliest earliest_time latest latest_time per_day per_hour per_minute per_second rate") 'words))
	 (comment-regexp "`\\{3\\}\\(?:.\\ \n\\)*?`\\{3\\}"))
    `((,comment-regexp . font-lock-comment-face)
      (,builtin-regexp . font-lock-function-name-face)
      (,eval-functions-regexp . font-lock-function-name-face)
      (,macro-regexp . font-lock-preprocessor-face)
      (,constants-regexp . font-lock-constant-face)
      ;; Highlight pipe character
      ("|" . font-lock-keyword-face))))

;; SMIE grammar and indentation rules
(defvar splunk-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc "|")))))

(defun splunk-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) 2)
    (`(:before . ,(or "(" "[" "{")) (smie-rule-parent))
    (`(:after . ,(or ")" "]" "}")) (smie-rule-parent))
    (`(:before . "|") (if (smie-rule-bolp) (smie-rule-parent) (smie-rule-separator kind)))))

;; Define the mode map (keybindings) for Splunk mode
(defvar splunk-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `splunk-mode'.")

;; Define the Splunk major mode
(define-derived-mode splunk-mode prog-mode "Splunk"
  "Major mode for editing Splunk SPL queries."
  (setq-local font-lock-defaults '((splunk-font-lock-keywords)))
  (setq-local smie-grammar splunk-smie-grammar)
  (smie-setup splunk-smie-grammar #'splunk-smie-rules)
  (setq-local comment-start "```")
  (setq-local comment-end "```")
  (setq-local comment-end-can-be-escaped t))


(provide 'splunk-mode)
