;;;-*- Mode: EMACS-LISP -*-

;;; https://chat.openai.com/c/7d4fde88-baa7-4288-9b1c-b3feb34713d2
;;; GPT-4

(require 'font-lock)
(require 'smie)

;; Define the keywords for SPL syntax highlighting
(defvar splunk-font-lock-keywords
  (let* ((transforming-keywords (split-string "search stats table timechart sort top dedup head join spath transaction tstats"))
         (eval-keywords (split-string "eval where rex fields"))
         (macro-names (split-string "macro1 macro2 macro3"))
         (constants (split-string "true false null"))
         (transforming-regexp (regexp-opt transforming-keywords 'words))
	 (builtin-regexp (regexp-opt (split-string "abstract accum addcoltotals addinfo addtotals analyzefields anomalies anomalousvalue append appendcols appendpipe arules associate audit autoregress bucket bucketdir chart cluster collect concurrency contingency convert correlate crawl datamodel dbinspect dbxquery dbxlookup dedup delete delta diff dispatch erex eval eventcount eventstats extract fieldformat fields fieldsummary file filldown fillnull findtypes folderize foreach format from gauge gentimes geostats head highlight history input inputcsv inputlookup iplocation join kmeans kvform loadjob localize localop lookup makecontinuous makemv makeresults map metadata metasearch multikv multisearch mvcombine mvexpand nomv outlier outputcsv outputlookup outputtext overlap pivot predict rangemap rare regex relevancy reltime rename replace rest return reverse rex rtorder run savedsearch script scrub search searchtxn selfjoin sendemail set setfields sichart sirare sistats sitimechart sitop sort spath stats strcat streamstats table tags tail timechart top transaction transpose trendline tscollect tstats typeahead typelearner typer uniq untable where x11 xmlkv xmlunescape xpath xyseries")))
         (eval-regexp (regexp-opt eval-keywords 'words))
         (macro-regexp (concat "`" (regexp-opt macro-names t)))
         (constants-regexp (regexp-opt constants 'words))
	 (comment-regexp "`\\{3\\}\\(?:.\\|\n\\)*?`\\{3\\}"))
    `((,comment-regexp . font-lock-comment-face)
      (,transforming-regexp . font-lock-keyword-face)
      (,builtin-regexp . font-lock-function-name-face)
      (,eval-regexp . font-lock-function-name-face)
      (,macro-regexp . font-lock-preprocessor-face)
      (,constants-regexp . font-lock-constant-face)
      ;; Highlight pipe character
      ("|" . font-lock-builtin-face))))

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
