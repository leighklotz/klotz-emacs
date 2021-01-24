(define-minor-mode k2-minor-mode
  "Minor mode for k2-rig-control context diffs.
\\{k2-minor-mode-map}"
  nil " Diff" nil
  ;; FIXME: setup font-lock
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-hooks 'diff-write-contents-hooks)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t)))
