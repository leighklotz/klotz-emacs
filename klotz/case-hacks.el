;;;
;;; https://stackoverflow.com/a/27422814
;;;

(require 'string-inflection)

;; Cycle between snake case, camel case, etc.
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
;; Force to CamelCase
(global-set-key (kbd "C-c c") 'string-inflection-camelcase)
;; Force to lowerCamelCase
(global-set-key (kbd "C-c l") 'string-inflection-lower-camelcase)
;; Cycle through Java styles
(global-set-key (kbd "C-c j") 'string-inflection-java-style-cycle)

(provide 'case-hacks)
