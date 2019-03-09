;;; wec-org.el --- org-mode customizations
;;; Commentary:
;; Useful global shortcuts, show symbols, custom TODO keywords

;;; Code:
(require 'org)

;; Display symbols (e. g. \alpha) as unicode
(setq org-pretty-entities t)

;; Bug tracking keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "BUG(b)" "WORKING(w)" "|" "FIXED(f)")))
(setq org-use-fast-todo-selection 'prefix) ; Use a prefix argument to select todo keywords by single-letter
(color-theme-sanityinc-solarized--with-colors
 'dark
 (setq org-todo-keyword-faces
       `(("WORKING" . yellow))))

(provide 'wec-org)
;;; wec-org.el ends here
