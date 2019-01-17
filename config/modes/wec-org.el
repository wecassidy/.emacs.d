;;; wec-org.el --- org-mode customizations
;;; Commentary:
;; Useful global shortcuts, show symbols, custom TODO keywords

;;; Code:
(require 'org)

;; Useful keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

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

;; Minor modes
(add-hook 'org-mode-hook 'turn-on-flyspell) ; Check spelling
(add-hook 'org-mode-hook 'turn-on-auto-fill) ; Hard wrap

(provide 'wec-org)
;;; wec-org.el ends here
