;;; wec-auto-complete.el --- company -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete configuration of auto-complete.  Currently using
;; company-mode.  See modes/*.el for which modes load which backends.

;;; Code:
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'company-statistics-mode) ; list more commonly used completions first

(global-set-key (kbd "C-<tab>") 'company-manual-begin) ; complete on C-tab

;; Show help in auto-complete
(require 'company-quickhelp)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)) ; show help when companying with C-c h
(setq company-quickhelp-use-propertized-text t)

;; Make the popup colours nicer
(color-theme-sanityinc-solarized--with-colors
 'dark
 (custom-set-faces
  `(company-tooltip ((t (:inherit default :background ,alt-background))))
  `(company-scrollbar-bg ((t (:background ,faint))))
  `(company-scrollbar-fg ((t (:background ,strong))))
  `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
  `(company-tooltip-common ((t (:foreground ,blue))))
  `(company-preview ((t (:foreground ,normal :background ,background :underline t))))))

(provide 'wec-auto-complete)
;;; wec-auto-complete.el ends here
