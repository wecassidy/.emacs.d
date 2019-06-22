;;; wec-web.el --- settings for web technologies (HTML, CSS, JS)

;;; Commentary:
;; web-mode, mostly, with a hint of jinja

;;; Code:
(require 'web-mode)
(defun web-mode-custom-hook ()
  "Hooks for Web mode."

  ;; Indentation
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)

  ;; CSS colours
  (setq web-mode-enable-css-colorization t))

(add-hook 'web-mode-hook 'web-mode-custom-hook)
(add-hook 'web-mode-hook 'emmet-mode)

(setq json-reformat:indent 2)
(setq js-indent-level 2)

;; Use web-mode for HTML, CSS, JS, and PHP files
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-hook 'scss-mode-hook (lambda () (setq css-indent-offset 2)))
(add-hook 'scss-mode-hook 'rainbow-mode)

;;; Jinja2
;; Use Emmet
(add-hook 'jinja2-mode-hook 'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

(provide 'wec-web)
;;; wec-web.el ends here
