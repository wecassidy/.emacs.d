;;; wec-rust.el --- rust-mode                        -*- lexical-binding: t; -*-
;;; Code:

(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq rust-format-on-save t)))

(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

(provide 'wec-rust)
;;; wec-rust.el ends here
