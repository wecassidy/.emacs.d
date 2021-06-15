;;; wec-py.el --- settings for Python files              -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Use IPython 3
(setq python-shell-interpreter "~/.local/bin/ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

;; Docstring minor mode
(add-hook 'python-mode-hook 'python-docstring-mode)
(setq python-docstring-sentence-end-double-space nil)
(setq numpydoc-insert-examples-block nil)

;; Autocomplete
;(require 'company)
(add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))

;; Execute Pylint with Python 3
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
  (setq flycheck-pycheckers-checkers '(pylint)))
  (setq flycheck-python-pylint-executable "/usr/bin/python3")

;; Show source code outline tree with C-c x
(eval-after-load "python"
  '(define-key python-mode-map "\C-cx" 'jedi-direx:switch-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

;; Automatically blacken code
(add-hook 'python-mode-hook 'blacken-mode)

(provide 'wec-py)
;;; wec-py.el ends here
