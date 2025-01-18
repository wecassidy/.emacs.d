;;; wec-py.el --- settings for Python files              -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Use IPython 3
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

;; Docstring minor mode
;(add-hook 'python-mode-hook 'python-docstring-mode)
;(setq python-docstring-sentence-end-double-space nil)
(setq numpydoc-insert-examples-block nil)

;; Autocomplete
;; (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))

;; Execute Pylint with Python 3
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
;;   (setq flycheck-pycheckers-checkers '(pyflakes pylint)))
  ;(setq flycheck-python-pylint-executable "/usr/bin/python3")
;(require 'flycheck-pyflakes)
(require 'flycheck)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

;; Show source code outline tree with C-c x
;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-cx" 'jedi-direx:switch-to-buffer))
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)

;; Automatically blacken code
(add-hook 'python-mode-hook 'blacken-mode)
(setq blacken-only-if-project-is-blackened t)

(provide 'wec-py)
;;; wec-py.el ends here
