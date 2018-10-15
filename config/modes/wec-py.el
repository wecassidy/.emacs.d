;;; wec-py.el --- settings for Python files              -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(require 'python)

;; Use IPython 3
(setq python-shell-interpreter "~/.local/bin/ipython3"
      python-shell-interpreter-args "--simple-prompt -i")

;; Docstring minor mode
(require 'python-docstring)
(add-hook 'python-mode-hook 'python-docstring-mode)
(setq python-docstring-sentence-end-double-space nil)

;; Autocomplete
;; (require 'jedi)
;; (setq jedi:server-command
;;       (list "/usr/bin/python3" jedi:server-script))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; Execute Pylint with Python 3
(require 'flycheck-pycheckers)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
  (setq flycheck-pycheckers-checkers '(pep8)))
; (setq flycheck-python-pylint-executable "/usr/bin/python3")

;; Show source code outline tree with C-c x
;; (eval-after-load "python"
;;   '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)

(provide 'wec-py)
;;; wec-py.el ends here
