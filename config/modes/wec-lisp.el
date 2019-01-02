;;; wec-lisp.el --- elisp, cl, and slime             -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Set the lisp program
(setq inferior-lisp-program "/usr/bin/sbcl")

;; Load slime-fancy
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))

(provide 'wec-lisp)
;;; wec-lisp.el ends here
