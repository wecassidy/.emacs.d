;;; wec-sh.el --- settings for shell scripts
;;; Commentary:
;;; Code:
;; Keyboard shortcut to launch a terminal
(global-set-key (kbd "C-c C-t") 'ansi-term)

(setq sh-basic-offset 2)
(setq sh-indentation 2)

(provide 'wec-sh)
;;; wec-sh.el ends here
