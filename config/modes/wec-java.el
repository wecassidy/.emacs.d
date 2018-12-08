;;; wec-java.el --- settings for Java
;;; Commentary:
;;; Code:
;; Java hooks
(require 'java-imports)
(add-hook 'jdee-mode-hook (lambda () (flycheck-mode -1)))

(setq jdee-server-dir "~/.emacs.d/jdee/jars/")

(provide 'wec-java)
;;; wec-java.el ends here
