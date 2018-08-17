;;; ~/.emacs.d/config/lang/java.el: settings for Java

(require 'java-imports)

;; Java hooks
(add-hook 'jdee-mode-hook (lambda () (flycheck-mode -1)))

(setq jdee-server-dir "~/.emacs.d/jdee/jars/")
