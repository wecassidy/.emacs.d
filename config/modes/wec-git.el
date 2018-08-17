;;; ~/.emacs.d/config/git.el --- settings for git and magit

;;; Commentary:
;; See also ~/.gitconfig and ~/.gitignore_global

;;; Code:
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq git-commit-major-mode 'org-mode)

(provide 'git.el)
;;; git.el ends here
