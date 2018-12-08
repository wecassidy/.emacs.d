;;; wec-git.el --- settings for git and magit

;;; Commentary:
;; See also ~/.gitconfig and ~/.gitignore_global

;;; Code:
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq git-commit-major-mode 'org-mode)
(setq magit-completing-read-function 'magit-ido-completing-read) ; Use ido

(provide 'wec-git)
;;; wec-git.el ends here
