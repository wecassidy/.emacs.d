;;; wec-git.el --- settings for git and magit

;;; Commentary:
;; See also ~/.gitconfig and ~/.gitignore_global

;;; Code:
(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status))
  :init
  (setq git-commit-major-mode 'org-mode)
  (setq magit-completing-read-function 'magit-ido-completing-read))

(provide 'wec-git)
;;; wec-git.el ends here
