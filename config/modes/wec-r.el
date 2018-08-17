;;; wec-r.el --- settings for R (ess-mode)               -*- lexical-binding: t; -*-

;;; Commentary:
;; Remaps C-up and C-down to match input in history in inferior ESS
;; buffers.  Remaps C-x t to complete a filename in inferior ESS
;; buffers.

;;; Code:
(add-hook 'inferior-ess-mode-hook
          '(lambda nil
             (define-key inferior-ess-mode-map [\C-up]
               'comint-previous-matching-input-from-input)
             (define-key inferior-ess-mode-map [\C-down]
               'comint-next-matching-input-from-input)
             (define-key inferior-ess-mode-map [\C-x \t]
               'comint-dynamic-complete-filename)))



(provide 'wec-r)
;;; wec-r.el ends here
