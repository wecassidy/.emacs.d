;;; wec-latex.el --- settings for TeX and Asymptote
;;; Commentary:
;;; Code:
;; TeX
(use-package auctex
  :mode
  ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
    (add-to-list 'display-line-numbers-exempt-modes 'latex-mode)
;  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-hook 'LaTeX-mode-hook #'latex-font-setup)
  :bind (("C-c b" . (lambda () (interactive) (TeX-font nil ?\C-b))) ; Bold
         ("C-c i" . (lambda () (interactive) (TeX-font nil ?\C-e))) ; Italic
         ("C-c s" . (lambda () (interactive) (TeX-font nil ?\C-c))) ; Smallcaps
         ("C-c t" . (lambda () (interactive) (TeX-font nil ?\C-t)))) ; Typewriter
  :hook (LaTeX-mode . latex-math-mode)
  :config
  (use-package latex-extra
    :hook LaTeX-mode)
  (put 'TeX-insert-quote 'delete-selection nil)
  (put 'TeX-insert-dollar 'delete-selection nil)
  (use-package latex-math-mode
    :hook LaTeX-mode)
  (use-package company-auctex
    :config
    (company-auctex-init)
    :hook (LaTeX-mode . (lambda ()
                          (add-to-list 'company-backends 'company-math-symbols-unicode)
                          (add-to-list 'company-backends 'company-math-symbols-latex)
                          (add-to-list 'company-backends 'company-latex-commands))))
  (tex-pdf-mode t)
  (use-package smartparens-latex)
  (add-hook 'LaTeX-mode-hook #'visual-fill-column-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode) (TeX-fold-buffer))))

(defun latex-font-setup ()
  "Set up fonts for latex editing: turn on variable pitch, set
  some fonts to monospace."
  (variable-pitch-mode t)
  (face-remap-add-relative 'default nil :height 140)
  (face-remap-add-relative 'fixed-pitch nil :height 140)
  (dolist (face '(font-latex-math-face
                  font-latex-sedate-face
                  font-lock-function-name-face
                  font-lock-keyword-face
                  font-lock-constant-face
                  font-latex-sedate-face))
    (face-remap-add-relative face nil
                        :inherit 'fixed-pitch)))

;; Citations: ebib, BibLaTeX, and RefTeX
(use-package ebib
  :bind (("C-c e" . ebib))
  :init
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-autogenerate-keys t)
  (setq ebib-keywords-file "ebib-keywords.txt")
  (setq ebib-layout 'custom)
  (setq ebib-index-columns '(("Author/Editor" 40 t)
                             ("Year" 6 t)
                             ("Title" 50 t)))
  :config
  (add-to-list 'ebib-file-associations '("pdf" . "evince")))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-ref-style-default-list '("Default" "Hyperref"))

(defvar my-reftex-cite-format-helpstrings
  '((biblatex
     (?\C-m . "author year/title")
     (?C    . "year/title")
     (?t    . "author (year/title)")
     (?T    . "")
     (?p    . "(author year/title)")
     (?P    . "(year/title)")
     (?f    . "\\cite in footnote")
     (?s    . "\\footcite in body, \\parencite in footnote")
     (?u    . "move punctuation as required")
     (?U    . "move punctuation as required, use starred form")
     (?a    . "author(s)")
     (?A    . "author (et al.)")
     (?i    . "title (shortened, if available)")
     (?I    . "full title")
     (?y    . "year")
     (?Y    . "all date information")
     (?n    . "add to bibliography"))))

(defun my-get-reftex-cite-help (format key)
  "Return the correct citation format example for the given format and key"
  (if (assq key (assq format my-reftex-cite-format-helpstrings))
      (cdr (assq key (assq format my-reftex-cite-format-helpstrings)))
    ""))

(provide 'wec-latex)
;;; wec-latex.el ends here
