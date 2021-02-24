;;; wec-latex.el --- settings for TeX and Asymptote
;;; Commentary:
;;; Code:
;; TeX
(use-package latex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  :bind (("C-c b" . (lambda () (interactive) (TeX-font nil ?\C-b))) ; Bold
         ("C-c i" . (lambda () (interactive) (TeX-font nil ?\C-e))) ; Italic
         ("C-c s" . (lambda () (interactive) (TeX-font nil ?\C-c))) ; Smallcaps
         ("C-c t" . (lambda () (interactive) (TeX-font nil ?\C-t)))) ; Typewriter
  :config
  (use-package latex-math-mode
    :hook latex-mode)
  (use-package latex-extra-mode
    :hook latex-mode)
  (use-package company-auctex
    :config
    (company-auctex-init)
    :hook (latex-mode . (lambda ()
                          (add-to-list 'company-backends 'company-math-symbols-unicode)
                          (add-to-list 'company-backends 'company-math-symbols-latex)
                          (add-to-list 'company-backends 'company-latex-commands))))
  (tex-pdf-mode t)
  (use-package smartparens-latex))

;; Starting/opening LaTeX files
(defun make-latex (parent-dir name)
  "Make a new LaTeX project. `parent-dir' is the parent directory,
`name' is the name of the LaTeX"
  (interactive "DLocation:\nMName:")
  (let ((dir (expand-file-name name parent-dir))
        (latex-file (concat name ".tex")))
    (make-directory dir)
    (magit-init dir)
    (find-file (expand-file-name latex-file dir))))

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

(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

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

;; Don't autofill in certain environments (from
;; https://tex.stackexchange.com/a/69556/69731)
(defvar my-LaTeX-no-autofill-environments
  '("equation" "equation*" "tabular" "tabular*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(provide 'wec-latex)
;;; wec-latex.el ends here
