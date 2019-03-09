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
  :hook (latex-mode . (lambda ()
                        (define-key LaTeX-mode-map (kbd "$") 'self-insert-command))))

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

(defun my-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`my-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment my-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(defun my-LaTeX-setup-auto-fill ()
  "This function turns on auto-fill-mode and sets the function
used to fill a paragraph to `my-LaTeX-auto-fill-function'."
  (auto-fill-mode)
  (setq auto-fill-function 'my-LaTeX-auto-fill-function))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-setup-auto-fill)

;; Faces
(require 'font-latex)
(setq font-latex-fontify-sectioning 'color)
;(eval-after-load "font-latex"
;  (progn
    (set-face-attribute 'font-latex-bold-face nil
                        :foreground nil)
    (set-face-attribute 'font-latex-italic-face nil
                        :foreground nil
                        :italic t)
    (set-face-attribute 'font-latex-math-face nil
                        :family "Inconsolata")
(add-hook 'LaTeX-mode-hook 'variable-pitch-mode)
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (face-remap-add-relative 'font-lock-keyword-face '(:family "Monospace"))))

(provide 'wec-latex)
;;; wec-latex.el ends here
