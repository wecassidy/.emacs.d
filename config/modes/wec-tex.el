;;; wec-tex.el --- settings for TeX and Asymptote
;;; Commentary:
;;; Code:
;; TeX
(require 'latex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'write-file-functions
                                          'delete-trailing-whitespace)))

(TeX-PDF-mode t)

;; Automatically pair LaTeX tags ($)
(add-hook 'LaTeX-mode-hook (lambda () (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))

;; Citations: ebib, BibLaTeX, and RefTeX
(require 'ebib)
(global-set-key "\C-ce" 'ebib)
(setq ebib-layout 'custom)
(setq ebib-bibtex-dialect 'biblatex)
(setq ebib-autogenerate-keys t)
(add-hook 'ebib-mode-hook '(add-to-list 'ebib-file-associations
                                        '("pdf" . "evince")))
(setq ebib-keywords-file "ebib-keywords.txt")
(setq ebib-index-columns '(("Author/Editor" 40 t)
                           ("Year" 6 t)
                           ("Title" 50 t)))

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

;; Key bindings
(defun tex-bold ()
  (interactive)
  (TeX-font nil ?\C-b))

(defun tex-italic ()
  (interactive)
  (TeX-font nil ?\C-e))

(defun tex-smallcaps ()
  (interactive)
  (TeX-font nil ?\C-c))

(defun tex-typewriter ()
  (interactive)
  (TeX-font nil ?\C-t))

(defun tex-bindings-hook ()
  (local-set-key "\C-cb" 'tex-bold)
  (local-set-key "\C-ci" 'tex-italic)
  (local-set-key "\C-cs" 'tex-smallcaps)
  (local-set-key "\C-ct" 'tex-typewriter))

(add-hook 'LaTeX-mode-hook 'tex-bindings-hook)

;; MLA
(defun mla-env ()
    (TeX-add-style-hook
     "latex"
     (lambda ()
       (LaTeX-add-environments
        '("mla" "First" "Last" "Teacher" "Course" "Date" "Title")))))

(add-hook 'LaTeX-mode-hook 'mla-env)

;; Autocomplete
(require 'company)
(require 'company-auctex)
(company-auctex-init)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'company-backends 'company-math-symbols-unicode)
                             (add-to-list 'company-backends 'company-math-symbols-latex)
                             (add-to-list 'company-backends 'company-latex-commands)))

;; Asymptote
(add-to-list 'load-path "/usr/local/share/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;; Faces
(setq font-latex-fontify-sectioning 'color)

(provide 'wec-tex)
;;; wec-tex.el ends here
