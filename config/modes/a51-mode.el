;;; a51-mode.el --- a51 assembler mode                -*- lexical-binding: t; -*-
;; Author: Wesley Cassidy <wec@wec-manjaro>
;;; Commentary:
;;; Code:

(defgroup a51 nil
  "Mode for editing A51 assembler for the 8051 processor."
  :group 'languages)

(defcustom a51-mode-hook nil
  "Normal hook run when entering A51 mode."
  :type 'hook
  :group 'a51)

(defcustom a51-instruction-column 12
  "Column for instructions."
  :type '(integer)
  :group 'a51)

;; Syntax highlighting regexes
(defcustom a51-directives-list
  '("segment" "rseg" "cseg" "dseg" "bseg" "iseg" "xseg"
    "equ" "set" "bit" "code" "data" "idata" "xdata"
    "lit" "db" "dw" "dd" "dbit" "ds" "dsb" "dsw" "dsd"
    "proc" "endp" "label" "public" "extrn" "extern" "name"
    "org" "even" "using" "end")
  "List of A51 assembler directives."
  :type '(repeat regexp)
  :group 'a51)

(defcustom a51-controls-regexp
  "^[[:blank:]]*\\$[A-Z0-9]+"
  "Regexp to match A51 assembler controls."
  :type '(regexp)
  :group 'a51)
(defcustom a51-mnemonics-list
  '("acall" "add" "addc" "ajmp" "anl" "cjne" "clr" "cpl" "da" "dec" "div"
    "djnz" "lcall" "ljmp" "inc" "jb" "jbc" "jc" "jmp" "jnb" "jnc" "jnz" "jz"
    "mov" "movc" "movx" "mul" "nop" "orl" "pop" "push" "ret" "reti"
    "rl" "rlc" "rr" "rrc" "setb" "sjmp" "subb" "swap" "xch" "xhcd" "xrl")
  "8051 machine instructions"
  :type '(repeat string)
  :group 'a51)
(defcustom a51-registers-list
  `("A" "DPTR" "PC" "C" "AB"
    ,@(let (Rn)
        (dotimes (i 16)
          (setq Rn (append Rn (list (format "R%d" i)))))
        Rn)
    ,@(let (ARn)
        (dotimes (i 8)
          (setq ARn (append ARn (list (format "AR%d" i)))))
        ARn)
    ,@(let (WRn)
        (dotimes (i 31)
          (unless (= i 1)
            (setq WRn (append WRn (list (format "WR%d" i))))))
        WRn)
    ,@(let (DRn)
        (dotimes (i 29)
          (unless (or (= i 1) (= i 2) (= i 3))
          (setq DRn (append DRn (list (format "DR%d" i))))))
        DRn)
    "DR56" "DR60")
  "8051 registers - reserved."
  :type '(repeat string)
  :group 'a51)
(defcustom a51-label-regexp "\\(^[[:blank:]]*[a-zA-Z_?][a-z-A-Z0-9_?]*[[:blank:]]*\\):"
  "Regular expression to match labels."
  :type '(regexp)
  :group 'a51)

(defvar a51-directives-regexp (regexp-opt a51-directives-list 'words))
(defvar a51-mnemonics-regexp (regexp-opt a51-mnemonics-list 'words))
(defvar a51-registers-regexp (regexp-opt a51-registers-list 'words))

(defvar a51-font-lock-defaults
  `(((,a51-label-regexp 1 font-lock-type-face)
     (,a51-controls-regexp . font-lock-function-name-face)
     (,a51-directives-regexp . font-lock-keyword-face)
     (,a51-mnemonics-regexp . font-lock-builtin-face)
     (,a51-registers-regexp . font-lock-constant-face))
    nil
    t)
  "Syntax highlighting settings for A51.")

(defvar a51-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ":" 'a51-colon)
    (define-key map (kbd "C-c C-a C-a") 'a51-align-dwim)
    (define-key map (kbd "C-c C-a C-r") 'a51-align-region)
    (define-key map (kbd "C-c C-a C-l") 'a51-align-line)
    (define-key map (kbd "C-c C-a C-b") 'a51-align-buffer)
    map)
  "Keymap for a51-mode.")

;;;###autoload
(define-derived-mode a51-mode prog-mode "A51 assembler"
  "Major mode for editing A51 assembly. Heavily patterned off
asm-mode, but hopefully faster."
  (setq-local indent-line-function #'a51-indent-line)
  (setq-local tab-width a51-instruction-column)

  (use-local-map (nconc (make-sparse-keymap) a51-mode-map))

  (setq-local font-lock-defaults a51-font-lock-defaults)
  ;; Comments
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (modify-syntax-entry ?\; "< b" a51-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" a51-mode-syntax-table)

  ;; Single quotes only
  (modify-syntax-entry ?\' "\"" a51-mode-syntax-table)
  (modify-syntax-entry ?\" "w" a51-mode-syntax-table)

  (modify-syntax-entry ?$ "'" a51-mode-syntax-table) ; Assembler controls
  (modify-syntax-entry ?? "_" a51-mode-syntax-table) ; Symbols can contain ?
  )


(defun a51-indent-line ()
  "Auto-indent the current line using A51 semantics."
  (interactive)
  (let* ((savep (point))
        (indent (condition-case nil
                    (save-excursion
                      (beginning-of-line)
                      (skip-chars-forward "[:blank:]")
                      (if (>= (point) savep) (setq savep nil))
                      (max (a51-calculate-indentation) 0))
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun a51-calculate-indentation ()
  "Calculate indentation in A51 mode. Assumes point is on the
first non-blank character."
  (let ((case-fold-search t))
    (or
     (and (looking-at a51-label-regexp) 0)
     (and (looking-at a51-controls-regexp) 0)
     (and (a51-directive-line-p) 0)
     (and (looking-at ";;") 0)
     (indent-next-tab-stop 0))))

(defun a51-align-dwim ()
  "Align labels, instructions, and comments to the appropriate
columns. Align all lines in region if it is active, otherwise
align the current line. See `a51-align-line' for details."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (a51-align-region (region-beginning) (region-end))
      (a51-align-line))))

(defun a51-align-region (start end)
  "Align labels, instructions, and comments to the appropriate
columns for every line in the region. See `a51-align-line' for
details."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (<= (point) end)
      (a51-align-line)
      (forward-line)
      (beginning-of-line))))

(defun a51-align-buffer ()
  "Align labels, instructions, and comments to the appropriate
columns for every line in the buffer. See `a51-align-line' for
details."
  (interactive)
  (a51-align-region (point-min) (point-max)))

(defun a51-align-line ()
  "Align labels, instructions, and comments to the appropriate
columns."
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (beginning-of-line)
      (cond ((looking-at ";;;") (indent-line-to 0))
            ((looking-at ";;") (indent-line-to a51-instruction-column))
            ((looking-at ";") (indent-line-to comment-column))

            ((or (looking-at a51-controls-regexp) (a51-directive-line-p))
             (indent-line-to 0)
             (skip-syntax-forward "^<" (line-end-position))
             (when (at-comment-start-p) (tab-to-column comment-column)))

            (t
             (when (looking-at a51-label-regexp)
               (indent-line-to 0)
               (skip-chars-forward "^:")
               (forward-char)
               (when (> (current-column) a51-instruction-column) (newline-and-indent)))
             (skip-chars-forward "[:blank:]")

             (when (looking-at a51-mnemonics-regexp)
               (delete-horizontal-space)
               (tab-to-column a51-instruction-column))
             (skip-syntax-forward "^<" (line-end-position))
             (when (at-comment-start-p) (tab-to-column comment-column)))))))

(defun tab-to-column (col)
  "Insert enough space to place point at `col'. If point is
already beyond `col', do nothing."
  (when (< (current-column) col) (insert-char ?\s (- col (current-column)))))

(defun at-comment-start-p ()
  "Return t if point is at a comment start character."
  (eq 11 (syntax-class (syntax-after (point)))))

(defun a51-directive-line-p ()
  "Check if the current line is an assembler directive. If so,
return point where the directive was found. Else return nil."
  (beginning-of-line)
  (and
   (re-search-forward a51-directives-regexp (line-end-position) t)
   (if (not (in-comment-p)) (point) nil)))

(defun in-comment-p ()
  "Return non-nil if point is in a comment."
  (nth 4 (syntax-ppss)))

(defun a51-colon ()
  "Move labels to the left margin and move point to
the instruction column. If the label is wider than the
instruction column, add a newline."
  (interactive)
  (call-interactively 'self-insert-command)
  (let ((at-label-p (save-excursion
                      (backward-char)
                      (skip-syntax-backward "w_") ; Move to start of symbol
                      (skip-chars-backward "[:blank:]")
                      (looking-at a51-label-regexp))))
    (when at-label-p
      ;; Kill indentation before the label
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space))

      ;; Move to the instruction column, inserting a newline if needed.
      (if (>= (current-column) a51-instruction-column)
          (newline-and-indent)
        (delete-horizontal-space)
        (tab-to-tab-stop)))))

(provide 'a51-mode)
;;; a51-mode.el ends here
