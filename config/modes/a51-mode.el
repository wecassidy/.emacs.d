;;; a51-mode.el --- a51 assembler mode                -*- lexical-binding: t; -*-
;; Author: Wesley Cassidy <wec@wec-manjaro>
;;; Commentary:
;;; Code:

(defgroup a51 nil
  "Mode for editing A51 assembler for the 8051 processor."
  :group 'languages)

(defcustom a51-mode-hook nil
  "Normal hook run when entering A51 mode"
  :type 'hook
  :group 'a51)

(defcustom a51-indent-offset 4 "Number of columns to indent in a51-mode."
  :type '(integer)
  :group 'a51)

;; Syntax highlighting regexes
(defcustom a51-directives-list
  '("SEGMENT" "RSEG" "CSEG" "DSEG" "BSEG" "ISEG" "XSEG"
    "EQU" "SET" "BIT" "CODE" "DATA" "IDATA" "XDATA"
    "LIT" "DB" "DW" "DD" "DBIT" "DS" "DSB" "DSW" "DSD"
    "PROC" "ENDP" "LABEL" "PUBLIC" "EXTRN" "EXTERN" "NAME"
    "ORG" "EVEN" "USING" "END")
  "A51 assembler directives"
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

(defvar a51-font-lock-defaults
  `(
    ((,(regexp-opt a51-directives-list 'words) . font-lock-keyword-face)
     (,a51-controls-regexp . font-lock-function-name-face)
     (,(regexp-opt a51-mnemonics-list 'words) . font-lock-builtin-face)
     (,(regexp-opt a51-registers-list 'words) . font-lock-constant-face)
     (,a51-label-regexp 1 font-lock-type-face))
    nil
    t)
  "Syntax highlighting settings for A51.")

(defvar a51-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?\; "< b" a51-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" a51-mode-syntax-table)

    ;; Single quotes only
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "w")

    (modify-syntax-entry ?$ "'") ; assembler controls
    )
  "How a51-mode interprets special characters.")

;;;###autoload
(define-derived-mode a51-mode prog-mode "A51 assembler"
  "Major mode for editing A51 assembly. Heavily patterned off
asm-mode, but hopefully faster."
  (setq-local indent-line-function #'a51-indent-line)
  (setq-local tab-width a51-indent-offset)

  (setq-local font-lock-defaults a51-font-lock-defaults)
  (setq-local comment-start ";")
  (setq-local comment-end ""))

(defun a51-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let ((savep (point))
        (indent (condition-case nil
                    (save-excursion
                      (forward-line 0)
                      (skip-chars-forward "[:blank:]")
                      (if (>= (point) savep) (setq savep nil))
                      (max (a51-calculate-indentation) 0))
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun a51-calculate-indentation ()
  (or
   (and (looking-at a51-label-regexp) 0)
   (and (looking-at a51-controls-regexp) 0)
   (indent-next-tab-stop 0)))

(provide 'a51-mode)
;;; a51-mode.el ends here
