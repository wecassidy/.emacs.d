;;; wec-csv.el --- settings for CSV files            -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(add-hook 'csv-mode-hook
          (lambda ()
            (csv-align-fields nil (point-min) (point-max))
            (csv-header-line)))

(provide 'wec-csv)
;;; wec-csv.el ends here
