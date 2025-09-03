;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

(use-feature! printing
  :defer 10
  :commands (pr-update-menus)
  :config
  ;; EPSON WF-3520
  (setopt printer-name "LABORTTY")
  ;; (setopt lpr-switches '())
  (pr-update-menus))

(provide 'ceamx-init-printing)
;;; ceamx-init-printing.el ends here
