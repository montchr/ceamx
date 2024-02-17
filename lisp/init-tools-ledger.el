;;; init-tools-ledger.el --- Support for ledger      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; <https://hledger.org/>
;; <https://github.com/narendraj9/hledger-mode>
;; <https://hledger.org/1.30/hledger.html#journal>

;;; Code:

(require 'lib-common)

(defvar ceamx-ledger-dir (expand-file-name "~/ledger"))

(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands (hledger-enable-reporting)

  :preface
  (defun ceamx/hledger-next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defun ceamx/hledger-prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  ;; TODO: use theme values
  ;; (defface hledger-warning-face
  ;;   '((((background dark))
  ;;      :background "Red" :foreground "White")
  ;;     (((background light))
  ;;      :background "Red" :foreground "White")
  ;;     (t :inverse-video t))
  ;;   "Face for warning"
  ;;   :group 'hledger)

  ;; (defun +hledger-view-mode-hl-freq-accts-h ()
  ;;     "Highlight frequently-changing hledger accounts.
  ;; For use as a hook on `hledger-view-mode-hook'."
  ;;     (run-with-timer 1
  ;;       nil
  ;;       (lambda ()
  ;;         (when (equal hledger-last-run-command
  ;;                 "balancesheet")
  ;;           ;; highlight frequently changing accounts
  ;;           (highlight-regexp "^.*\\(savings\\|cash\\).*$")
  ;;           (highlight-regexp "^.*credit-card.*$"
  ;;             'hledger-warning-face)))))


  (defun +hledger-accounts-completions ()
    "Return completion candidates for hledger accounts."
    (when-let ((bounds (and (boundp 'hledger-accounts-cache)
                         (bounds-of-thing-at-point 'symbol))))
      (list (car bounds) (point) hledger-accounts-cache)))

  :init

  (def-hook! +hledger-accounts-capf-h () '(hledger-mode-hook)
    "Add hledger accounts to completion-at-point functions."
    (add-hook 'completion-at-point-functions 'hledger-completion-accounts))

  (setopt hledger-jfile (file-name-concat ceamx-ledger-dir "main.journal"))

  ;; (setq hledger-email-secrets-file (expand-file-name "secrets.el" emacs-assets-directory))

  ;; Expanded account balances in the overall monthly report are
  ;; mostly noise and do not convey any meaningful information.
  ;; (setopt hledger-show-expanded-report nil)

  ;; TODO:
  ;; (when (boundp 'my-hledger-service-fetch-url)
  ;;   (setq hledger-service-fetch-url
  ;;     my-hledger-service-fetch-url))

  (dolist (fn '(hl-line-mode
                 ;; FIXME: possibly performance issue? emacs at constant 20% but
                 ;; probably unrelated
                 ;; +hledger-view-mode-hl-freq-accts-h

                 ;; TODO: ???
                 ;; center-text-for-reading
                 ))
    (add-hook 'hledger-view-mode-hook fn))

  :config
  (push "\\*Personal Finance\\*" popper-reference-buffers)

  (define-keymap :keymap hledger-mode-map
    "C-c e" #'hledger-jentry
    ;; NOTE: Overrides global binding for completion-at-point/cape commands.
    "M-p" #'ceamx/hledger-prev-entry
    "M-n" #'ceamx/hledger-next-entry))

(use-package flycheck-hledger
  :after (flycheck hledger-mode)
  :demand t
  :init
  (add-hook 'hledger-mode-hook #'flycheck-mode)
  :config
  (setopt flycheck-hledger-strict t))

;; TODO
;; (use-package hledger-input
;;   ;; :pin manual
;;   ;; :load-path "packages/rest/hledger-mode/"
;;   ;; :bind (("C-c e" . hledger-capture)
;;   ;;        :map hledger-input-mode-map
;;   ;;        ("C-c C-b" . popup-balance-at-point))
;;   :preface
;;   (defun popup-balance-at-point ()
;;     "Show balance for account at point in a popup."
;;     (interactive)
;;     (if-let ((account (thing-at-point 'hledger-account)))
;;         (message (hledger-shell-command-to-string (format " balance -N %s "
;;                                                           account)))
;;       (message "No account at point")))

;;   :config
;;   (setq hledger-input-buffer-height 20)
;;   (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
;;   (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
;;   (add-hook 'hledger-input-mode-hook
;;             (lambda ()
;;               (make-local-variable 'company-idle-delay)
;;               (setq-local company-idle-delay 0.1))))

(provide 'init-tools-ledger)
;;; init-tools-ledger.el ends here
