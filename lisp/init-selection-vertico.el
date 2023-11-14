;;; init-selection-vertico.el --- Vertico configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;; "VERTical Interactive COmpletion"
;;
;; <https://github.com/minad/vertico>

;;; Code:

(use-package vertico
  :elpaca (vertico :host github
                   :repo "minad/vertico"
                   :files (:defaults "extensions/*"))
  :demand t

  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setopt vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setopt vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setopt vertico-resize t)

  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (setopt vertico-cycle t)

  :config
  ;; Prefix current candidate with arrow
  ;; <https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow>
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index start)
      (setq cand (funcall orig cand prefix suffix index start))
      (concat (if (= vertico--index index)
                  (propertize "» " 'face 'vertico-current)
                "  ")
              cand))))

(elpaca-wait)

;; Configure directory extension.
(use-package vertico-directory :elpaca nil
  :after vertico
  :commands (vertico-directory-tidy)

  :init
  ;; TODO: what for, exactly? needs binding?
  ;; via <https://github.com/minad/vertico/wiki#additions-for-moving-up-and-down-directories-in-find-file>
  ;; (defun cmx/vertico-directory-delete-entry ()
  ;;   "Delete directory or entire entry before point."
  ;;   (interactive)
  ;;   (when (and (> (point) (minibuffer-prompt-end))
  ;;              ;; Check vertico--base for stepwise file path completion
  ;;              (not (equal vertico--base ""))
  ;;              (eq 'file (vertico--metadata-get 'category)))
  ;;     (save-excursion
  ;;       (goto-char (1- (point)))
  ;;       (when (search-backward "/" (minibuffer-prompt-end) t)
  ;;         (delete-region (1+ (point)) (point-max))
  ;;         t))))

  :config
  (define-keymap :keymap vertico-map
    "RET"     #'vertico-directory-enter
    "DEL"     #'vertico-directory-delete-char
    "M-DEL"   #'vertico-directory-delete-word)

  ;; Tidy shadowed file names -- e.g. cleans `~/foo/bar///' to `/', and `~/foo/bar/~/' to `~/'.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


(use-package vertico-repeat :elpaca nil
  :after (savehist vertico)
  :commands (vertico-repeat-history vertico-repeat-save)
  :hook ((minibuffer-setup . vertico-repeat-save))
  :config
  (add-to-list 'savehist-additional-variables #'vertico-repeat-history))

(use-feature emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defvar crm-separator)
  (defun +crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple' (ARGS are candidates).
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
            (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
            (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'+crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setopt minibuffer-prompt-properties '( read-only t
                                          cursor-intangible t
                                          face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setopt read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable recursive minibuffers.
  (setopt enable-recursive-minibuffers t))

(provide 'init-selection-vertico)
;;; init-selection-vertico.el ends here
