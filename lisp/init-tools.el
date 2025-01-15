;;; init-tools.el --- Tools and utilities  -*- lexical-binding: t;  -*-

;; Copyright (c) 2023-2025  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'seq)

(require 'ceamx-lib)
(package! pandoc-mode
  (add-hook 'markdown-mode-hook #'pandoc-mode)

  (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings))
(package! htmlize
  ;; FIXME: conflicts with `beframe'
  ;; (keymap-global-set "C-c b h" #'htmlize-buffer)
  )
(package! (unpackaged :host github :repo "alphapapa/unpackaged.el"))
(package! mugur)
(package! free-keys)
(package! uuidgen)
(require 'ceamx-paths)

(package! hledger-mode
  (setopt hledger-jfile ceamx-ledger-main-journal-file))
(after! popper
  (add-to-list 'popper-reference-buffers "\\*Personal Finance\\*"))
(require 'lib-tools)

(after! hledger-mode
  (define-keymap :keymap hledger-mode-map
    "C-c e" #'hledger-jentry
    ;; NOTE: Overrides global binding for completion-at-point/cape commands.
    "M-p" #'ceamx/hledger-prev-entry
    "M-n" #'ceamx/hledger-next-entry))
(package! flycheck-hledger
  (when (fboundp 'flycheck-mode)
    (add-hook 'hledger-mode-hook #'flycheck-mode))

  (setopt flycheck-hledger-strict t))
(require 'lib-tools)

(after! hledger-mode
  (add-hook 'hledger-mode-hook #'+hledger-accounts-capf-h))
(require 'ceamx-lib)

(defvar pdf-tools-handle-upgrades nil)

(after! pdf-tools
  (dolist
      (pkg
       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
         pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
         pdf-util pdf-view pdf-virtual))
    (require pkg))
  (pdf-tools-install))
(use-feature! i-ching
  :commands (i-ching/lookup
             i-ching/cast))

(provide 'init-tools)
;;; init-tools.el ends here
