;;; init-templates.el --- Snippets and file templates  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Configuration for file templates and snippet expansion.

;; NOTE: `tempel' does not seem to pick up changes to files even after
;;       re-evalling the `use-package' declaration. However, it does
;;       pick up newly-added templates.

;;; Code:

(use-package tempel
  ;; :custom
  ;;
  ;; Require trigger prefix before template name when completing.
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  (setopt tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))

  ;; Setup completion at point
  (defun cmx:tempel--setup-capf-h ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (dolist (hook '(conf-mode-hook prog-mode-hook text-mode-hook))
    (add-hook hook #'cmx:tempel--setup-capf-h)
    ;; See also `global-tempel-abbrev-mode'.
    (add-hook hook #'tempel-abbrev-mode)))

(use-package tempel-collection
  :after tempel)

(provide 'init-templates)
;;; init-templates.el ends here
