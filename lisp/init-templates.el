;;; init-templates.el --- Expandable file templates and abbrevs  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

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

(require 'ceamx-lib)

(package! tempel
  (setopt tempel-path (file-name-concat ceamx-templates-dir "tempel/*.eld"))

  ;; Require this prefix before triggering template name completion.
  (setopt tempel-trigger-prefix "<")

  ;; Setup completion at point for Tempel templates.
  (def-hook! +tempel-setup-capf-h ()
    '(conf-mode-hook prog-mode-hook text-mode-hook)
    "Add the Tempel Capf to `completion-at-point-functions'.

`tempel-expand' only triggers on exact matches.  Alternatively
use `tempel-complete' if you want to see all matches, but then
you should also configure `tempel-trigger-prefix', such that
Tempel does not trigger too often when you don't expect it.

NOTE: We add `tempel-expand' *before* the main programming mode
Capf, such that it will be tried first."
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))
(defun +tempel-include (elt)
  "Tempel user element ELT to include a nested template."
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

(after! tempel
  (add-to-list 'tempel-user-elements #'+tempel-include))
(define-keymap :keymap (current-global-map)
  "M-+" #'tempel-complete
  "M-*" #'tempel-insert)

(after! tempel
  (define-keymap :keymap tempel-map
    "TAB" #'tempel-next
    "S-TAB" #'tempel-previous))
(package! tempel-collection
  (after! tempel
    (require 'tempel-collection)))
(package! yasnippet
  (setopt yas-snippet-dirs
          (list (file-name-as-directory
                 (concat ceamx-templates-dir "yasnippet"))))

  (defer! 2
    (yas-global-mode 1)))

(after! yasnippet
  ;; These must be functions, so we must wait until load.
  (setopt yas-prompt-functions '(yas-completing-prompt
                                 yas-no-prompt)))
(after! (yasnippet hippie-exp)
  (declare-function yas-hippie-try-expand "yasnippet")

  (add-hook 'hippie-expand-try-functions-list #'yas-hippie-try-expand -80))
;; Note that `yas-maybe-expand' is indeed a variable, not a function.
(after! yasnippet
  (keymap-set yas-minor-mode-map "SPC" yas-maybe-expand)
  (keymap-set yas-minor-mode-map "RET" yas-maybe-expand))
(defun +yasnippet-snippet-mode-disable-final-newline-h ()
  "Prevent appendage of a final newline in `snippet-mode' files.
A final newline would be inserted literally into the snippet expansion."
  (setq-local require-final-newline nil))

(add-hook 'snippet-mode-hook #'+yasnippet-snippet-mode-disable-final-newline-h nil t)
(package! spdx)

(provide 'init-templates)
;;; init-templates.el ends here
