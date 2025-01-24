;;; ceamx-completion.el --- Ceamx: Completions Library  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2025  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx

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

;;;; Requirements

(require 'ceamx-lib)

(autoload #'consult-info "consult-info")
(autoload #'consult--file-preview "consult")
(autoload #'consult--read "consult")
(autoload #'embark-export "embark")
(autoload #'wgrep-change-to-wgrep-mode "wgrep")


;;;; Variables

;;;; Customization

;;;; Functions

(defun ceamx-completion-corfu-minibuffer-enable-p ()
  "Whether to enable `corfu' completion in a currently-active minibuffer."
  (not (or (bound-and-true-p mct--active)
           (bound-and-true-p vertico--input)
           (eq (current-local-map) read-passwd-map))))

;;;###autoload
(defun ceamx-completion--tempel-include (elt)
  "Tempel user element ELT to include a nested template."
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

;;;###autoload
(defun ceamx-completion-orderless-consult-dispatch (word _index _total)
  "Orderless dispatcher for Consult with support for custom syntax.
Recognizes the following patterns:

    * .ext (file extension)
    * regexp$ (regexp matching at end)

Sourced from the Consult Wiki."
  (cond
   ;; Ensure that $ works with Consult commands, which add
   ;; disambiguation suffixes.
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1)
                                  (ceamx-completion-orderless--consult-suffix))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\."
                                  (substring word 1)
                                  (ceamx-completion-orderless--consult-suffix))))))

 (defun ceamx-completion-orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))


;;;; Commands

;; via <https://github.com/minad/consult?tab=readme-ov-file#help>
;;;###autoload
(defun ceamx/emacs-info ()
  "Search through common Emacs info pages."
  (interactive)
  (consult-info "emacs" "efaq" "elisp" "cl"))

;;;###autoload
(defun ceamx/org-info ()
  "Search through the Org-Mode info page."
  (interactive)
  (consult-info "org"))

;;;###autoload
(defun ceamx/completion-info ()
  "Search through completion info pages."
  (interactive)
  (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                "corfu" "cape" "tempel"))

;;;###autoload
(defun ceamx/consult-info-dwim (&optional buffer)
  "Search Info manuals appropriate to BUFFER's major-mode."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((mode major-mode)
           (fn (pcase mode
                 ((pred (lambda (x) (memq x '(emacs-lisp-mode))))
                  #'ceamx/emacs-info)
                 ((pred (lambda (x) (memq x '(org-mode org-agenda-mode))))
                  #'ceamx/org-info)
                 (_ #'consult-info))))
      (command-execute fn))))

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/completion/vertico/autoload/vertico.el#L91-L108>
;;;###autoload
(defun ceamx-completion/embark-export-write ()
  "Export the current `vertico' candidates to a writable buffer.
Supported export flows include the following:

`consult-grep'      => `wgrep'
files               => `wdired'
`consult-location'  => `occur-edit'"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "Embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;; via <https://github.com/minad/consult/wiki#previewing-files-in-find-file>
;;;###autoload
(defun ceamx-completion/consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

(provide 'ceamx-completion)
;;; ceamx-completion.el ends here
