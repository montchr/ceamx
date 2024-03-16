;;; init-tools.el --- Tools and utilities            -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

;; Tools and, of course, utilities.

;;; Code:

;;; Requirements

(require 'seq)

(require 'lib-common)

;;; Install `pandoc-mode'

;; <https://joostkremers.github.io/pandoc-mode/>

(package! pandoc-mode
  (add-hook 'markdown-mode-hook #'pandoc-mode)

  (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings))

;;; Install the `unpackaged' library of useful yet unsubstantial Emacs Lisp code

;; <https://github.com/alphapapa/unpackaged.el>

(package! (unpackaged :host github :repo "alphapapa/unpackaged.el"))

;;; DISABLED Install `org-tanglesync' to sync tangled source code blocks

;; (package! org-tanglesync
;;   ;; FIXME: try to not do this
;;   (require 'org-tanglesync)

;;   (add-hook 'org-mode-hook #'org-tanglesync-mode)
;;   ;; (remove-hook 'org-mode-hook #'org-tanglesync-mode)

;;   (add-hook 'prog-mode-hook #'org-tanglesync-watch-mode)
;;   ;; (remove-hook 'prog-mode-hook #'org-tanglesync-watch-mode)

;;   ;; (add-hook 'text-mode-hook #'org-tanglesync-watch-mode)
;;   ;; (remove-hook 'text-mode-hook #'org-tanglesync-watch-mode)

;;   (setopt org-tanglesync-watch-files
;;     (seq-map (apply-partially #'file-name-concat user-emacs-directory)
;;       '("config-sync.org")))

;;   (global-keys!
;;     "C-c M-i" #'org-tanglesync-process-buffer-interactive
;;     "C-c M-a" #'org-tanglesync-process-buffer-automatic))

(provide 'init-tools)
;;; init-tools.el ends here
