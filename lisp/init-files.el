;;; init-files.el --- File handling -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;;  Configuration for file handling

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)
(require 'ceamx-paths)

(require 'lib-common)
(require 'lib-files)
(require 'lib-keys)

;;; Manage backup files and prevent file-lock clutter

;; TODO: enable file locks in TRAMP?

(setopt create-lockfiles nil)
(setopt make-backup-files nil)

(when make-backup-files
  (setopt version-control t)
  (setopt delete-old-versions t)
  (setopt kept-new-versions 5)
  (setopt kept-old-versions 5))

(setopt delete-by-moving-to-trash t)

;;; Add file headers to new files

(use-feature! autoinsert
  :config
  (auto-insert-mode t))

;;; Configure finding of files

(setopt find-file-suppress-same-file-warnings t)

(setopt find-file-visit-truename t)

;; TODO: move elsewhere
(use-feature! xref
  :config
  ;; Always find references of symbol at point.
  (setopt xref-prompt-for-identifier nil))

;;;; Prompt to create missing parent directories for not-found files

;; <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/doom-editor.el#L78-L89>

(defun ceamx-create-missing-directories-h ()
  "Automatically create missing directories when creating new files."
  (unless (file-remote-p buffer-file-name)
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-directory))
        (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                    parent-directory))
        (progn (make-directory parent-directory 'parents)
          t)))))

(add-hook 'find-file-not-found-functions #'ceamx-create-missing-directories-h)

;;; Configure auto-saving of file-visiting buffers

;; Prevent creation of the list of all auto-saved files.
(setopt auto-save-list-file-prefix nil)

;; Number of input events before autosave
(setopt auto-save-interval 300)

;; Idle interval for all file-visiting buffers
(setopt auto-save-visited-interval 30)

;; Idle interval before autosave
(setopt auto-save-timeout 30)

;; Don't create auto-save "~" files.
(setopt auto-save-default nil)

;; Save file-visiting buffers according to the configured timers.
(auto-save-visited-mode)

;;; Set the list of safe local variable values

(setopt safe-local-variable-values
        '((eval load-file "./ceamx-dev-loader.el")))

;;; Keybindings

(global-keys!
  "C-c f" '("[ File ]" . ceamx-file-map)
  "C-c C-f" '("[ File ]" . ceamx-file-map)

  ;; I mistakenly hit this sequence frequently instead of C-x C-f, but have never
  ;; once needed to configure `fill-column' on-demand (that should be configured
  ;; explicitly, or simply call `set-fill-column' with M-x).
  "C-x f" #'find-file)

(keys! ceamx-file-map
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name

  "c" '("copy..." . ceamx/copy-this-file)
  "d" '("delete" . ceamx/delete-this-file)
  "f" '("find (g)..." . find-file)
  "r" '("rename/move..." . ceamx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file)

  "C-d" '("diff with..." . ceamx/diff-with-file))

(provide 'init-files)
;;; init-files.el ends here
