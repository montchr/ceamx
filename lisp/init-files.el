;;; init-files.el --- File handling -*- lexical-binding: t -*-

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

;;  Configuration for file handling

;;; Code:

(autoload 'cmx/delete-this-file "lib-files" nil t)
(autoload 'cmx/copy-this-file "lib-files" nil t)
(autoload 'cmx/move-this-file "lib-files" nil t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/doom-editor.el#L78-L89>
(defun cmx-create-missing-directories-h ()
  "Automatically create missing directories when creating new files."
  (unless (file-remote-p buffer-file-name)
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent-directory))
           (progn (make-directory parent-directory 'parents)
                  t)))))
(add-hook 'find-file-not-found-functions #'cmx-create-missing-directories-h)

;; Add file headers to new files.
(use-feature! autoinsert
  :config
  (auto-insert-mode t))

(setopt find-file-suppress-same-file-warnings t)

(setopt backup-by-copying t)
(setopt backup-directory-alist `((".*" . ,(expand-file-name
                                           (concat cmx-local-dir "backups")))))
(setopt delete-old-versions t)
(setopt kept-new-versions 5)
(setopt kept-old-versions 5)
(setopt require-final-newline t)
(setopt version-control t)
(setopt find-file-visit-truename t)

(use-feature! autorevert
  :defer 2
  :config
  ;; Automatically revert a buffer if its file has changed on disk.
  (setopt auto-revert-interval 0.01)
  (global-auto-revert-mode t))

(use-feature! xref
  :config
  ;; Always find references of symbol at point.
  (setopt xref-prompt-for-identifier nil))

;;
;;; Autosaves
;;

(setopt auto-save-interval 300)          ; input events before autosave
(setopt auto-save-visited-interval 30)   ; idle interval for all file-visiting buffers
(setopt auto-save-timeout 30)         ; idle interval before autosave

;; Don't create auto-save ~ files.
(setopt auto-save-default nil)

(auto-save-visited-mode)

(provide 'init-files)
;;; init-files.el ends here
