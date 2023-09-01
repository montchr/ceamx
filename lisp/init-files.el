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

(setq find-file-suppress-same-file-warnings t)

(setq! backup-by-copying t)
(setq! backup-directory-alist `((".*" . ,(expand-file-name
                                          (concat +path-local-dir "backups")))))
(setq! delete-old-versions t)
(setq! kept-new-versions 5)
(setq! kept-old-versions 5)
(setq! require-final-newline t)
(setq! version-control t)

(use-feature autorevert
  :defer 2
  :custom
  ;; Automatically revert a buffer if its file has changed on disk.
  (auto-revert-interval 0.01 "Instantly revert")
  :config
  (global-auto-revert-mode t))

(use-feature recentf)

(use-feature xref
  :config
  ;; Always find references of symbol at point.
  (setq xref-prompt-for-identifier nil))


;;
;;; Autosaves
;;

(setq auto-save-interval 666)           ; input events before autosave
(setq auto-save-timeout 33)             ; idle seconds before autosave
;; Don't create auto-save ~ files.
(setq auto-save-default nil)

(auto-save-visited-mode)


(provide 'init-files)
;;; init-files.el ends here
