;;; init-files.el --- File handling -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

(use-feature emacs
  :init
  ;; Create missing directories when we open a file that doesn't exist under a
  ;; directory tree that may not exist.
  ;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/doom-editor.el#L78-L89>
  (defun cmx/create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t)))))
  (add-hook 'find-file-not-found-functions #'cmx/create-missing-directories-h))

(use-feature files
  :custom
  (backup-by-copying t)
  (backup-directory-alist `((".*" . ,(expand-file-name
                                      (concat +path-local-dir "backups"))))
                          "Keep backups in a dedicated directory.")
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (require-final-newline t)
  (version-control t))

(use-feature recentf)

(use-feature xref
  :config
  ;; Always find references of symbol at point.
  (setq xref-prompt-for-identifier nil))

;; TODO: find another way to set these after general?
;;       `+general-global-file' invocation cannot be at top-level
;;       maybe doom's `after!'?
(use-feature general
   :config
  (+general-global-file
    ;; "R" 'cmx/rename-current-file
    "C" 'cmx/copy-this-file
    "D" 'cmx/delete-this-file
    "R" 'cmx/move-this-file
    ))

(provide 'init-files)
;;; init-files.el ends here
