;;; init-files.el --- File handling  -*- lexical-binding: t;  -*-

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

(require 'ceamx-keymaps)
(require 'ceamx-paths)

(require 'ceamx-lib)
(require 'lib-files)
(setopt create-lockfiles nil)
(setopt make-backup-files nil)

(when make-backup-files
  (setopt version-control t)
  (setopt delete-old-versions t)
  (setopt kept-new-versions 5)
  (setopt kept-old-versions 5))

(setopt delete-by-moving-to-trash t)


(use-feature! autoinsert
  :config
  (auto-insert-mode t))
(setopt find-file-suppress-same-file-warnings t)
(setopt find-file-visit-truename t)
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

(provide 'init-files)
;;; init-files.el ends here
