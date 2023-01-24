;;; lib-files.el --- File helpers -*- lexical-binding: t -*-

;; Copyright (c) 2014-2022  Henrik Lissner
;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Henrik Lissner
;;         Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
;; Modified: 23 January, 2023
;; Created: 23 January, 2023
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

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;;; Commentary:

;;  Helper functions for working with files.

;;; Code:

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L369-L391>
(defun cmx-files--update-refs (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))
      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))
      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))
        (when (and
               (bound-and-true-p projectile-mode)
                ;; FIXME: de-doom
                ;; (doom-project-p)
                ;; (projectile-file-cached-p file (doom-project-root))
              )
          (projectile-purge-file-from-cache file)))
      )
    (dolist (default-directory toplevels)
      (magit-refresh))
    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))

;;
;;; Commands

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L397-L424>
(defun cmx/delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (cmx-files--update-refs path)
          (message "Deleted %S" short-path))))))

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L427-L441>
(defun cmx/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (cmx-files--update-refs old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))


;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L427-L441>
(defun cmx/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (cmx-files--update-refs old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))

;; (defun doom--sudo-file-path (file)
;;   (let ((host (or (file-remote-p file 'host) "localhost")))
;;     (concat "/" (when (file-remote-p file)
;;                   (concat (file-remote-p file 'method) ":"
;;                           (if-let (user (file-remote-p file 'user))
;;                               (concat user "@" host)
;;                             host)
;;                           "|"))
;;             "sudo:root@" host
;;             ":" (or (file-remote-p file 'localname)
;;                     file))))

;; ;;;###autoload
;; (defun doom/sudo-find-file (file)
;;   "Open FILE as root."
;;   (interactive "FOpen file as root: ")
;;   (find-file (doom--sudo-file-path file)))

;; ;;;###autoload
;; (defun doom/sudo-this-file ()
;;   "Open the current file as root."
;;   (interactive)
;;   (find-file
;;    (doom--sudo-file-path
;;     (or buffer-file-name
;;         (when (or (derived-mode-p 'dired-mode)
;;                   (derived-mode-p 'wdired-mode))
;;           default-directory)))))

;; ;;;###autoload
;; (defun doom/sudo-save-buffer ()
;;   "Save this file as root."
;;   (interactive)
;;   (let ((file (doom--sudo-file-path buffer-file-name)))
;;     (if-let (buffer (find-file-noselect file))
;;         (let ((origin (current-buffer)))
;;           (copy-to-buffer buffer (point-min) (point-max))
;;           (unwind-protect
;;               (with-current-buffer buffer
;;                 (save-buffer))
;;             (unless (eq origin buffer)
;;               (kill-buffer buffer))
;;             (with-current-buffer origin
;;               (revert-buffer t t))))
;;       (user-error "Unable to open %S" file))))

;; ;;;###autoload
;; (defun doom/remove-recent-file (file)
;;   "Remove FILE from your recently-opened-files list."
;;   (interactive
;;    (list (completing-read "Remove recent file: " recentf-list
;;                           nil t)))
;;   (setq recentf-list (delete file recentf-list))
;;   (recentf-save-list)
;;   (message "Removed %S from `recentf-list'" (abbreviate-file-name file)))

(provide 'lib-files)
;;; lib-files.el ends here