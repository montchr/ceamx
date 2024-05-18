;;; lib-files.el --- Files support support  -*- lexical-binding: t;  -*-

;; Copyright (C) 2022-2024  Chris Montgomery <chmont@proton.me>
;; Copyright (C) 2014-2022  Henrik Lissner
;; Copyright (C) 2006-2021  Steve Purcell
;; Copyright (C) 2008-2024  Jonas Bernoulli
;; SPDX-License-Identifier: GPL-3.0-or-later AND MIT AND BSD-2-Clause

;; Author: Henrik Lissner
;;         Vegard Øye <vegard_oye at hotmail.com>
;;         Steve Purcell
;;         Chris Montgomery <chmont@proton.me>
;;         Jonas Bernoulli <jonas@bernoul.li>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Created: 23 January 2023
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

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;; Code:

(require 'cl-lib)
;; FIXME: is this supposed to work on save? not working in either magit or projectile
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L369-L391>
(defun ceamx-files--update-refs (&rest files)
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
;; via <https://github.com/emacs-evil/evil/blob/9eb69b7f5b3c72cfc66f69b3242e935015780654/evil-commands.el#L3325-L3332>
(defun ceamx/file-edit (file &optional bang)
  "Open FILE.
If no FILE is specified, reload the current buffer from disk."
  :repeat nil
  (interactive "<f><!>")
  (if file
      (find-file file)
    (revert-buffer bang (or bang (not (buffer-modified-p))) t)))
;; via <https://github.com/emacs-evil/evil/blob/9eb69b7f5b3c72cfc66f69b3242e935015780654/evil-commands.el#L4652-L4660>
(defun ceamx/buffer-new (&optional file)
  "Edit a new unnamed buffer or FILE."
  :repeat nil
  (interactive "<f>")
  (if file
      (ceamx/file-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer))))
;; FIXME: this does not actually kill its buffers -- buffer must be deleted manually
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L397-L424>
(defun ceamx/delete-this-file (&optional path force-p)
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
          ;; Ensures that windows displaying this buffer will be switched to
          ;; real buffers (`doom-real-buffer-p')
          ;; FIXME: implement -- invent the universe -- but the stuff within is very useful to us (e.g. doom-real-buffer-p and filtering buffers)...
          ;; (doom/kill-this-buffer-in-all-windows buf t)
          ;; TODO: remove when the above is implemented -- `kill-this-buffer' only removes the one buffer
          (kill-this-buffer)
          (ceamx-files--update-refs path)
          (message "Deleted %S" short-path))))))
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L427-L441>
(defun ceamx/copy-this-file (new-path &optional force-p)
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
    (ceamx-files--update-refs old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L427-L441>
(defun ceamx/move-this-file (new-path &optional force-p)
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
    (ceamx-files--update-refs old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))
;; via <https://github.com/noctuid/dotfiles/blob/434ddb77c4b40f4b7ab2246cc2254aa4f408b16f/emacs/.emacs.d/awaken.org>
(defun ceamx/kill-this-buffer ()
  "`kill-this-buffer' with no menu-bar checks.
`kill-this-buffer' is supposed to be called from the menu bar.
See <https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/>."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer))))
(defun ceamx/diff-with-file (&optional arg)
  (interactive "P")
  (let ((buffer (when arg (current-buffer))))
    (diff-buffer-with-file buffer)))
;; via <https://github.com/tarsius/fwb-cmds/blob/88e823809067983acfaeafa57d0bb6e889429ad2/fwb-cmds.el#L140C1-L156C78>
;;;###autoload
(defun ceamx/sudo-find-file (&optional arg)
  "Edit the visited file as \"root\".
If the current buffer does not visit a file, the visited file is
writable or with a prefix argument, then read a file to visit."
  (interactive "P")
  (require 'tramp)
  (if (or arg
          (not buffer-file-name)
          (file-writable-p buffer-file-name))
      (let ((default-directory
             (concat "/sudo:root@localhost:" default-directory)))
        (apply #'find-file
               (find-file-read-args
                "Find file: "
                (confirm-nonexistent-file-or-buffer))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'lib-files)
;;; lib-files.el ends here
