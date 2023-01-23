;;; autoloads/magit.el --- Autoloads for magit -*- lexical-binding: t -*-

;; Copyright (c) 2014-2022  Henrik Lissner
;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Henrik Lissner
;; URL: https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/tools/magit/autoload.el
;; Modified: 22 January, 2023
;; Created: 22 January, 2023
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

;;  Autoloaded functions for magit, lifted from Doom Emacs.
;;
;;  - <2023-01-22>: <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/tools/magit/autoload.el>

;;; Code:

;;;###autoload
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))

             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       +magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let (window (window-in-direction direction))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
        (unless magit-display-buffer-noselect
          (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))


;;
;;; Auto-revert

(defvar +magit--stale-p nil)

(defun +magit--revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable '+magit--stale-p)
    (when (and buffer-file-name (file-exists-p buffer-file-name))
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t t)))))

;;;###autoload
(defun +magit-mark-stale-buffers-h ()
  "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer)
          (+magit--revert-buffer buffer)
        (with-current-buffer buffer
          (setq-local +magit--stale-p t))))))

;;;###autoload
(defun +magit-revert-buffer-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--stale-p
    (+magit--revert-buffer (current-buffer))))


;;
;;; Commands

;;;###autoload
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (+magit/quit-all))))

;;;###autoload
(defun +magit/quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
  (+magit-mark-stale-buffers-h))

(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

;;;###autoload
(defun +magit/start-code-review (arg)
  (interactive "P")
  (call-interactively
    (if (or arg (not (featurep 'forge)))
        #'code-review-start
      #'code-review-forge-pr-at-point)))
