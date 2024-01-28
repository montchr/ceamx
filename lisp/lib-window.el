;;; lib-window.el --- Helpers for window management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

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

;; Helper functions for `init-window'.

;;; Code:

(declare-function 'popper-popup-status "popper")
(declare-function 'popper-toggle "popper")

;; TODO: add buffers tracking files in nix store, which are only useful for
;; reference purposes, often invoked when viewing definition of low-level
;; Emacs internals defined in C code (e.g. `string-equal')
(defun +popper-current-buffer-popup-p (buf)
  "Whether the buffer BUF should be considered a popup.
This is intended for use as a predicate in `popper-reference-buffers'."
    (with-current-buffer buf
      (and (derived-mode-p 'fundamental-mode)
           (not (bound-and-true-p scratch-buffer))
           ;; Less than `max-lines' but not empty.
           (let ((lines (count-lines (point-min) (point-max)))
                 (max-lines 10))
             (and (not (zerop lines))
               (< lines max-lines))))))

(defun +popper-close-focused (&rest _)
  "Close any focused `popper' popup.
Intended as a general hook function."
  (declare-function popper-toggle "popper")
  (when (bound-and-true-p popper-popup-status)
    (popper-toggle)))

;; via <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>
(defun +display-buffer-reuse-minor-mode-window (buffer alist)
  "Return a window sharing a minor mode with BUFFER.
ALIST is an association list of action symbols and values.  See
Info node \"(elisp) Buffer Display Action Alists\" for details of
such alists."
  (let* ((alist-entry (assq 'reusable-frames alist))
         (alist-mode-entry (assq 'minor-mode alist))
         (frames (cond (alist-entry (cdr alist-entry))
                       ((if (eq pop-up-frames 'graphic-only)
                            (display-graphic-p)
                          pop-up-frames)
                        0)
                       ;; TODO: remove or note the intention here -- not a call but
                       ;; a condition check to maintain support for the deprecated
                       ;; function. but really should be removed.
                       ;; (display-buffer-reuse-frames 0)
                       (t (last-nonminibuffer-frame))))
         (inhibit-same-window-p (cdr (assq 'inhibit-same-window alist)))
         (windows (window-list-1 nil 'nomini frames))
         (allowed-modes (if alist-mode-entry
                            (cdr alist-mode-entry)))
         (curwin (selected-window))
         (curframe (selected-frame)))
    (unless (listp allowed-modes)
      (setq allowed-modes (list allowed-modes)))
    (let ((same-mode-same-frame)
          (same-mode-other-frame))
      (dolist (window windows)
        (let ((mode?
               (with-current-buffer (window-buffer window)
                 (cl-some (lambda (m) (and (boundp m) (symbol-value m) 'same))
                          allowed-modes))))
          (when (and mode? (not (and inhibit-same-window-p (eq window curwin))))
            (push window (if (eq curframe (window-frame window))
                             same-mode-same-frame
                           same-mode-other-frame)))))
      (let ((window (car (nconc same-mode-same-frame
                                same-mode-other-frame))))
        (when (window-live-p window)
          (prog1 (window--display-buffer buffer window 'reuse alist)
            (unless (cdr (assq 'inhibit-switch-frame alist))
              (window--maybe-raise-frame (window-frame window)))))))))

;; via <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>
;;;###autoload
(defun ceamx/display-buffer-at-bottom ()
  "Move the current buffer to the bottom of the frame.
This is useful to take a buffer out of a side window.

The window parameters of this function are provided mostly for
didactic purposes."
  (interactive)
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (delete-window)
      (display-buffer-at-bottom
       buffer '((window-height .
                 (lambda (win)
                   (fit-window-to-buffer
                    win (/ (frame-height) 3)))))))))

(provide 'lib-window)
;;; lib-window.el ends here
