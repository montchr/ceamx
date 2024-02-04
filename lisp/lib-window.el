;;; lib-window.el --- Helpers for window management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;;         Vegard Øye <vegard_oye at hotmail.com>

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

;;; Sources:

;; <https://github.com/emacs-evil/evil/blob/5995f6f21f662484440ed67a28ce59e365feb9ad/evil-commands.el>
;; <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

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

;;; Interactive

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

(defcustom ceamx-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.
The buffer will be created if it does not exist."
  :group 'ceamx
  :type '(string))

(defmacro with-safe-side-windows! (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows.
Copied from the `evil' macro `evil-save-side-windows'."
  (declare (indent defun)
    (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (and (fboundp 'window-toggle-side-windows)
                     (window-with-parameter 'window-side))))
       ;; (declare-function window-toggle-side-windows "window")
       (when ,sides
         (window-toggle-side-windows))
       (unwind-protect
         (progn ,@body)
         (when ,sides
           (window-toggle-side-windows))))))

;; TODO: this seems very similar to `windmove-swap-states-in-direction'...?
(defun ceamx-move-window (side)
  "Move the `selected-window' to SIDE.
The state of the `selected-window' is saved along with the state
of the window tree consisting of all the other windows. Then, all
windows are deleted, the remaining window is split according to
DIRECTION, the state of the window in DIRECTION is replace with
the saved state of the `selected-window', and, finally, the state
of the saved window tree is reconstructed on the opposite side.

SIDE has the same meaning as in `split-window'.

Copied from the `evil' function `evil-move-window'."
  (with-safe-side-windows!
    (unless (one-window-p)
      (save-excursion
        (let ((this-window-state (window-state-get (selected-window))))
          (delete-window)
          (let ((window-tree (window-state-get)))
            (delete-other-windows)
            (let ((sub-window (selected-window))
                  (new-window (ceamx--split-window-in-direction side)))
              (window-state-put window-tree sub-window)
              (window-state-put this-window-state new-window)
              (select-window new-window)))))
      (balance-windows))))

(defun ceamx--split-window-in-direction (direction &optional window)
  "Split WINDOW in DIRECTION.
DIRECTION may be one of the DIRECTION arguments accepted by
`split-window'. Additionally, when DIRECTION is the symbol `up'
or `down', the direction argument passed to `split-window' will
be translated to `above' or `below', respectively.

When WINDOW is nil, the currently-selected will be used. See
`split-window' for details."
  (split-window window nil (pcase direction
                             ('up 'above)
                             ('down 'below)
                             (_ direction))))

(require 'windmove)

;; via <https://github.com/doomemacs/doomemacs/blob/ff33ec8f7a89d168ca533612e2562883c89e029f/modules/editor/evil/autoload/evil.el#L42-L73>
(defun ceamx--window-swap-or-split (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows in DIRECTION and there is only one window
in the current frame, split the window in DIRECTION and place
this window there."
  (when (window-dedicated-p)
    (user-error "Cannot swap/split a dedicated window"))
  (let* ((this-window (selected-window))
         ;; FIXME: unused, even in the original.
         (this-buffer (current-buffer))
         (that-window (window-in-direction direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              ;; FIXME: wouldn't this case never be true, since calling
              ;; `window-dedicated-p' with no argument is the same as calling it
              ;; with `this-window' (`selected-window') as argument?
              (window-dedicated-p this-window))
      (setq that-buffer nil)
      (setq that-window nil))
    (if (not (or that-window (one-window-p t)))
      ;; TODO: note original usage of evil-window-move-* functions which are
      ;; literally just calls to evil-move-window with a direction specified. we
      ;; don't need that level of indirection.
      (ceamx-move-window direction)
      ;; TODO: not working, but it'd be nice to use internal libraries if possible/appropriate
      ;; (windmove-swap-states-in-direction direction)
      (unless that-window
        (setq that-window (ceamx--split-window-in-direction direction this-window))
        (with-selected-window that-window
          (switch-to-buffer ceamx-fallback-buffer-name))
        (setq that-buffer (window-buffer that-window)))
      ;; FIXME: something is happening towards the end here where the new window
      ;; is not selected
      (window-swap-states this-window that-window)
      (select-window that-window))))

(defun ceamx/window-move-left ()
  "Swap or move selected window to the left."
  (interactive)
  (ceamx--window-swap-or-split 'left))

(defun ceamx/window-move-right ()
  "Swap or move selected window to the right."
  (interactive)
  (ceamx--window-swap-or-split 'right))

(defun ceamx/window-move-up ()
  "Swap or move selected window upwards."
  (interactive)
  (ceamx--window-swap-or-split 'up))

(defun ceamx/window-move-down ()
  "Swap or move selected window downwards."
  (interactive)
  (ceamx--window-swap-or-split 'down))


(defun ceamx/split-window (&optional count direction file)
  "TODO"
  (interactive "P\nS\nf")
  (select-window
    (split-window (selected-window)
      (when count (- count))
      direction))
  ;; (when (and (not count)
  ;;         ceamx-window-auto-balance)
  ;;   (balance-windows (window-parent)))
  (when file
    (find-file file)))

(defun ceamx/split-window-with-buffer (buffer)
  "Split window and switch to BUFFER.
If BUFFER is not the name of an existing buffer, then a new
buffer will be created with that name."
  (interactive "B")
  (ceamx/split-window)
  (switch-to-buffer buffer))

(defun ceamx/split-window-with-next-buffer ()
  "Split window and switch to the next buffer in the buffer list."
  (interactive)
  (ceamx/split-window-with-buffer (next-buffer)))

(defun ceamx/split-window-with-prev-buffer ()
  "Split window and switch to the previous buffer in the buffer list."
  (interactive)
  (ceamx/split-window-with-buffer (previous-buffer)))

(defun ceamx/buffer-create (&optional file)
  "Edit a new unnamed buffer or open FILE.
When called interactively, prompt the user for FILE."
  (interactive "F")
  (if file
    (find-file file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer))))

(defun ceamx/window-increase-height (count)
  "Increase window height by COUNT."
  (interactive "p")
  (enlarge-window count))

(defun ceamx/window-decrease-height (count)
  "Decrease window height by COUNT."
  (interactive "p")
  (enlarge-window (- count)))

(defun ceamx/window-increase-width (count)
  "Increase window width by COUNT."
  (interactive "p")
  (enlarge-window count t))

(defun ceamx/window-decrease-width (count)
  "Decrease window width by COUNT."
  (interactive "p")
  (enlarge-window (- count) t))

(provide 'lib-window)
;;; lib-window.el ends here
