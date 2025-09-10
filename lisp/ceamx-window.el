;;; ceamx-window.el --- Window management support library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2022-2025  Chris Montgomery <chmont@protonmail.com>
;; Copyright (C) 2020-2024  Protesilaos Stavrou

;; Author: Chris Montgomery <chmont@protonmail.com>
;;         Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;;         Protesilaos Stavrou <info@protesilaos.com>

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
;;; Code:

;;
;;;; Requirements

(require 'windmove)

;;
;;;; Customization

(defcustom ceamx-window-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.
The buffer will be created if it does not exist."
  :group 'ceamx
  :type '(string))

;;
;;;; Introspection functions

;; via prot-emacs
;;;###autoload
(defun ceamx-window-bounds ()
  "Return start and end points in the window as a cons cell."
  (cons (window-start) (window-end)))

;; via prot-emacs
;;;###autoload
(defun ceamx-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold', respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

;; via prot-emacs
;;;###autoload
(defun ceamx-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;;;###autoload
(defun ceamx-window-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))


;;
;;;; `display-buffer' functions

;; <https://github.com/karthink/popper/blob/570b0820f884a9c0e3d9cb07e7f7f523b39b836f/popper.el#L265-L283>

(defun ceamx-window-display-popup-at-bottom (buffer &optional alist)
  "Display popup-buffer BUFFER at the bottom of the screen.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists."
  (display-buffer-in-side-window
   buffer
   (append alist
           `((side . bottom)
             (slot . 1)))))

(defun ceamx-window-display-popup (buffer &optional alist)
  "Display and switch to popup-buffer BUFFER at the bottom of the screen.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists."
  (let ((window (ceamx-window-display-popup-at-bottom buffer alist)))
    (select-window window)))

;; via <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>
(defun ceamx-window-display-reuse-minor-mode-window (buffer alist)
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

;;
;;;; Popups

;; <https://github.com/karthink/popper/blob/master/popper.el#L265-L283>

(defun ceamx-window-current-buffer-popup-p (buf)
  "Whether the buffer BUF should be considered a popup.
This is intended for use as a predicate in `popper-reference-buffers'."
  (with-current-buffer buf
    (let ((window (get-buffer-window buf)))
      (and (derived-mode-p 'fundamental-mode)
           ;; Ignore the scratch buffer
           (not (bound-and-true-p scratch-buffer))
           ;; Ignore `dired-preview' window.
           (when (featurep 'dired-preview)
             (not (dired-preview--window-parameter-p window)))
           ;; Less than `max-lines' but not empty.
           (let ((lines (count-lines (point-min) (point-max)))
                 (max-lines 10))
             (and (not (zerop lines))
                  (< lines max-lines)))))))

(defun ceamx-window-close-focused-popup (&rest _)
  "Close any focused `popper' popup.
Intended as a general hook function."
  (declare-function popper-toggle "popper")
  (when (bound-and-true-p popper-popup-status)
    (popper-toggle)))

(defun ceamx-window+popper--select-below-fn (buffer &optional _alist)
  (funcall (if (> (frame-width) 170)
               ;; #'display-buffer-in-direction
               #'popper-select-popup-at-bottom
             #'display-buffer-at-bottom)
           buffer
           `((window-height . ,popper-window-height)
             (direction . below)
             (body-function . ,#'select-window))))

;;
;;;; Window Management

;; via <https://github.com/doomemacs/doomemacs/blob/ff33ec8f7a89d168ca533612e2562883c89e029f/modules/editor/evil/autoload/evil.el#L42-L73>
(defun ceamx-window--swap-or-split (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows in DIRECTION and there is only one window
in the current frame, split the window in DIRECTION and place
this window there."
  (let* ((this-window (selected-window))
         (that-window (window-in-direction direction this-window)))
    (unless that-window
      (setq that-window (split-window this-window nil direction))
      (with-selected-window that-window
        (switch-to-buffer ceamx-window-fallback-buffer-name)))
    (window-swap-states this-window that-window)
    (select-window that-window)))

;;;###autoload
(defun ceamx-window/move-left ()
  "Swap or move selected window to the left."
  (interactive)
  (ceamx-window--swap-or-split 'left))

;;;###autoload
(defun ceamx-window/move-right ()
  "Swap or move selected window to the right."
  (interactive)
  (ceamx-window--swap-or-split 'right))

;;;###autoload
(defun ceamx-window/move-up ()
  "Swap or move selected window upwards."
  (interactive)
  (ceamx-window--swap-or-split 'up))

;;;###autoload
(defun ceamx-window/move-down ()
  "Swap or move selected window downwards."
  (interactive)
  (ceamx-window--swap-or-split 'down))

;;
;;;; Buffer management

;;;###autoload
(defun ceamx-window/switch-to-buffer ()
  "Switch to buffer offered from various sources.
If an `activities-mode' activity is current, use
`activities-switch-buffer'.  Otherwise, use `bufler-switch-buffer'."
  (interactive)
  (cond ((or (equal '(16) current-prefix-arg)
             (not (activities-current)))
         (call-interactively #'bufler-switch-buffer))
        (t (call-interactively #'activities-switch-buffer))))

(provide 'ceamx-window)
;;; ceamx-window.el ends here
