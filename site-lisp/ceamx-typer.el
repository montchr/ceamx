;;; ceamx-typer.el --- Use Emacs to type anywhere in a Wayland environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery
;; Copyright (C) 2025  Thanos Apollo <public@thanosapollo.org>

;; Author: Thanos Apollo <public@thanosapollo.org>
;;         Chris Montgomery <chmont@protonmail.com>
;; Keywords: wp, text, tools, convenience, frames

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

;; Launch an ephemeral Emacs frame to forward text input to some other
;; destination in a Wayland session.

;; Based on the approach originally documented in the blog post titled
;; "Emacs Everywhere, even in Wayland" by Thanos Apollo:
;;
;; <https://thanosapollo.org/posts/use-emacs-everywhere/>

;; This adaptation replaces the use of the seemingly-abandoned =wtype=
;; tool in favor of =ydotool=.  Well, it will... when I get around to it...


;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defun ceamx-typer--wtype-process-text-command (text)
  "Prepare the shell command to process TEXT with \"wtype\"."
  (let* ( (has-final-newline (string-match-p "\n$" text))
          (lines (split-string text "\n"))
          (last-idx (1- (length lines))))
    (string-join
      (cl-loop for line in lines
               for i from 0
               collect (cond
                         ;; Last line without final newline
                         ((and (= i last-idx) (not has-final-newline))
                           (format "wtype -s 350 \"%s\""
                             (replace-regexp-in-string "\"" "\\\\\"" line)))
                         ;; Any other line
                         (t
                           (format "wtype -s 350 \"%s\" && wtype -k Return"
                             (replace-regexp-in-string "\"" "\\\\\""))
                           )
                         ))
      " && ")))

(defun ceamx-typer/finish-edit ()
  "Forward buffer text to the automation tool and close the Emacs frame."
  (interactive)
  (call-process-shell-command
    (ceamx-typer--wtype-process-text-command (buffer-string))
    nil 0)
  (delete-frame))

(defun ceamx-typer/abandon-edit ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defvar-keymap ceamx-typer-mode-map
  :doc "Keymap for `ceamx-typer-mode'."
  "C-c C-c" #'ceamx-typer/finish-edit
  "C-c C-k" #'ceamx-typer/abandon-edit)

(define-minor-mode ceamx-typer-mode
  "Minor mode for inserting text outside of Emacs via simulated keyboard input.
Supports either the \"wtype\" tool as backend.  The operating
environment must provide the necessary executable."
  :keymap ceamx-typer-mode-map)

(defun ceamx-typer/edit ()
  "Launch an ephemeral frame with a clean buffer for typing."
  (interactive)
  (let ( (frame (make-frame '( (name . "emacs-float")
                              (fullscreen . 0)
                              (undecorated . t)
                              (width . 70)
                              (height . 20))))
         (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (ceamx-typer-mode)
      (setq-local header-line-format
        (format " %s to insert text or %s to cancel."
          (propertize "C-c C-c" 'face 'help-key-binding)
          (propertize "C-c C-k" 'face 'help-key-binding)))
      ;; Ephemeralize this frame.
      (set-frame-parameter frame 'delete-before-kill-buffer t)
      (set-window-dedicated-p (selected-window) t))))

(provide 'ceamx-typer)
;;; ceamx-typer.el ends here
