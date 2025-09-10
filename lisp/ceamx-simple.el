;;; ceamx-simple.el --- ceamx :: lib :: simple       -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Chris Montgomery
;; Copyright (C) 2020-2024  Protesilaos Stavrou

;; Author: Chris Montgomery <chmont@protonmail.com>
;;         Protesilaos Stavrou <info@protesilaos.com>
;; Keywords:

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

;; Simple functions and commands for editing and the like.

;;; Code:

;;;###autoload
(defun ceamx/scroll-down ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

;;;###autoload
(defun ceamx/scroll-up ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

;; via prot-emacs
;;;###autoload
(defun ceamx/kill-current-buffer (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well.  Kill the window regardless of ARG if it
satisfies `ceamx-window-small-p' and it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (if (or (and (ceamx-window-small-p)
                 (null (window-prev-buffers)))
            (and arg (not (one-window-p))))
        (kill-buffer-and-window)
      (kill-buffer))))

(provide 'ceamx-simple)
;;; ceamx-simple.el ends here
