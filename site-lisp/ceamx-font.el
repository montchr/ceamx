;;; ceamx-font.el --- Helpers for working with typography in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: faces, text

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

;;

;;; Code:

;; via <https://github.com/protesilaos/fontaine/issues/20#issue-3281274571>
(defun ceamx-font-screen-geometry ()
  "Return the geometry of the primary monitor as a plist: (:x X :y Y :width W :height H)."
  (let* ((monitors (display-monitor-attributes-list))
         (primary (car monitors))  ;; Fallback: first monitor
         (geom (assq 'geometry primary)))
    (when geom
      (let ((g (cdr geom)))
        (list :x (nth 0 g)
              :y (nth 1 g)
              :width (nth 2 g)
              :height (nth 3 g))))))

;; via <https://github.com/protesilaos/fontaine/issues/20#issue-3281274571>
(defun ceamx-font-height-to-fit-lines (lines &optional font-name)
  "Return the font height needed to fit LINES vertically in the current screen.
Optional FONT-NAME can be used to compute based on a specific font family."
  (interactive "nNumber of lines to fit: ")
  (let* ((geom (ceamx-font-screen-geometry))
         (screen-height (plist-get geom :height))
         (window (selected-window))
         (line-height
          (condition-case nil
              (let* ((start-pos (posn-at-point (point-min) window))
                     (end-pos (posn-at-point (save-excursion (goto-char (point-min)) (forward-line 1) (point)) window))
                     (start-px (and start-pos (cdr (posn-x-y start-pos))))
                     (end-px   (and end-pos (cdr (posn-x-y end-pos)))))
                (if (and start-px end-px)
                    (- (cdr end-px) (cdr start-px))
                  (frame-char-height)))
            (error (frame-char-height))))
         (current-font-height (face-attribute 'default :height))
         (font-height (floor (* current-font-height (/ (float screen-height) (* lines line-height))))))
    (when (called-interactively-p 'interactive)
      (message "Calculated font height: %d (for %d lines, %d px line height)" font-height lines line-height))
    font-height))

(provide 'ceamx-font)
;;; ceamx-font.el ends here
