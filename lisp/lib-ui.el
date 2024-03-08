;;; lib-ui.el --- General user interface library     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;; <https://photo.stackexchange.com/questions/18494/what-is-the-difference-between-pixel-pitch-and-pixel-density>
;; <https://insights.samsung.com/2023/10/05/what-is-pixel-pitch-understanding-fine-pixel-pitch-led-displays/>

;;; Sources:

;; <https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/awaken.org#gui-related>

;;; Code:

;;;; Requirements

(require 'config-ui)

;;;; Constants

(defconst ceamx-inch-as-mm 25.4
  "One inch in millimeters.")

(defconst ceamx-pt-as-mm 0.353
  "One typographic point in millimeters.")

;;;; Functions

(defun ceamx-default-monitor-geometry ()
  "Return geometry for the first monitor in `display-monitor-attributes-list'."
  (let* ((first-monitor (car (display-monitor-attributes-list))))
    (alist-get 'geometry first-monitor)))

;; via <https://www.reddit.com/r/emacs/comments/7hzxb8/comment/dqywyqc/>
(defun ceamx-pixel-pitch (&optional frame)
  "Return the pixel pitch for FRAME in millimeters.
When FRAME is nil, the current frame will be used as default.

Pixel pitch describes the density of a display as a measure of
the distance from the center of a pixel to the center of its
adjacent pixel."
  (let ((monitor-attrs (frame-monitor-attributes frame)))
    (* 1000 (/ (float (nth 1 (assoc 'mm-size monitor-attrs)))
               (nth 3 (assoc 'geometry monitor-attrs))))))

(defun ceamx-font-height (number &optional multiplier)
  "Return a numeric font height based on NUMBER multiplied by MULTIPLIER.
NUMBER should be a whole number. MULTIPLIER should be a float.

If MULTIPLIER is nil, the value of `ceamx-font-height-multiplier'
will be used as default."
  (truncate (* number (or multiplier ceamx-font-height-multiplier))))

(provide 'lib-ui)
;;; lib-ui.el ends here
