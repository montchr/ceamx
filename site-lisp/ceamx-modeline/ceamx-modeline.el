;;; ceamx-modeline.el --- Ceamx Modeline             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
;; Keywords: local, faces, frames, convenience

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

;; Custom mode-line.

;;;; Sources:

;; (info "(elisp) Mode Line Format")


;;; Code:

;;; Options

(defgroup ceamx-modeline nil
  "A minor-mode modeline from Ceamx."
  :group 'ceamx)

(defvar ceamx-modeline--original-format nil)

;;; Functions

(defun ceamx-modeline-format ()
  "Return the value of the `ceamx-modeline' format."
  `("%e"
    mode-line-front-space
    (:propertize (""
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote)
     display
     (min-width (5.0)))
    mode-line-frame-identification
    ;; mode-line-buffer-identification
    (:eval (breadcrumb-project-crumbs))
    "   "
    mode-line-position
    (vc-mode vc-mode)
    "  "
    mode-line-modes
    mode-line-misc-info
    mode-line-end-spaces))

;;; Mode

;;;###autoload
(define-minor-mode ceamx-modeline-mode
  "Ceamx custom modeline mode."
  :group 'ceamx-modeline
  :global t
  (if ceamx-modeline-mode
      (progn
        (setq ceamx-modeline--original-format (default-value 'mode-line-format))
        (setq-default mode-line-format (ceamx-modeline-format)))
    (progn
      (setq-default mode-line-format ceamx-modeline--original-format))))

;;; Footer:

(provide 'ceamx-modeline)
;;; ceamx-modeline.el ends here
