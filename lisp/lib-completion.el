;;; lib-completion.el --- Completion helper functions  -*- lexical-binding: t; -*-

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

;;; Code:

(defvar corfu--frame)

;; via <https://github.com/doomemacs/doomemacs/pull/7002>
(defun +corfu-visible-p ()
  "Predicate whether Corfu is active/visible."
  (or (and (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (featurep 'corfu-terminal)
           (popon-live-p corfu-terminal--popon))))

(provide 'lib-completion)
;;; lib-completion.el ends here
