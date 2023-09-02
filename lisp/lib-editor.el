;;; lib-editor.el --- Editor library functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;;

;;; Code:

;; mode-specific local-electric pairs
;; <https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:BE3F251D-5F39-4337-B27C-CFB81EE9A504>
(defconst +default-electric-pairs electric-pair-pairs)
(defun cmx-electric-pair-add-local-pairs (pairs)
  "Add new local electric PAIRS to the current buffer.

Example usage:

    (add-hook \\'jupyter-org-interaction-mode
      (lambda ()
        (cmx-electric-pair-add-local-pairs \\'())))"
  (setq-local electric-pair-pairs (append +default-electric-pairs pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))


(provide 'lib-editor)
;;; lib-editor.el ends here
