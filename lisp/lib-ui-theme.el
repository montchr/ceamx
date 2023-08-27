;;; lib-ui-theme.el ---Theme library functions and macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;; Theme library functions and macros.

;;; Code:

(defun cmx-reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes `(,cmx-ui-theme-light))
  (cmx-reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes `(,cmx-ui-theme-dark))
  (cmx-reapply-themes))


;;
;;; Hooks
;;

;; Hook to render C-style docblocks aka "multiline comments" as comments.
;; via <https://protesilaos.com/emacs/modus-themes#h:d0a3157b-9c04-46e8-8742-5fb2a7ae8798>
(defun cmx--multine-comment-face-hook ()
  (setq-local c-doc-face-name 'font-lock-comment-face))

;; via <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
(defvar after-enable-theme-hook nil)
(defun cmx--after-enable-theme-hook (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'after-enable-theme-hook))


(provide 'lib-ui-theme)
;;; lib-ui-theme.el ends here
