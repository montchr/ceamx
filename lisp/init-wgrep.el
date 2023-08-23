;;; init-wgrep.el --- Configuration for writable minibuffer grep  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;; "Writable grep buffer and apply the changes to files"
;;
;; <https://github.com/mhayashi1120/Emacs-wgrep>

;;; Code:

(require 'lib-common)

(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode)

  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)

  :config
  (keymap-set grep-mode-map "W" #'wgrep-change-to-wgrep-mode)
  (keymap-set dired-mode-map "C-c C-e" #'wgrep-change-to-wgrep-mode)
  ;; (advice-add #'wgrep-abort-changes :after #'+popup-close-a)
  ;; (advice-add #'wgrep-finish-edit :after #'+popup-close-a)
  )

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/completion/vertico/autoload/vertico.el#L91-L108>
(defun +vertico/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired,
and consult-location to occur-edit."
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "Embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

(keymap-set minibuffer-local-map "C-c C-e" #'+vertico/embark-export-write)

(provide 'init-wgrep)
;;; init-wgrep.el ends here
