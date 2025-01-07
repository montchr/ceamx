;;; ceamx-init-dired.el --- Dired customizations -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Configuration for Dired and extensions.

;; FIXME: Hide directories like ".git" and ".direnv" by default...

;;; Code:

(require 'ceamx-lib)

;; General Dired customizations


(after! dired
  ;; cf. `dired-omit-files', `dired-omit-lines', `dired-omit-extensions'
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (setopt dired-auto-revert-buffer t)
  (setopt dired-dwim-target t)
  (setopt dired-kill-when-opening-new-dired-buffer t)
  (setopt dired-listing-switches "-al --group-directories-first")

  (setopt dired-backup-overwrite 'always)
  (setopt dired-vc-rename-file t)

  (setopt dired-clean-confirm-killing-deleted-buffers nil)
  (setopt dired-clean-up-buffers-too t)

  (setopt dired-create-destination-dirs 'ask
          dired-create-destination-dirs-on-trailing-dirsep t
          dired-recursive-deletes 'always
          dired-recursive-copies 'always)

  (setopt delete-by-moving-to-trash t)

  (setopt dired-mouse-drag-files t)
  (setopt mouse-drag-and-drop-region-cross-program t))

;; ~dired-subtree~ :: insert subdirs arboreally


(package! dired-subtree
  (after! dired
    (define-keymap :keymap dired-mode-map
      "<tab>" #'dired-subtree-toggle
      "TAB" #'dired-subtree-toggle
      "<backtab>" #'dired-subtree-remove
      "S-TAB" #'dired-subtree-remove)))

(after! dired-subtree
  (setopt dired-subtree-use-backgrounds nil))

;; ~trashed~ :: interact with operating system trash diredly


(package! trashed)

(after! trashed
  (setopt trashed-action-confirmer #'y-or-n-p)
  (setopt trashed-use-header-line t)
  (setopt trashed-sort-key '("Date deleted" . t))
  (setopt trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; ~diredfl~ :: additional syntax highlighting in dired buffers

;; + Package :: <https://github.com/purcell/diredfl>


(package! diredfl
  (add-hook 'ceamx-after-init-hook #'diredfl-global-mode)
  (after! diredfl
    (set-face-attribute 'diredfl-dir-name nil :bold t)))

;; ~nerd-icons-dired~ :: icons for list items :icons:


(package! nerd-icons-dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;; ~dired-preview~ :: a file previewer

;; + Website :: <https://protesilaos.com/emacs/dired-preview>


(package! dired-preview
  (defer! 3
    (dired-preview-global-mode))
  (after! dired-preview
    (setopt dired-preview-delay 0.7)
    (setopt dired-preview-max-size (expt 2 20))
    (setopt dired-preview-ignored-extensions-regexp
            (concat "\\."
                    "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                    "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                    "\\|iso\\|epub\\|pdf\\)"))))

(provide 'ceamx-init-dired)
;;; ceamx-init-dired.el ends here
