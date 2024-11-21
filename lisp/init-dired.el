;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

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

;;; Dired, the Directory Editor

(with-eval-after-load 'dired
  (setopt dired-auto-revert-buffer t)
  (setopt dired-dwim-target t)
  (setopt dired-kill-when-opening-new-dired-buffer t)
  (setopt dired-listing-switches "-al --group-directories-first")

  (setopt dired-backup-overwrite t)
  (setopt dired-vc-rename-file t)

  ;; TODO: difference between these two?
  (setopt dired-clean-confirm-killing-deleted-buffers nil)
  (setopt dired-clean-up-buffers-too t)

  (setopt dired-create-destination-dirs 'ask
          dired-create-destination-dirs-on-trailing-dirsep t
          dired-recursive-deletes 'always
          dired-recursive-copies 'top)

  (setopt dired-mouse-drag-files t)
  ;; TODO: does this really belong here?
  (setopt mouse-drag-and-drop-region-cross-program t)

  ;; Omit "uninteresting" files.
  ;; See `dired-omit-files', `dired-omit-lines', `dired-omit-extensions'
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  ;; TODO: find a better binding
  ;; (define-keymap :keymap dired-mode-map
  ;;   "M-p" #'dired-up-directory)
  )

;;; Provide addtional syntax highlighting for Dired with ~diredfl~

;; <https://github.com/purcell/diredfl>

(package! diredfl
  (add-hook 'dired-mode-hook #'diredfl-mode)

  (with-eval-after-load 'diredfl
    (set-face-attribute 'diredfl-dir-name nil :bold t)))

;;; ~dired-preview~, a file previewer for Dired

;; - website :: <https://protesilaos.com/emacs/dired-preview>

(package! dired-preview
  (setopt dired-preview-delay 0.7)
  (setopt dired-preview-max-size (expt 2 20))
  (setopt dired-preview-ignored-extensions-regexp
          (concat "\\."
                  "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                  "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                  "\\|iso\\|epub\\|pdf\\)"))

  (dired-preview-global-mode))

(provide 'init-dired)
;;; init-dired.el ends here
