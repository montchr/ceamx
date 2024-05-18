;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
;;        eza/fd/rg/etc. do this by default but Dired should prob use GNU ls

;;; Code:

(require 'ceamx-lib)

;;; Dired, the Directory Editor

(use-feature! dired
  :commands (dired-omit-mode)

  :config
  (setopt dired-auto-revert-buffer t)
  (setopt dired-dwim-target t)
  (setopt dired-kill-when-opening-new-dired-buffer t)
  (setopt dired-listing-switches "-al --group-directories-first")
  (setopt dired-mouse-drag-files t)

  ;; TODO: does this really belong here?
  (setopt mouse-drag-and-drop-region-cross-program t)

  (define-keymap :keymap dired-mode-map
    "M-p" #'dired-up-directory))

;;; Provide Dired with polished interface and feature enhancements with ~dirvish~

;;  <https://github.com/alexluigit/dirvish>
;;  <https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#Sample-config>

(use-package dirvish
  :commands (dirvish-override-dired-mode
              dirvish-peek-mode
              dirvish-side-follow-mode)

  :init
  (after! dired
    (dirvish-override-dired-mode))

  ;; Omit "uninteresting" files.
  ;; See `dired-omit-files', `dired-omit-lines', `dired-omit-extensions'
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  :config
  (dirvish-peek-mode)                   ; Preview minibuffer file selections
  (dirvish-side-follow-mode)

  ;; TODO: use consts; there's no straightforward way to access these values
  ;; without custom elisp afaik
  ;; FIXME: ensure directories exist!
  (setopt dirvish-quick-access-entries
    '(("c" "~/Documents/cheatsheets/" "Cheatsheets")
       ("D" "~/Downloads/" "Downloads")
       ("r" "~/Documents/reference" "Reference")
       ("n" "~/Documents/notes/" "Notes")))

  (setopt dirvish-mode-line-format
    '( :left (sort symlink)
       :right (omit yank index)))

  ;; previous value, in case:
  ;; (setopt dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state))
  (setopt dirvish-attributes
    '(vc-state
       subtree-state
       nerd-icons
       collapse
       file-time
       file-size))
  (setopt dirvish-subtree-state-style 'nerd)

  ;; <https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#mouse-settings>
  (def-hook! ceamx--dirvish-no-mouse-follows-link (&rest _)
    'dirvish-find-entry-hook
    "Disable `mouse-1-click-follows-link' in `dirvish' buffers."
    (setopt mouse-1-click-follows-link nil))

  (define-keymap :keymap dirvish-mode-map
    ;; NOTE: `mouse-1-click-follows-link' must be nil (see above)
    "<mouse-1>" #'dirvish-subtree-toggle-or-open
    "<mouse-2>" #'dired-mouse-find-file-other-window
    "<mouse-3>" #'dired-mouse-find-file
    "a" #'dirvish-quick-access
    "f" #'dirvish-file-info-menu
    "y" #'dirvish-yank-menu
    "N" #'dirvish-narrow
    "^" #'dirvish-history-last
    "h" #'dirvish-history-jump          ; remapped `describe-mode'
    "s" #'dirvish-quicksort             ; remapped `dired-sort-toggle-or-edit'
    "v" #'dirvish-vc-menu               ; remapped `dired-view-file'
    "q" #'dirvish-quit
    "TAB" #'dirvish-subtree-toggle
    "M-f" #'dirvish-history-go-forward
    "M-b" #'dirvish-history-go-backward
    "M-l" #'dirvish-ls-switches-menu
    "M-m" #'dirvish-mark-menu
    "M-t" #'dirvish-layout-toggle
    "M-s" #'dirvish-setup-menu
    "M-e" #'dirvish-emerge-menu
    "M-j" #'dirvish-fd-jump))

;;; Provide addtional syntax highlighting for Dired with ~diredfl~

;; <https://github.com/purcell/diredfl>

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
    ;; highlight parent and directory preview as well
    (dirvish-directory-view-mode . diredfl-mode))

  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(provide 'init-dired)
;;; init-dired.el ends here
