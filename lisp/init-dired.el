;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Configuration for Dired

;;; Code:

;; FIXME: errors (prob incompatible with `exa')
(setopt dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(setopt dired-kill-when-opening-new-dired-buffer t)
(setopt dired-mouse-drag-files t)
(setopt mouse-drag-and-drop-region-cross-program t)

;;; `dirvish' :: <https://github.com/alexluigit/dirvish>
;;
;;  <https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#Sample-config>
(use-package dirvish
  :defer t
  :after (all-the-icons)

  :commands (dirvish-override-dired-mode
             dirvish-peek-mode
             dirvish-side-follow-mode)

  :init
  ;; FIXME: not working? too early?
  (dirvish-override-dired-mode)

  ;; TODO
  ;; :custom
  ;; (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
  ;;  '(("h" "~/"                          "Home")
  ;;    ("d" "~/Downloads/"                "Downloads")
  ;;    ("m" "/mnt/"                       "Drives")
  ;;    ("t" "~/.local/share/Trash/files/" "TrashCan")))

  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'

  (setopt mouse-1-click-follows-link nil)
  (setopt dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setopt dirvish-attributes
        '(all-the-icons
          file-time
          file-size
          collapse
          subtree-state
          vc-state
          git-msg))

  (define-keymap :keymap dirvish-mode-map
    "<mouse-1>" #'dirvish-subtree-toggle-or-open
    "<mouse-2>" #'dired-mouse-find-file-other-window
    "<mouse-3>" #'dired-mouse-find-file
    "a"   #'dirvish-quick-access
    "f"   #'dirvish-file-info-menu
    "y"   #'dirvish-yank-menu
    "N"   #'dirvish-narrow
    "^"   #'dirvish-history-last
    "h"   #'dirvish-history-jump ; remapped `describe-mode'
    "s"   #'dirvish-quicksort    ; remapped `dired-sort-toggle-or-edit'
    "v"   #'dirvish-vc-menu      ; remapped `dired-view-file'
    "TAB" #'dirvish-subtree-toggle
    "M-f" #'dirvish-history-go-forward
    "M-b" #'dirvish-history-go-backward
    "M-l" #'dirvish-ls-switches-menu
    "M-m" #'dirvish-mark-menu
    "M-t" #'dirvish-layout-toggle
    "M-s" #'dirvish-setup-menu
    "M-e" #'dirvish-emerge-menu
    "M-j" #'dirvish-fd-jump))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))

  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(provide 'init-dired)
;;; init-dired.el ends here
