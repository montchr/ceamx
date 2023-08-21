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

(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-mouse-drag-files t)                  
(setq mouse-drag-and-drop-region-cross-program t)

;; <https://github.com/alexluigit/dirvish>
;; <https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#Sample-config>
(use-package dirvish
  :defer t
  :after (all-the-icons)

  :commands
  (dirvish-override-dired-mode
   dirvish-peek-mode
   dirvish-side-follow-mode)

  :init
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
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))

  ;; TODO: only when graphical
  (setq mouse-1-click-follows-link nil)
  ;; TODO: only when graphical
  ;; TODO: move to :bind
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (;; ("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))

  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; (use-package dired-x
;;   :config
;;   ;; Make dired-omit-mode hide all "dotfiles"
;;   (setq dired-omit-files
;;         (concat dired-omit-files "\\|^\\..*$")))

(provide 'init-dired)
;;; init-dired.el ends here
