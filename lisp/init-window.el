;;; init-window.el --- Window management -*- lexical-binding: t -*-

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

;;; Code:

(require 'lib-common)

(defadvice keyboard-escape-quit
    (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))

;; Disable buffer line wrapping by default.
;; <https://www.emacswiki.org/emacs/TruncateLines>
(set-default 'truncate-lines t)

;; Hide buffer until there's output.
;; Prevents an extra window appearing during init.
(setopt async-shell-command-display-buffer nil)

(setopt display-buffer-base-action
        '((display-buffer-reuse-mode-window display-buffer-pop-up-window)
          (reusable-frames . t)))
(setopt even-window-sizes nil)

(use-feature winner
  :config (winner-mode))

(use-package burly
  :elpaca (burly :host github :repo "alphapapa/burly.el")
  :init
  (burly-tabs-mode))

(use-package olivetti :defer t
  :hook (org-mode . olivetti-mode))

;;
;;; popper -- <https://github.com/karthink/popper>
;;  "minor-mode to summon and dismiss buffers easily."

(use-package popper
  :diminish
  :commands (popper-mode
             popper-echo-mode
             popper-group-by-projectile)
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq! popper-reference-buffers
         '("\\*Messages\\*"
           "Output\\*$"
           "\\*vc\\*"
           "\\*Warnings\\*"
           "\\*elpaca-log\\*"
           compilation-mode
           help-mode
           helpful-mode
           (lambda (buf) (with-current-buffer buf
                      (and (derived-mode-p 'fundamental-mode)
                           (< (count-lines (point-min) (point-max))
                              10))))))

  (popper-mode +1)
  (popper-echo-mode +1)                 ; For echo area hints

  :config
  ;; <https://github.com/karthink/popper?tab=readme-ov-file#popup-placement-controlled-using-display-buffer-alist-or-shackleel>
  (setq popper-display-control t)

  (after! 'projectile
    (setq! popper-group-function #'popper-group-by-projectile)))

;;
;;; `ace-window' :: <https://github.com/abo-abo/ace-window>
;;

(use-package ace-window
  :after avy)

;;
;;; Window management hydra
;;

;; Depends on `ace-window', `projectile', and of course, `hydra'.
;; Maybe others too.
;;
;; Bound in `cmx-leader-keymap'

(defhydra cmx-hydra/window (:hint nil)
  "
---------------------------------------------------------
^Movement^    ^Split^         ^Switch^		  ^Resize^
---------------------------------------------------------
  ^_k_^       _v_ertical      _b_uffer		  _q_ X←
_h_   _l_     _x_ horizontal	_f_ind files	_w_ X↓
  ^_j_^       _z_ undo      	_a_ce 1		    _e_ X↑
              _Z_ reset      	_s_wap		    _r_ X→
_F_ollow      _D_lt Other   	_S_ave		    max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete
"
  ;; I usually only need to move one window at a time,
  ;; so red breaks flow.
  ("h" #'windmove-left   :color blue)
  ("j" #'windmove-down   :color blue)
  ("k" #'windmove-up     :color blue)
  ("l" #'windmove-right  :color blue)

  ("q" #'hydra-move-splitter-left)
  ("w" #'hydra-move-splitter-down)
  ("e" #'hydra-move-splitter-up)
  ("r" #'hydra-move-splitter-right)
  ("b" #'consult-buffer)
  ("f" #'projectile-find-file)
  ("F" #'follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         ;; FIXME: hook does not exist?
         (add-hook 'ace-window-end-once-hook #'cmx-hydra/window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         ;; FIXME: hook does not exist?
         (add-hook 'ace-window-end-once-hook #'cmx-hydra/window/body)))
  ("S" #'burly-bookmark-windows)
  ("d" #'delete-window :color blue)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         ;; FIXME: hook does not exist?
         (add-hook 'ace-window-end-once-hook #'cmx-hydra/window/body)))
  ("o" #'delete-other-windows  :color blue)
  ("i" #'ace-maximize-window   :color blue)
  ("z" (progn
         (winner-undo)
         (setq this-command #'winner-undo)))
  ("Z" #'winner-redo)
  ("SPC" nil))

(provide 'init-window)
;;; init-window.el ends here
