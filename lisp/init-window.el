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
(setq async-shell-command-display-buffer nil)

;; Equally-sized windows are generally undesirable as a default.
;;
;; While it may be appropriate for primary buffers,
;; the vast majority of buffers Emacs throws at us
;; are only useful for a short period of time,
;; and so they should be treated as "popups".
;; Same goes for buffers we invoke manually.
;;
;; For example, Embark is great, but should not take up as much visual space
;; as the file-visiting buffer from which it was invoked.
(setq! display-buffer-base-action
       '((display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . t)))
(setq! even-window-sizes nil)

;; TODO: remove? also see <https://github.com/nex3/perspective-el?tab=readme-ov-file#some-musings-on-emacs-window-layouts>
(use-feature winner)

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
  (setq popper-display-control nil)

  (after! 'projectile
    (setq! popper-group-function #'popper-group-by-projectile)))


(provide 'init-window)
;;; init-window.el ends here
