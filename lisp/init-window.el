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

(use-feature emacs
  :init
  ;; Don't close windows with <ESC> key.
  (defadvice keyboard-escape-quit
      (around keyboard-escape-quit-dont-close-windows activate)
    (let ((buffer-quit-function (lambda () ())))
      ad-do-it)))

(use-feature winner)

(use-package burly
  :elpaca (burly :host github :repo "alphapapa/burly.el")
  :init
  (burly-tabs-mode))

(use-package olivetti :defer t
  :hook (org-mode . olivetti-mode))

;;
;;; popper <https://github.com/karthink/popper> -- "minor-mode to summon and dismiss buffers easily."
;;

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          help-mode
          (lambda (buf) (with-current-buffer buf
                          (and (derived-mode-p 'fundamental-mode)
                               (< (count-lines (point-min) (point-max))
                                  10))))))

  (popper-mode +1)
  (popper-echo-mode +1)                 ; For echo area hints

  :config
  (setq! popper-group-function #'popper-group-by-projectile))


(provide 'init-window)
;;; init-window.el ends here
