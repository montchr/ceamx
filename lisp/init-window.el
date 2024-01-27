;;; init-window.el --- Window management -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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
(require 'lib-window)

;; Hide buffer until there's output.
;; Prevents an extra window appearing during init.
(setopt async-shell-command-display-buffer nil)

;; FIXME: one of these, i think, is responsible for breaking childframes e.g. `embark-act', `Info-mode'
;; TODO: what do each of these do?
;; (setopt display-buffer-base-action
;;         '((display-buffer-reuse-mode-window
;;            display-buffer-pop-up-window)
;;           (reusable-frames . t)))

(setopt even-window-sizes nil)

(use-feature! winner
  :config (winner-mode))

;;; `olivetti-mode' :: <https://github.com/rnkn/olivetti>
(use-package olivetti
  :commands (olivetti-mode)

  :init
  (add-hook 'org-mode-hook #'olivetti-mode)

  :config
  (setopt olivetti-style 'fancy))

;; via <https://github.com/jwiegley/dot-emacs/blob/master/init.org#shackle>
;; (use-package shackle
;;   :unless alternate-emacs
;;   :demand t
;;   :commands shackle-mode
;;   :custom
;;   (shackle-default-rule '(:select t))
;;   (shackle-rules
;;    '((compilation-mode :select nil :size 0.6)
;;      ("\\`\\*Messages" :select t :align t :size 0.6)
;;      ("\\`\\*company-coq:" :regexp t :noselect t)
;;      ("\\`\\*fetch" :regexp t :size 0.25 :noselect t :align bottom)
;;      ("\\`\\*Flycheck" :regexp t :size 0.2 :noselect t :align bottom)
;;      ("\\`\\*?magit-diff" :regexp t :align bottom :noselect t)))
;;   :config
;;   (shackle-mode 1))

;;; `ace-window' :: <https://github.com/abo-abo/ace-window>
(use-package ace-window
  :after avy)

(provide 'init-window)
;;; init-window.el ends here
