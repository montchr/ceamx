;;; init-window.el --- Window management -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Not the operating system.

;;; Code:

(use-feature emacs
  :init
  ;; Don't close windows with <ESC> key.
  (defadvice keyboard-escape-quit
      (around keyboard-escape-quit-dont-close-windows activate)
    (let ((buffer-quit-function (lambda () ())))
      ad-do-it)))

(use-feature winner)


(elpaca-use-package (burly :host github :repo "alphapapa/burly.el")
  :general
  (+general-global-bookmark
    "F" #'burly-bookmark-frames
    "W" #'burly-bookmark-windows)
  (+general-global-frame
    "b" '(burly-bookmark-frames :which-key "bmark"))
  (+general-global-window
    "b" '(burly-bookmark-windows :which-key "bmark"))
  (+general-global-tabs
    "R" #'burly-reset-tab)

  :init
  (burly-tabs-mode))

(provide 'init-window)
;;; init-window.el ends here
