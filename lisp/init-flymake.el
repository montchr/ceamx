;;; init-flymake.el --- Flymake support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@proton.me>

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
;;; Code:

(setopt flymake-no-changes-timeout 1.0
        flymake-wrap-around t
        flymake-fringe-indicator-position 'right-fringe)
(after! flymake
  (define-keymap :keymap flymake-mode-map
    "C-c ! l" #'flymake-show-buffer-diagnostics
    "C-c ! n" #'flymake-goto-next-error
    "C-c ! p" #'flymake-goto-previous-error
    "C-c ! c" #'flymake-show-buffer-diagnostics))

(provide 'init-flymake)
;;; init-flymake.el ends here
