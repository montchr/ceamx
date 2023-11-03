;;; init-hooks.el --- Custom hook registration        -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Chris Montgomery
;; Copyright (c) 2014-2022  Henrik Lissner
;; SPDX-License-Identifier: GPL-3.0-or-later AND MIT

;; Author: Chris Montgomery <chris@cdom.io>
;;         Henrik Lissner

;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Largely copied from Doom Emacs.

;; <https://github.com/doomemacs/doomemacs/blob/7a7503045850ea83f205de6e71e6d886187f4a22/lisp/doom-ui.el>

;;; Code:

(defvar doom-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defvar doom-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`doom/reload-theme'.")

(defvar doom-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defvar doom-switch-window-hook nil
  "A list of hooks run after changing the focused windows.")

(defvar doom-switch-frame-hook nil
  "A list of hooks run after changing the focused frame.")

;;; TODO...

(provide 'init-hooks)
;;; init-hooks.el ends here
