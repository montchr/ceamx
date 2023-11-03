;;; lib-keys-evil.el --- Evil helpers                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery
;; Copyright (C) 2014-2023  Henrik Lissner

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

;; Functions of evil.

;;; Sources:

;; <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/editor/evil/autoload/embrace.el>

;;; Code:

(require 'lib-text)

(autoload 'evil-apply-on-block "evil")
(autoload 'evil-define-operator "evil")
(autoload 'evil-delete "evil")
(autoload 'evil-window-split "evil")
(autoload 'evil-window-vsplit "evil")
(autoload 'wgrep-mark-deletion "wgrep")

;; TODO: move to a config file? idk...
(defvar evil-split-window-below)
(defvar evil-vsplit-window-right)

;;;###autoload
(defun cmx/save-and-kill-this-buffer ()
  "Save and kill the current buffer."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;;;###autoload
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;;;###autoload
(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(defun +evil/shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun +evil/shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;
;;; Operators
;;

;; FIXME: these have side effects, move to `init-keys-evil'

;;;###autoload
(evil-define-operator +evil-delete (beg end type register yank-handler)
  "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
  (interactive "<R><x><y>")
  (condition-case _ex
      (evil-delete beg end type register yank-handler)
    ('text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))

;;
;;; Advice
;;

(defun +evil-escape-a (&rest _)
  "Call `cmx/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'cmx/escape)))

(provide 'lib-keys-evil)
;;; lib-keys-evil.el ends here
