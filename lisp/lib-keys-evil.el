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

;;; window management

(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (window-in-direction direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'evil-window-move-far-left)
                   ('right #'evil-window-move-far-right)
                   ('up    #'evil-window-move-very-top)
                   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (scratch-buffer))
        (setq that-buffer (window-buffer that-window)))
      (window-swap-states this-window that-window)
      (select-window that-window))))

;;;###autoload
(defun cmx/evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun cmx/evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun cmx/evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun cmx/evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))

;;; `evil-embrace'

;;;###autoload
(defun +evil--embrace-get-pair (char)
  (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
      pair
    (if-let* ((pair (assoc-default char embrace--pairs-list)))
        (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                  (funcall (embrace-pair-struct-read-function pair)))))
            real-pair
          (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
      (cons char char))))

;;;###autoload
(defun +evil--embrace-escaped ()
  "Backslash-escaped surround character support for embrace."
  (let ((char (read-char "\\")))
    (if (eq char 27)
        (cons "" "")
      (let* ((pair (+evil--embrace-get-pair (string char)))
             (escape (if (sp-point-in-string) "\\\\" "\\"))
             (escape (format "\\1%s" (regexp-quote escape))))
        (cons (replace-regexp-in-string "^\\( *\\)" escape (car pair))
              (replace-regexp-in-string "^\\( *\\)" escape (cdr pair)))))))

;;;###autoload
(defun +evil--embrace-elisp-fn ()
  "Elisp function support for embrace."
  (cons (format "(%s " (or (read-string "(") "")) ")"))

;;;###autoload
(defun +evil--embrace-angle-brackets ()
  "Type/generic angle brackets."
  (cons (format "%s<" (or (read-string "") ""))
        ">"))

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

(provide 'lib-keys-evil)
;;; lib-keys-evil.el ends here
