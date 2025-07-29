;;; ceamx-focus.el --- Ceamx Enhanced Focus          -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: convenience

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

;;; Commentary:

;; More than just `olivetti-mode'.

;;; Code:

;;;; Requirements

(autoload 'diff-hl-mode "diff-hl")
(autoload 'display-line-numbers-mode "display-line-numbers")
(autoload 'flycheck-mode "flycheck")
(autoload 'flymake-mode "flymake")
(autoload 'flyover-mode "flyover")
(autoload 'jinx-mode "jinx")
(autoload 'olivetti-mode "olivetti")
(autoload 'sideline-mode "sideline")

(defgroup ceamx-focus ()
  "Minor mode for focused editing in Ceamx."
  :prefix "ceamx-focus-"
  :group 'ceamx)

;;;; Variables

(defvar ceamx-focus--initial-mode-states ())

;;;; Customization

(defcustom ceamx-focus-disabled-modes-list '( diff-hl-mode
                                              display-line-numbers-mode
                                              flycheck-mode
                                              flymake-mode
                                              flyover-mode
                                              jinx-mode)
  "Modes to disable when `ceamx-focus-mode' is active.
The initial state of each specified mode will be restored upon disabling `ceamx-focus-mode'."
  :type '(list function)
  :group 'ceamx-focus)

(defcustom ceamx-focus-enabled-modes-list '( olivetti-mode)
  "Modes to enable when `ceamx-focus-mode' is active.
The initial state of each specified mode will be restored upon disabling `ceamx-focus-mode'."
  :type '(list function)
  :group 'ceamx-focus)

(defcustom ceamx-focus-force-toggle-enabled-modes t
  "Whether the modes listed in `ceamx-focus-enabled-modes-list' should be
unconditionally disabled when leaving `ceamx-focus-mode'.")

;;;; Functions

(defun ceamx-focus--store-initial-mode-state (mode)
  (when (fboundp mode)
    (cl-pushnew (cons mode (symbol-value mode))
      ceamx-focus--initial-mode-states)))

(defun ceamx-focus-save-state-and-disable-mode (mode)
  (when (fboundp mode)
    (ceamx-focus--store-initial-mode-state mode)
    (funcall mode -1)))

(defun ceamx-focus-save-state-and-enable-mode (mode)
  (when (fboundp mode)
    (ceamx-focus--store-initial-mode-state mode)
    (funcall mode 1)))

(defun ceamx-focus-restore-initial-mode-state (mode)
  (let ((initial (alist-get mode ceamx-focus--initial-mode-states)))
    (when (fboundp mode)
      (funcall mode initial))))

(defun ceamx-focus-restore-all-initial-mode-states ()
  (dolist (mode-state ceamx-focus--initial-mode-states)
    (let ( (mode (car mode-state))
           (was-enabled (cdr mode-state)))
      (when (fboundp mode)
        ;; FIXME: indentation...
        (if (and (not (and ceamx-focus-force-toggle-enabled-modes
                        (memq mode ceamx-focus-enabled-modes-list)))
              was-enabled)
          (funcall mode 1)
          (funcall mode -1))))))

;;;; Modes

(define-minor-mode ceamx-focus-mode
  "Ceamx minor mode for focused editing."
  :init-value nil
  (if ceamx-focus-mode
    (progn
      (dolist (disabled-mode ceamx-focus-disabled-modes-list)
        (ceamx-focus-save-state-and-disable-mode disabled-mode))
      (dolist (enabled-mode ceamx-focus-enabled-modes-list)
        (ceamx-focus-save-state-and-enable-mode enabled-mode)))
    (ceamx-focus-restore-all-initial-mode-states)))

;;;; Footer

(provide 'ceamx-focus)
;;; ceamx-focus.el ends here
