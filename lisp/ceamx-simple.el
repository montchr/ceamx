;;; ceamx-simple.el --- Common utility commands        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery
;; Copyright (C) 2020-2023  Protesilaos Stavrou
;; Copyright (c) 2023  Bruno Boal <egomet@bboal.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;;         Protesilaos Stavrou <info@protesilaos.com>
;;         Bruno Boal <egomet@bboal.com>
;; Keywords: local, convenience

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

;;;; Sources

;; <https://github.com/protesilaos/dotfiles/blob/df9834d8db815920bfd7aacfaf11ef16fa089c53/emacs/.emacs.d/ceamx-lisp/ceamx-simple.el>
;; <https://github.com/BBoal/emacs-config/blob/95520648c5f2ed0784d42e98afff035a6964fd2f/bb-lisp/bb-simple.el>

;;; Code:

;;;; Requirements

;;;; Variables

(defgroup ceamx-simple ()
  "Generic utilities for editing."
  :group 'editing)

(defcustom ceamx-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `ceamx/insert-date'."
  :type 'string
  :group 'ceamx-simple)

(defcustom ceamx-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `ceamx/insert-date'."
  :type 'string
  :group 'ceamx-simple)

(defvar ceamx-point-in-comment-functions ()
  "List of functions to run to determine if point is in a comment.
Each function takes one argument: the position of the point.  Stops on the first
function to return non-nil.")

(defvar ceamx-simple-checkers-buffer-names-regexp
  (rx "*" (or "Flycheck" "Package-Lint")))

(defvar ceamx-occur-grep-modes-list
  '(occur-mode
    grep-mode
    xref--xref-buffer-mode
    flymake-diagnostics-buffer-mode)
  "List of major-modes used in occur-type buffers.")

(defvar ceamx-repl-modes-list
  '(eshell-mode
    inferior-emacs-lisp-mode            ; ielm
    shell-mode
    eat-mode
    nix-repl-mode)
  "List of major-modes used in REPL buffers.")

(defvar ceamx-repl-buffer-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Inferior .*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar ceamx-help-modes-list
  '(helpful-mode
    help-mode
    eldoc-mode)
  "List of major-modes used in documentation buffers.")

(defvar ceamx-help-buffer-names-list
  '("^\\*Apropos"
    "^\\*eldoc\\*")
  "List of buffer names used in help buffers.")

(defvar ceamx-manual-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar ceamx-message-modes-list
  '(compilation-mode
    edebug-eval-mode)
  "List of major-modes used in message buffers.")


;;;; Functions

;;;;; Public

(defun ceamx-simple-buffer-which-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
If BUFFER-OR-NAME is nil, return the current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

(defun ceamx-point-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
POS defaults to the current position."
  (let ((pos (or pos (point))))
    (if ceamx-point-in-comment-functions
        (run-hook-with-args-until-success 'ceamx-point-in-comment-functions pos)
      (nth 4 (syntax-ppss pos)))))

;;;;; Private

(defun ceamx-simple--pos-url-on-line (char)
  "Return position of `ceamx-common-url-regexp' at CHAR."
  (when (integer-or-marker-p char)
    (save-excursion
      (goto-char char)
      (re-search-forward ceamx-common-url-regexp (line-end-position) :noerror))))

;;; Commands

;;;###autoload
(defun ceamx/insert-date (&optional arg)
  "Insert the current date as `ceamx-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `ceamx-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date ceamx-simple-date-specifier)
          (time ceamx-simple-time-specifier)
          (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))


;;;###autoload
(defun ceamx/escape-url-line (char)
  "Escape all URLs or email addresses on the current line.
When called from Lisp CHAR is a buffer position to operate from
until the end of the line.  In interactive use, CHAR corresponds
to `line-beginning-position'."
  (interactive
   (list
    (if current-prefix-arg
        (re-search-forward
         ceamx-common-url-regexp
         (line-end-position) :no-error
         (prefix-numeric-value current-prefix-arg))
      (line-beginning-position))))
  (when-let ((regexp-end (ceamx-simple--pos-url-on-line char)))
    (goto-char regexp-end)
    (unless (looking-at ">")
      (insert ">")
      (when (search-backward "\s" (line-beginning-position) :noerror)
        (forward-char 1))
      (insert "<"))
    (ceamx/escape-url-line (1+ regexp-end)))
  (goto-char (line-end-position)))

;;;###autoload
(defun ceamx/escape-url-region (&optional beg end)
  "Apply `ceamx/escape-url-line' on region lines between BEG and END."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (error "There is no region!")))
  (let ((beg (min beg end))
        (end (max beg end)))
    (save-excursion
      (goto-char beg)
      (setq beg (line-beginning-position))
      (while (<= beg end)
        (ceamx/escape-url-line beg)
        (beginning-of-line 2)
        (setq beg (point))))))

;;;###autoload
(defun ceamx/escape-url-dwim ()
  "Escape URL on the current line or lines implied by the active region.
Call the commands `ceamx/escape-url-line' and
`ceamx/escape-url-region' ."
  (interactive)
  (if (region-active-p)
    (ceamx/escape-url-region (region-beginning) (region-end))
    (ceamx/escape-url-line (line-beginning-position))))

;;;###autoload
(defun ceamx/mark-symbol-at-point ()
  "Select the symbol under cursor.
Copied from the `mc--mark-symbol-at-point' function from the
`multiple-cursors' package."
  (interactive)
  (when (not (use-region-p))
    (let ((b (bounds-of-thing-at-point 'symbol)))
      (goto-char (car b))
      (set-mark (cdr b)))))

;; via <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L1781-L1797>
;;;###autoload
(defun ceamx/continue-comment ()
  "Continue current comment, preserving trailing whitespace.
This differs from `default-indent-new-line' in the following way:

If you have a comment like \";; Some text\" with point at the end of the
line, then running `default-indent-new-line' will get you a new line
with \";; \", but running it again will get you a line with only
\";;\" (no trailing whitespace).  This is annoying for inserting a new
paragraph in a comment.  With this command, the two inserted lines are
the same."
  (interactive)
  ;; `default-indent-new-line' uses `delete-horizontal-space'
  ;; because in auto-filling we want to avoid the space character at
  ;; the end of the line from being put at the beginning of the next
  ;; line.  But when continuing a comment it's not desired.
  (cl-letf (((symbol-function #'delete-horizontal-space) #'ignore))
    (default-indent-new-line)))

(provide 'ceamx-simple)
;;; ceamx-simple.el ends here
