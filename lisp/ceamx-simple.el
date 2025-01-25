;;; ceamx-simple.el --- Common utility commands        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery
;; Copyright (C) 2020-2024  Protesilaos Stavrou
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

(require 'ceamx-lib)

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

(defvar ceamx-simple-point-in-comment-functions ()
  "List of functions to run to determine if point is in a comment.
Each function takes one argument: the position of the point.  Stops on the first
function to return non-nil.")

(defvar ceamx-simple-checkers-buffer-names-regexp
  (rx "*" (or "Flycheck" "Package-Lint")))

(defvar ceamx-simple-grep-modes-list
  '(occur-mode
    grep-mode
    xref--xref-buffer-mode
    flymake-diagnostics-buffer-mode)
  "List of major-modes used in occur-type buffers.")

(defvar ceamx-simple-repl-modes-list
  '(eshell-mode
    inferior-emacs-lisp-mode            ; ielm
    shell-mode
    eat-mode
    nix-repl-mode)
  "List of major-modes used in REPL buffers.")

(defvar ceamx-simple-repl-buffer-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Inferior .*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar ceamx-simple-help-modes-list
  '(helpful-mode
    help-mode
    eldoc-mode)
  "List of major-modes used in documentation buffers.")

(defvar ceamx-simple-help-buffer-names-list
  '("^\\*Apropos"
    "^\\*eldoc\\*")
  "List of buffer names used in help buffers.")

(defvar ceamx-simple-manual-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar ceamx-simple-message-modes-list
  '(compilation-mode
    edebug-eval-mode)
  "List of major-modes used in message buffers.")


;;;; Functions

(defun ceamx-simple-buffer-which-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
If BUFFER-OR-NAME is nil, return the current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))

(defun ceamx-simple-point-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
POS defaults to the current position."
  (let ((pos (or pos (point))))
    (if ceamx-simple-point-in-comment-functions
        (run-hook-with-args-until-success 'ceamx-point-in-comment-functions pos)
      (nth 4 (syntax-ppss pos)))))

(defun ceamx-simple--pos-url-on-line (char)
  "Return position of `ceamx-url-regexp' at CHAR."
  (when (integer-or-marker-p char)
    (save-excursion
      (goto-char char)
      (re-search-forward ceamx-url-regexp (line-end-position) :noerror))))

;; FIXME: is this supposed to work on save? not working in either magit or projectile
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L369-L391>
(defun ceamx-simple--update-file-refs (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))
      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))
      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))
        (when (and
               (bound-and-true-p projectile-mode)
               ;; FIXME: de-doom
               ;; (doom-project-p)
               ;; (projectile-file-cached-p file (doom-project-root))
               )
          (projectile-purge-file-from-cache file)))
      )
    (dolist (default-directory toplevels)
      (magit-refresh))
    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))

(defun ceamx-simple--mark (bounds)
  "Mark between BOUNDS as a cons cell of beginning and end positions."
  (push-mark (car bounds))
  (goto-char (cdr bounds))
  (activate-mark))

;;; Commands

;;;###autoload
(defun ceamx/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;;;###autoload
(defun ceamx-simple/new-line-below (n)
  "Create N empty lines below the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (goto-char (line-end-position))
  (dotimes (_ n) (insert "\n")))

;;;###autoload
(defun ceamx-simple/new-line-above (n)
  "Create N empty lines above the current one.
When called interactively without a prefix numeric argument, N is
1."
  (interactive "p")
  (let ((point-min (point-min)))
    (if (or (bobp)
            (eq (point) point-min)
            (eq (line-number-at-pos point-min) 1))
        (progn
          (goto-char (line-beginning-position))
          (dotimes (_ n) (insert "\n"))
          (forward-line (- n)))
      (forward-line (- n))
      (ceamx-simple/new-line-below n))))

;;;###autoload
(defun ceamx-simple/copy-line ()
  "Copy the current line to the `kill-ring'."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

;;;###autoload
(defun ceamx-simple/kill-ring-save (beg end)
  "Copy the current region or line.
When the region is active, use `kill-ring-save' between the BEG and END
positions.  Otherwise, copy the current line."
  (interactive "r")
  (if (region-active-p)
      (kill-ring-save beg end)
    (ceamx-simple/copy-line)))

(defun ceamx-simple--duplicate-buffer-substring (boundaries)
  "Duplicate buffer substring between BOUNDARIES.
BOUNDARIES is a cons cell representing buffer positions."
  (unless (consp boundaries)
    (error "`%s' is not a cons cell" boundaries))
  (let ((beg (car boundaries))
        (end (cdr boundaries)))
    (goto-char end)
    (newline)
    (insert (buffer-substring-no-properties beg end))))

;;;###autoload
(defun ceamx-simple/duplicate-line-or-region ()
  "Duplicate the current line or active region."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (ceamx-simple--duplicate-buffer-substring
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun ceamx-simple/yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (line-beginning-position) (line-end-position)))
  (yank))

;;;###autoload
(defun ceamx-simple/mark-sexp ()
  "Mark symbolic expression at or near point.
Repeat to extend the region forward to the next symbolic
expression."
  (interactive)
  (if (and (region-active-p)
           (eq last-command this-command))
      (ignore-errors (forward-sexp 1))
    (when-let* ((thing (cond
                        ((thing-at-point 'url) 'url)
                        ((thing-at-point 'sexp) 'sexp)
                        ((thing-at-point 'string) 'string)
                        ((thing-at-point 'word) 'word))))
      (ceamx-simple--mark (bounds-of-thing-at-point thing)))))

;;;###autoload
(defun ceamx-simple/insert-date (&optional arg)
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
(defun ceamx-simple/escape-url-line (char)
  "Escape all URLs or email addresses on the current line.
When called from Lisp CHAR is a buffer position to operate from
until the end of the line.  In interactive use, CHAR corresponds
to `line-beginning-position'."
  (interactive
   (list
    (if current-prefix-arg
        (re-search-forward
         ceamx-url-regexp
         (line-end-position) :no-error
         (prefix-numeric-value current-prefix-arg))
      (line-beginning-position))))
  (when-let* ((regexp-end (ceamx-simple--pos-url-on-line char)))
    (goto-char regexp-end)
    (unless (looking-at ">")
      (insert ">")
      (when (search-backward "\s" (line-beginning-position) :noerror)
        (forward-char 1))
      (insert "<"))
    (ceamx-simple/escape-url-line (1+ regexp-end)))
  (goto-char (line-end-position)))

;;;###autoload
(defun ceamx-simple/escape-url-region (&optional beg end)
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
        (ceamx-simple/escape-url-line beg)
        (beginning-of-line 2)
        (setq beg (point))))))

;;;###autoload
(defun ceamx-simple/escape-url-dwim ()
  "Escape URL on the current line or lines implied by the active region.
Call the commands `ceamx/escape-url-line' and
`ceamx/escape-url-region' ."
  (interactive)
  (if (region-active-p)
    (ceamx-simple/escape-url-region (region-beginning) (region-end))
    (ceamx-simple/escape-url-line (line-beginning-position))))

;;;###autoload
(defun ceamx-simple/zap-to-char-backward (char &optional arg)
  "Backward `zap-to-char' for CHAR.
Optional ARG is a numeric prefix to match ARGth occurance of
CHAR."
  (interactive
   (list
    (read-char-from-minibuffer "Zap to char: " nil 'read-char-history)
    (prefix-numeric-value current-prefix-arg)))
  (zap-to-char (- arg) char t))

;; TODO: support point at bol
;; via <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L1781-L1797>
;;;###autoload
(defun ceamx-simple/continue-comment ()
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

;;;;; Buffers

(defun ceamx-simple--buffer-major-mode-prompt ()
  "Prompt of `ceamx-simple/buffers-major-mode'.
Limit list of buffers to those matching the current
`major-mode' or its derivatives."
  (let ((read-buffer-function nil)
        (current-major-mode major-mode))
    (read-buffer
     (format "Buffer for %s: " major-mode)
     nil
     :require-match
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair)
         (derived-mode-p current-major-mode))))))

;;;###autoload
(defun ceamx-simple/buffers-major-mode ()
  "Select BUFFER matching the current one's major mode."
  (interactive)
  (switch-to-buffer (ceamx-simple--buffer-major-mode-prompt)))

(defun ceamx-simple--buffer-vc-root-prompt ()
  "Prompt of `ceamx-simple/buffers-vc-root'."
  (let ((root (or (vc-root-dir)
                  (locate-dominating-file "." ".git")))
        (read-buffer-function nil))
    (read-buffer
     (format "Buffers in %s: " root)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (string-match-p root default-directory))))))

;;;###autoload
(defun ceamx-simple/buffers-vc-root ()
  "Select buffer matching the current one's VC root."
  (interactive)
  (switch-to-buffer (ceamx-simple--buffer-vc-root-prompt)))

;; FIXME: this does not actually kill its buffers -- buffer must be deleted manually
;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L397-L424>
(defun ceamx-simple/delete-current-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; Ensures that windows displaying this buffer will be switched to
          ;; real buffers (`doom-real-buffer-p')
          ;; FIXME: implement -- invent the universe -- but the stuff within is very useful to us (e.g. doom-real-buffer-p and filtering buffers)...
          ;; (doom/kill-this-buffer-in-all-windows buf t)
          ;; TODO: remove when the above is implemented -- `kill-this-buffer' only removes the one buffer
          (kill-this-buffer)
          (ceamx-simple--update-file-refs path)
          (message "Deleted %S" short-path))))))

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L427-L441>
(defun ceamx-simple/copy-current-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (ceamx-simple--update-file-refs old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/lib/files.el#L427-L441>
(defun ceamx-simple/move-current-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    ;; FIXME: save file immediately to trigger auto-encryption
    ;; (when (and auto-encryption-mode
    ;;            (epa-file-name-p new-path))
    ;;   (save-buffer buffer-file-name))
    (ceamx-simple--update-file-refs old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))
    ;; TODO: encrypt/decrypt based on old/new paths
    ;; (cond
    ;;  ((and (not (epa-file-name-p old-path))
    ;;        (epa-file-name-p new-path))
    ;;   (epa-encrypt-file new-path epa-file-encrypt-to)
    ;;   (message "File encrypted and moved to %S" (abbreviate-file-name new-path)))
    ;;  ((and (epa-file-name-p old-path)
    ;;        (not (epa-file-name-p new-path)))
    ;;   (epa-decrypt-file new-path)
    ;;   (message "File *de*crypted and moved to %S" (abbreviate-file-name new-path)))
    ;;  (nil
    ;;   (message "File moved to %S" (abbreviate-file-name new-path))))
    ))

;; via <https://protesilaos.com/emacs/dotemacs#h:f15bc843-2dd1-4792-95ec-9b7b6e561804>
;;;###autoload
(defun ceamx-simple/kill-buffer (buffer)
  "Kill BUFFER without confirmation.
When called interactively, prompt for BUFFER.  Otherwise, kill the
current buffer."
  (interactive (list (read-buffer "Select buffer: ")))
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (or buffer (current-buffer)))))

;; via prot-emacs
;;;###autoload
(defun ceamx-simple/kill-current-buffer (&optional arg)
  "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well.  Kill the window regardless of ARG if it
satisfies `ceamx-window-small-p' and it has no previous
buffers in its history."
  (interactive "P")
  (let ((kill-buffer-query-functions nil))
    (if (or (and (ceamx-window-small-p)
                 (null (window-prev-buffers)))
            (and arg (not (one-window-p))))
        (kill-buffer-and-window)
      (kill-buffer))))

;;;###autoload
(defun ceamx-simple/diff-with-file (&optional arg)
  (interactive "P")
  (let ((buffer (when arg (current-buffer))))
    (diff-buffer-with-file buffer)))

;; via <https://github.com/tarsius/fwb-cmds/blob/88e823809067983acfaeafa57d0bb6e889429ad2/fwb-cmds.el#L140C1-L156C78>
;;;###autoload
(defun ceamx-simple/sudo-find-file (&optional arg)
  "Edit the visited file as \"root\".
If the current buffer does not visit a file, the visited file is
writable or with a prefix argument, then read a file to visit."
  (interactive "P")
  (require 'tramp)
  (if (or arg
          (not buffer-file-name)
          (file-writable-p buffer-file-name))
      (let ((default-directory
             (concat "/sudo:root@localhost:" default-directory)))
        (apply #'find-file
               (find-file-read-args
                "Find file: "
                (confirm-nonexistent-file-or-buffer))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;; Movement

;;;###autoload
(defun ceamx-simple/multi-line-below ()
  "Move half a screen below."
  (interactive)
  (forward-line (floor (window-height) 2))
  (setq this-command 'scroll-up-command))

;;;###autoload
(defun ceamx-simple/multi-line-above ()
  "Move half a screen above."
  (interactive)
  (forward-line (- (floor (window-height) 2)))
  (setq this-command 'scroll-down-command))

;;;###autoload
(defun ceamx-simple/kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))

(provide 'ceamx-simple)
;;; ceamx-simple.el ends here
