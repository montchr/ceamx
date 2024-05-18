;;; lib-elisp.el --- Helper utilities for Emacs Lisp development and language support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery
;; Copyright (C) 2018  Adam Porter

;; Author: Chris Montgomery <chmont@proton.me>
;;         Adam Porter <adam@alphapapa.net>
;; Keywords: local, lisp, tools, internal, convenience

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

;;; Sources:

;; <https://github.com/alphapapa/emacs-package-dev-handbook/blob/4a78d753e965bc2cb87f72a72974a4514c4d18dd/README.org#emacs-lisp-macroreplace>

;;; Code:

;;; Macros

;; via <https://github.com/alphapapa/emacs-package-dev-handbook/blob/master/README.org#debug-warn-macro>
(cl-defmacro debug-warn! (&rest args)
  "Display a debug warning showing the runtime value of ARGS.
The warning automatically includes the name of the containing
function, and it is only displayed if `warning-minimum-log-level'
is `:debug' at expansion time (otherwise the macro expands to nil
and is eliminated by the byte-compiler).  When debugging, the
form also returns nil so, e.g. it may be used in a conditional in
place of nil.

Each of ARGS may be a string, which is displayed as-is, or a
symbol, the value of which is displayed prefixed by its name, or
a Lisp form, which is displayed prefixed by its first symbol.

Before the actual ARGS arguments, you can write keyword
arguments, i.e. alternating keywords and values.  The following
keywords are supported:

  :buffer BUFFER   Name of buffer to pass to `display-warning'.
  :level  LEVEL    Level passed to `display-warning', which see.
                   Default is :debug."
  ;; TODO: Can we use a compiler macro to handle this more elegantly?
  (pcase-let* ((fn-name (when byte-compile-current-buffer
                          (with-current-buffer byte-compile-current-buffer
                            ;; This is a hack, but a nifty one.
                            (save-excursion
                              (beginning-of-defun)
                              (cl-second (read (current-buffer)))))))
                (plist-args (cl-loop while (keywordp (car args))
                              collect (pop args)
                              collect (pop args)))
                ((map (:buffer buffer) (:level level)) plist-args)
                (level (or level :debug))
                (string (cl-loop for arg in args
                          concat (pcase arg
                                   ((pred stringp) "%S ")
                                   ((pred symbolp)
                                     (concat (upcase (symbol-name arg)) ":%S "))
                                   ((pred listp)
                                     (concat "(" (upcase (symbol-name (car arg)))
                                       (pcase (length arg)
                                         (1 ")")
                                         (_ "...)"))
                                       ":%S "))))))
    ;; FIXME: ensure available (from where?) or just defvar
    (when (eq :debug warning-minimum-log-level)
      `(let ((fn-name ,(if fn-name
                         `',fn-name
                         ;; In an interpreted function: use `backtrace-frame' to get the
                         ;; function name (we have to use a little hackery to figure out
                         ;; how far up the frame to look, but this seems to work).
                         `(cl-loop for frame in (backtrace-frames)
                            for fn = (cl-second frame)
                            when (not (or (subrp fn)
                                        (special-form-p fn)
                                        (eq 'backtrace-frames fn)))
                            return (make-symbol (format "%s [interpreted]" fn))))))
         (display-warning fn-name (format ,string ,@args) ,level ,buffer)
         nil))))

;;; Commands

;;;###autoload
(defun ceamx/emacs-lisp-macroreplace ()
  "Replace macro form before or after point with its expansion."
  (interactive)
  (if-let* ((beg (point))
             (end t)
             (form (or (ignore-errors
                         (save-excursion
                           (prog1 (read (current-buffer))
                             (setq end (point)))))
                     (ignore-errors
                       (forward-sexp -1)
                       (setq beg (point))
                       (prog1 (read (current-buffer))
                         (setq end (point))))))
             (expansion (macroexpand-all form)))
    ;; FIXME: replace obsolete function with what?
    (setf (buffer-substring beg end) (pp-to-string expansion))
    (user-error "Unable to expand")))

(provide 'lib-elisp)
;;; lib-elisp.el ends here
