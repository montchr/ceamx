;;; lib-hydras.el --- Hydra helpers -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>
;; Copyright (c) 2016-2023  John Kitchin <jkitchin@andrew.cmu.edu> and the Scimax contributors

;; Maintainer:  Chris Montgomery <chris@cdom.io>
;; Author:  Chris Montgomery <chris@cdom.io>
;;          John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Modified: 10 July 2023
;; Created: 09 July 2023
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

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;;; Commentary:

;;  Helpers for working with hydras.

;;;; Sources/References:

;;  - <https://github.com/jkitchin/scimax/blob/43d91a4e218af8e3d807c1138b3489355767bf24/scimax-hydra.el>
;;  - <https://github.com/abo-abo/hydra/wiki/Nesting-Hydras#visiting-other-hydras-temporarily>

;;

;;; Code:

(defvar cmx-hydra-stack '())

(defun cmx-hydra-push (expr)
  "Push an EXPR onto the stack."
  (push expr cmx-hydra-stack))

(defun cmx-hydra-pop ()
  "Pop an expression off the stack and call it."
  (interactive)
  (let ((x (pop cmx-hydra-stack)))
    (when x
	    (call-interactively x))))

(defun cmx-hydra-show ()
  "Show the current stack."
  (interactive)
  (with-help-window (help-buffer)
    (princ "cmx-hydra-stack\n")
    (pp cmx-hydra-stack)))

(defun cmx-hydra-reset ()
  "Reset the stack to empty."
  (interactive)
  (setq cmx-hydra-stack '()))

(defun cmx-hydra-help ()
  "Show help buffer for current hydra."
  (interactive)
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (unless (featurep 'emacs-keybinding-command-tooltip-mode)
	      (require 'emacs-keybinding-command-tooltip-mode))
      (emacs-keybinding-command-tooltip-mode +1))
    (let ((s (format "Help for %s\n" hydra-curr-body-fn)))
      (princ s)
      (princ (make-string (length s) ?-))
      (princ "\n"))

    (princ (mapconcat
	          (lambda (head)
	            (format "%s%s"
		                  ;;  key
		                  (s-pad-right 10 " " (car head))
		                  ;; command
		                  (let* ((hint (if (stringp (nth 2 head))
				                               (concat " " (nth 2 head))
				                             ""))
			                       (cmd (cond
				                           ;; quit
				                           ((null (nth 1 head))
				                            "")
				                           ;; a symbol
				                           ((symbolp (nth 1 head))
				                            (format "`%s'" (nth 1 head)))
				                           ((and (listp (nth 1 head))
					                               (eq 'enter-hydra! (car (nth 1 head))))
				                            (format "`%s'" (nth 1 (nth 1 head))))
				                           ((listp (nth 1 head))
				                            (with-temp-buffer
				                              (pp (nth 1 head) (current-buffer))
				                              (let ((fill-prefix (make-string 10 ? )))
					                              (indent-code-rigidly
					                               (save-excursion
					                                 (goto-char (point-min))
					                                 (forward-line)
					                                 (point))
					                               (point-max) 10))
				                              (buffer-string)))
				                           (t
				                            (format "%s" (nth 1 head)))))
			                       (l1 (format "%s%s" (s-pad-right 50 " " (car (split-string cmd "\n"))) hint))
			                       (s (s-join "\n" (append (list l1) (cdr (split-string cmd "\n"))))))
			                  (s-pad-right 50 " " s))))
	          (symbol-value
	           (intern
	            (replace-regexp-in-string
	             "/body$" "/heads"
	             (symbol-name  hydra-curr-body-fn))))
	          "\n"))))


;;
;;; Macros
;;

(defmacro enter-hydra! (hydra)
  "Push current HYDRA to a stack.
This is a macro so I don't have to quote the hydra name."
  `(progn
     (cmx-hydra-push hydra-curr-body-fn)
     (call-interactively ',hydra)))


(provide 'lib-hydras)
;;; lib-hydras.el ends here
