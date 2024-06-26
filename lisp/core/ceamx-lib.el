;;; ceamx-lib.el --- Ceamx common library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2023-2024  Chris Montgomery <chmont@proton.me>
;; Copyright (C) 2014-2023  Henrik Lissner
;; Copyright (C) 2006-2021  Steve Purcell
;; Copyright (C) 2016–2022  Radian LLC and contributors
;; Copyright (C) 2018  Adam Porter
;; Copyright (C) 2013-2021  Bailey Ling <bling@live.ca>
;; Copyright (C) 2013-2023  7696122 <7696122@gmail.com>

;; Author: Chris Montgomery <chmont@proton.me>
;;         Henrik Lissner
;;         Steve Purcell
;;         Radon Rosborough <radon@intuitiveexplanations.com>
;;         Adam Porter <adam@alphapapa.net>
;;         Bailey Ling <bling@live.ca>
;;         7696122 <7696122@gmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx

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
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(defun ceamx-host-p (name)
  "Whether Emacs is running on the machine NAME."
  (string= name (system-name)))
(defun ceamx-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)
(defmacro noop! (&rest _body)
  "Do nothing with BODY and return nil.
Unlike `ignore', produce no side effects."
  (declare (indent defun))
  nil)
(require 'cl-lib)

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs
features (aka packages).  PACKAGE may use :or/:any and :and/:all
operators.  The precise format is:

- An unquoted package symbol (the name of a package)
    (after! package-a BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (list (if (or (not (bound-and-true-p byte-compile-current-file))
                    (require package nil 'noerror))
                #'progn
              #'with-no-warnings)
            `(with-eval-after-load ',package ,@body))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defmacro defer! (secs &rest body)
  "Run BODY when Emacs is idle for SECS seconds."
  (declare (indent defun) (debug t))
  `(run-with-idle-timer
    ,secs
    nil
    (lambda () ,@body)))
(defmacro defer-until! (condition &rest body)
  "Run BODY when CONDITION is non-nil.
Leverages checks via `after-load-functions'.
Meant to serve as a predicated alternative to `after!'."
  (declare (indent defun) (debug t))
  `(if ,condition
       (progn ,@body)
     ,(let ((fn (intern (format "ceamx--delay-form-%s-h" (sxhash (cons condition body))))))
       `(progn
          (fset ',fn (lambda (&rest args)
                       (when ,(or condition t)
                        (remove-hook 'after-load-functions #',fn)
                        (unintern ',fn nil)
                        (ignore args)
                        ,@body)))
          (put ',fn 'permanent-local-hook t)
          (add-hook 'after-load-functions #',fn)))))
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))
(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))
(defmacro appendopt! (variable &rest lists)
  "Append LISTS to the existing user option VARIABLE.
This uses `setopt' to set the new value of VARIABLE."
  `(setopt ,variable (append ,variable ,@lists)))
(defmacro prependopt! (variable &rest lists)
  "Prepend LISTS to the existing user option VARIABLE.
This uses `setopt' to set the new value of VARIABLE."
  `(setopt ,variable (append ,@lists ,variable)))
(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.
If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                ,list)))
;; TODO: another version to test car of alist so that new additions with the
;;       same car will override the existing list
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
      (cl-pushnew ,var ,place :test #'equal))))
(defun ceamx-subdirs (parent-dir)
  "Return every non-hidden subdirectory of PARENT-DIR."
  (cl-remove-if-not
   #'file-directory-p
   (directory-files
    (expand-file-name parent-dir) t "^[^\\.]")))
(defmacro def-advice! (name arglist how symbol docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST, DOCSTRING, and BODY are as in `defun'.

HOW and SYMBOL are as in `advice-add'.  HOW describes how to add
the newly-defined advice.  SYMBOL is the function to be advised."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Ceamx: advice `%S' not documented'" name))
  (unless (and (listp symbol)
               (= 2 (length symbol))
               (eq (nth 0 symbol) 'function)
               (symbolp (nth 1 symbol)))
    (error "Ceamx: advice `%S' does not sharp-quote symbol `%S'" name symbol))
  `(progn
     (defun ,name ,arglist
      ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name how))
                          "an"
                        "a")))
        (format "%s\n\nThis is %s `%S' advice for\n`%S'."
         docstring article how
         (if (and (listp symbol)
              (memq (car symbol) ''function))
             (cadr symbol)
           symbol)))
      ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,symbol ',how #',name)
     ',name))
(defmacro def-hook! (name arglist hooks docstring &rest body)
  "Define function NAME and add it to HOOKS.
ARGLIST is as in `defun'.  HOOKS is a list of hooks to which to
add the function, or just a single hook.  DOCSTRING and BODY are
as in `defun'."
  (declare (indent defun)
           (doc-string 4))
  (setq hooks (ensure-list (ceamx-unquote hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Ceamx: no docstring provided for `def-hook!'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
        ,(format "%s\n\nThis function is for use in %s."
          docstring hooks-str)
        ,@body)
       (dolist (hook ',hooks)
        (add-hook hook #',name)))))
(defmacro use-feature! (name &rest args)
  "Configuration-only wrapper for `use-package', passing through NAME and ARGS.

This macro is a wrapper for `use-package' (which see) disabling package
installation by setting package installation keywords to nil.

If `use-package-always-ensure' is non-nil, its effect will be
ignored in this `use-package' macro expansion because `:ensure'
will be nil."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))
(defmacro package! (order &rest body)
  "Declare a package ORDER and its initial configuration BODY.
Provides the necessary autoloads so that we can declare packages
without needing to declare autoloads for `elpaca' in every file."
  (declare (indent defun))
  `(progn
     (autoload 'elpaca "elpaca" nil nil t)
     (elpaca ,order ,@body)))
(defun ceamx-normalize-char (char)
  "Normalize CHAR to a valid character matching `characterp'.
CHAR may either be a valid character or a string convertable to a
character with `string-to-char'.  If CHAR is already a character
matching `characterp', then it will be returned as-is.

When CHAR is a string containing more than one character, only
the first character will be transformed.  See `string-to-char' for
more info.

This function is impure because the interpretation of CHAR can
vary based on... various reasons?"
  (declare (side-effect-free t)
           (obsolete 'string-to-char "2024-03-23"))
  (cl-assert (char-or-string-p char) t)
  (if (stringp char)
      (cond ((length= char 0)
             (user-error "Character string `%s' is empty" char))
            ((length> char 1)
             (user-error "Character string `%s' should only contain a single character" char))
            (t
             (string-to-char char)))
    char))
(defmacro global-keys! (&rest keys)
  "Define keybindings KEYS in the global keymap.
Wrapper for `define-keymap' with `current-global-map' as target keymap."
  (declare (indent defun) (debug t))
  `(define-keymap :keymap (current-global-map)
     ,@keys))
(defun ceamx-repeatify-keymap (repeat-map)
  "Set the `repeat-map' property on all commands bound in REPEAT-MAP."
  (named-let process ((keymap (symbol-value repeat-map)))
    (map-keymap
     (lambda (_key cmd)
       (cond
        ((symbolp cmd) (put cmd 'repeat-map repeat-map))
        ((keymapp cmd) (process cmd))))
     keymap)))

(provide 'ceamx-lib)
;;; ceamx-lib.el ends here
