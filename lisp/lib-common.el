;;; lib-common.el --- Common library functions -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Chris Montgomery <chris@cdom.io>
;; Copyright (C) 2014-2023  Henrik Lissner
;; Copyright (C) 2006-2021  Steve Purcell
;; Copyright (C) 2016–2022  Radian LLC and contributors
;; Copyright (C) 2018  Adam Porter
;; Copyright (C) 2013-2021  Bailey Ling <bling@live.ca>
;; Copyright (C) 2013-2023  7696122 <7696122@gmail.com>

;; Author: Chris Montgomery <chris@cdom.io>
;;         Henrik Lissner
;;         Steve Purcell
;;         Radon Rosborough <radon@intuitiveexplanations.com>
;;         Adam Porter <adam@alphapapa.net>
;;         Bailey Ling <bling@live.ca>
;;         7696122 <7696122@gmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Created: 29 January, 2023

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

;; General, common, and generic library functions.

;;; Sources:

;; <https://github.com/doomemacs/doomemacs/blob/03d692f129633e3bf0bd100d91b3ebf3f77db6d1/lisp/doom-lib.el>
;; <https://github.com/radian-software/radian/blob/9a82b6e7395b3f1f143b91f8fe129adf4ef31dc7/emacs/radian.el>

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)

;;;; Helpers

(defun ceamx--resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (ceamx-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
      hook-list
      (cl-loop for hook in hook-list
        if (eq (car-safe hook) 'quote)
        collect (cadr hook)
        else collect (intern (format "%s-hook" (symbol-name hook)))))))

;; TODO: seems probably excessive
(defun ceamx--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                              vars)
                         (while args
                           (push (if singles
                                   (list (pop args))
                                   (cons (pop args) (pop args)))
                             vars))
                         (nreverse vars))
    ;; TODO: use `derived-mode-hook-name'
    for hook in (ceamx--resolve-hook-forms hooks)
    for mode = (string-remove-suffix "-hook" (symbol-name hook))
    append
    (cl-loop for (var . val) in vars
      collect
      (list var val hook
        (intern (format "ceamx--setq-%s-for-%s-h"
                  var mode))))))

;;; Generic

(defun ceamx-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;; (defun ceamx-keyword-intern (str)
;;   "Convert STR (a string) into a keyword (`keywordp')."
;;   (declare (pure t) (side-effect-free t))
;;   (cl-check-type str string)
;;   (intern (concat ":" str)))

;; (defun ceamx-keyword-name (keyword)
;;   "Return the string name of KEYWORD (`keywordp') minus the leading colon."
;;   (declare (pure t) (side-effect-free t))
;;   (cl-check-type keyword keyword)
;;   (substring (symbol-name keyword) 1))

(defalias 'ceamx-partial #'apply-partially)
(defun ceamx-rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun ceamx-lookup-key (keys &rest keymaps)
  "Lookup KEYS in the active or specified KEYMAPS.

Like `lookup-key', but search active keymaps if KEYMAPS is omitted."
  (if keymaps
      (cl-some (ceamx-rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(defmacro cmd! (&rest body)
  "Return (lambda () (interactive) ,@BODY)

A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro cmd!! (command &optional prefix-arg &rest args)
  "Return a closure to interactively invoke COMMAND with ARGS and PREFIX-ARG.

Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))

;; via <https://protesilaos.com/emacs/dotemacs#h:3563ceb5-b70c-4191-9c81-f2f5a202c4da>
(defmacro noop! (&rest _body)
  "Do nothing with BODY and return nil.
Unlike `ignore', produce no side effects."
  (declare (indent defun))
  nil)

;;
;;; Loading

(defmacro add-load-paths! (&rest dirs)
  "Add DIRS to `load-path', relative to the current file.
The current file is the file from which `add-load-paths!' is used.

This macro is identical to Doom\\='s `add-load-path!' macro
except in name and docstring. The name of the macro has been
changed to clarify that this is a variadic macro. This docstring
also corrects an apparent typo in the original, which referred to
a non-existent macro.

Original source: <https://github.com/doomemacs/doomemacs/blob/03d692f129633e3bf0bd100d91b3ebf3f77db6d1/lisp/doom-lib.el#L620-L626>"
  `(let ((default-directory (dir!))
          file-name-handler-alist)
     (dolist (dir (list ,@dirs))
       (cl-pushnew (expand-file-name dir) load-path :test #'string=))))

;; via <https://github.com/bling/dotemacs/blob/97c72c8425c5fb40ca328d1a711822ce0a0cfa26/core/core-boot.el#L53-L74>
(defmacro after! (feature &rest body)
  "Execute BODY after FEATURE has been loaded.

FEATURE may be any one of:
    \\='evil            => (with-eval-after-load \\='evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autoloads\" BODY)
    [evil cider]     => (with-eval-after-load \\='evil
                          (with-eval-after-load \\='cider
                            BODY))

TODO: Doom's implementation is more flexible and handles
undefined symbols, but does not support quoted FEATURE. Some new
macro supporting quoted FEATURE and the features of Doom's
version would probably be ideal."
  (declare (indent 1))
  (cond
    ((vectorp feature)
      (let ((prog (macroexp-progn body)))
        (cl-loop for f across feature
          do
          (progn
            (setq prog (append `(',f) `(,prog)))
            (setq prog (append '(with-eval-after-load) prog))))
        prog))
    (t
      `(with-eval-after-load ,feature ,@body))))

(defmacro after!! (feature &rest body)
  "Evaluate BODY after FEATURE and an eponymous mode it provides.
Wrapper for `after!' with an additional check for a mode named after FEATURE."
  (declare (indent defun))
  (let ((mode-sym (intern (format "%s-mode" (symbol-name (ceamx-unquote feature))))))
    `(after! ,feature
       (when (fboundp ',mode-sym)
         ,@body))))

;; via <https://github.com/doomemacs/doomemacs/blob/03d692f129633e3bf0bd100d91b3ebf3f77db6d1/lisp/doom-lib.el#L686-L701>
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

;;; Variables

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

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

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

;;
;;; Filesystem

(defun ceamx-subdirs (parent-dir)
  "Return every non-hidden subdirectory of PARENT-DIR."
  (cl-remove-if-not
   #'file-directory-p
   (directory-files
    (expand-file-name parent-dir) t "^[^\\.]")))

;; via <https://github.com/noctuid/dotfiles/blob/434ddb77c4b40f4b7ab2246cc2254aa4f408b16f/emacs/.emacs.d/awaken.org>
(cl-defun ceamx-basename (&optional (file (buffer-file-name)))
  "Return the basename of FILE."
  (file-name-sans-extension (file-name-nondirectory file)))

(defmacro file! ()
  "Return the path of the file this macro was called."
  (or
   ;; REVIEW: Use `macroexp-file-name' once 27 support is dropped.
   (let ((file (car (last current-load-list))))
     (if (stringp file) file))
   (bound-and-true-p byte-compile-current-file)
   load-file-name
   buffer-file-name                     ; for `eval'
   (error "file!: cannot deduce the current file path")))

(defmacro dir! ()
  "Return the directory of the file this macro was called."
   (let (file-name-handler-alist)
     (file-name-directory (macroexpand '(file!)))))

(defmacro subdirs! (parent-dir)
  "Return non-hidden subdirectories of PARENT-DIR.
Simple wrapper around `ceamx-subdirs'."
  `(ceamx-subdirs ,parent-dir))

;;
;;; Advice

(defmacro def-advice! (name arglist how symbol docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST, DOCSTRING, and BODY are as in `defun'.

HOW and SYMBOL are as in `advice-add'. HOW describes how to add
the newly-defined advice. SYMBOL is the function to be advised."
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

;;; Hooks

(defmacro def-hook! (name arglist hooks docstring &rest body)
  "Define function NAME and add it to HOOKS.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
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
         (add-hook hook ',name)))))

;; FIXME: accept unquoted args
(defmacro hook! (hooks func)
  "Add function FUNC to one or more HOOKS.
HOOKS may be either a single quoted hook symbol, or a list of
multiple hook symbols.

FUNC may be any quoted function symbol."
  (declare (indent defun))
  (setq func (ceamx-unquote func))
  (cl-assert (fboundp func) t)
  `(dolist (hook (ensure-list ,hooks))
     (cl-assert (boundp hook) t)
     (add-hook hook ',func)))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (obsolete "def-hook!" "2023-11-10")
    (indent (lambda (indent-point state)
              (goto-char indent-point)
              (when (looking-at-p "\\s-*(")
                (lisp-indent-defform state indent-point))))
    (debug t))
  ;; TODO: use `derived-mode-hook-name'
  (let* ((hook-forms (ceamx--resolve-hook-forms hooks))
          (func-forms ())
          (defn-forms ())
          append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
              (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                      next)
                ((eq first 'quote)
                  (let ((quoted (cadr next)))
                    (if (atom quoted)
                      next
                      (when (cdr quoted)
                        (setq rest (cons (list first (cdr quoted)) rest)))
                      (list first (car quoted)))))
                ((memq first '(defun cl-defun))
                  (push next defn-forms)
                  (list 'function (cadr next)))
                ((prog1 `(lambda (&rest _) ,@(cons next rest))
                   (setq rest nil))))
          func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
              `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (ceamx--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (ceamx--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;;; Packages

(defmacro use-feature! (name &rest args)
  "Configuration-only wrapper for `use-package', passing through NAME and ARGS.

This macro is a wrapper for `use-package' disabling package
installation by setting package installation keywords to nil. The
supported keywords are `:ensure' (for package.el and recent
versions of `elpaca'), `:straight', `:quelpa', and `:elpaca' (for
older versions of `elpaca').

As an example, if `use-package-always-ensure' is non-nil, its
effect will be ignored in the `use-package' macro expansion
because `:ensure' will be nil.

For further information, refer to the `use-package' documentation
or the documentation for the respective package manager."
  (declare (indent defun))
  (let (
         ;; FIXME: check if :ensure keyworr compat available

         ;;(quelpa-keyword-maybe (when (fboundp 'use-package-handler/:quelpa) '(:quelpa nil)))
         ;;(straight-keyword-maybe (when (fboundp 'use-package-handler/:straight) '(:straight nil)))

         ;; (elpaca-keyword-maybe (when (fboundp 'use-package-handler/:elpaca) '(:elpaca nil)))
         )
    `(use-package ,name
       :ensure nil
       ;; ,@elpaca-keyword-maybe
       ;;,@quelpa-keyword-maybe
       ;; ,@straight-keyword-maybe
       ,@args)))

;;; Debugging

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

(provide 'lib-common)
;;; lib-common.el ends here
