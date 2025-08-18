;;; core/ceamx-lib.el --- Ceamx common library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2023-2024  Chris Montgomery <chmont@protonmail.com>
;; Copyright (C) 2014-2023  Henrik Lissner
;; Copyright (C) 2006-2021  Steve Purcell
;; Copyright (C) 2016–2024  Radian LLC and contributors
;; Copyright (C) 2018  Adam Porter
;; Copyright (C) 2013-2021  Bailey Ling <bling@live.ca>
;; Copyright (C) 2013-2023  7696122 <7696122@gmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;;         Henrik Lissner
;;         Steve Purcell
;;         Radon Rosborough <radon@intuitiveexplanations.com>
;;         Adam Porter <adam@alphapapa.net>
;;         Bailey Ling <bling@live.ca>
;;         7696122 <7696122@gmail.com>

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

;; This library is a requirement of just about every customization and library
;; file.  It is loaded early during initialization before packages are loaded.
;; Avoid any library dependencies which are not already part of Emacs.

;;;; Sources

;; <https://github.com/doomemacs/doomemacs/blob/03d692f129633e3bf0bd100d91b3ebf3f77db6d1/lisp/doom-lib.el>
;; <https://github.com/radian-software/radian/blob/9a82b6e7395b3f1f143b91f8fe129adf4ef31dc7/emacs/radian.el>
;; <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/doom-keybinds.el#L93C1-L109C56>

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'seq)

(require 'ceamx-keymaps)

;;;; Variables

;; via <https://github.com/protesilaos/dotfiles/blob/df9834d8db815920bfd7aacfaf11ef16fa089c53/emacs/.emacs.d/prot-lisp/ceamx.el>
(defconst ceamx-url-regexp
  (concat
   "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
   "[.@]"
   "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
  "Regular expression to match (most?) URLs or email addresses.")


;;;; Commands


;;;; Functions

;;;;; Environment Context

(defun ceamx-host-p (name)
  "Whether Emacs is running on the machine NAME."
  (string= name (system-name)))

(defun ceamx-host-macos-p ()
  "Whether the current host system is macOS."
  (or (memq window-system '(mac ns))
      (eq system-type 'darwin)))

(defun ceamx-host-gnu-linux-p ()
  "Whether the current host system is GNU/Linux."
  (eq system-type 'gnu/linux))

(defun ceamx-host-nixos-p ()
  "Whether the operating system is NixOS."
  (string-match-p "NixOS" (shell-command-to-string "uname -v")))

(defun ceamx-host-wsl-p ()
  "Whether Emacs is currently running in WSL."
  (and (eq system-type 'gnu/linux)
       (or (getenv "WSLENV")
           (getenv "WSL_DISTRO_NAME"))))

(when (ceamx-host-wsl-p)
  (defun ceamx-wsl/copy-selected-text (start end)
    "In WSL, copy text region with START and END to the host clipboard."
    (interactive "r")
    (when (use-region-p)
      (let ((text (buffer-substring-no-properties start end)))
        (shell-command (concat "echo '" text "' | clip.exe"))))))

;;;;; Emacs

(defun ceamx-emacs-nix-build-date ()
  "Return the Nix build date for this version of Emacs."
  (string-match "--prefix.*emacs.*\\([[:digit:]]\\{8\\}\\)" system-configuration-options)
  (string-to-number (match-string 1 system-configuration-options)))

;;;;; Filesystem

(defun ceamx-subdirs (parent-dir)
  "Return every non-hidden subdirectory of PARENT-DIR."
  (cl-remove-if-not
   #'file-directory-p
   (directory-files
    (expand-file-name parent-dir) t "^[^\\.]")))

;;;;; String Manipulation

(defun ceamx-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun ceamx-string-first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun ceamx-string-truncate-middle (string length)
  "If STRING is longer than LENGTH, return a version truncated in the middle.
When truncating, \"...\" is always inserting in the middle of the
string, so the resulting string may be longer than the original if
LENGTH is 3 or smaller."
  (declare (pure t) (side-effect-free t))
  (let ((half (floor ceamx-modeline-string-truncate-length 2)))
    (concat (substring str 0 half) "..." (substring str (- half)))))

(defun ceamx-string-truncate-right (string length)
  "If STRING is longer than LENGTH, return a truncated version.
When truncating, \"...\" is always appended to the string, so
the resulting string may be longer than the original if LENGTH is
3 or smaller.

This function is based on `string-truncate-left' (which see) from the
`subr-x' library."
  (declare (pure t) (side-effect-free t))
  (let ((strlen (length string)))
    (if (<= strlen length)
        string
      (setq length (max 0 (- length 3)))
      (concat
       (substring string (min (1- strlen)
                              (max 0 (- strlen length))))
       "..."))))

;;;;; Keybindings & Keymaps

;; via <oantolin>: <https://old.reddit.com/r/emacs/comments/1adwnse/repeatmode_is_awesome_share_you_useful_configs/kk9vpif/>
(defun ceamx-repeatify-keymap (repeat-map)
  "Set the `repeat-map' property on all commands bound in REPEAT-MAP."
  (named-let process ((keymap (symbol-value repeat-map)))
    (map-keymap
     (lambda (_key cmd)
       (cond
        ((symbolp cmd) (put cmd 'repeat-map repeat-map))
        ((keymapp cmd) (process cmd))))
     keymap)))

;;;;; Window

;; via prot-emacs
;;;###autoload
(defun ceamx-window-bounds ()
  "Return start and end points in the window as a cons cell."
  (cons (window-start) (window-end)))

;; via prot-emacs
;;;###autoload
(defun ceamx-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold', respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

;; via prot-emacs
;;;###autoload
(defun ceamx-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;;;###autoload
(defun ceamx-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

;;;;; Buffer

;; via prot-emacs
;;;###autoload
(defun ceamx-page-p ()
  "Return non-nil if there is a `page-delimiter' in the buffer."
  (or (save-excursion (re-search-forward page-delimiter nil t))
      (save-excursion (re-search-backward page-delimiter nil t))))

;; via prot-emacs
;;;###autoload
(defun ceamx-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;;;;; Miscellaneous

;; via <https://github.com/radian-software/radian/blob/31b28372df6c24b82a53ba9c89140b4888c16f88/emacs/radian.el#L641C1-L654C38>
(defun ceamx--remove-sharp-quotes (form)
  "Remove sharp quotes in all sub-forms of FORM."
  (pcase form
    (`(function ,x) (ceamx--remove-sharp-quotes x))
    (`(,x . ,y) (cons (ceamx--remove-sharp-quotes x)
                      (ceamx--remove-sharp-quotes y)))
    ((pred vectorp)
     (apply #'vector (mapcar #'ceamx--remove-sharp-quotes form)))
    (x x)))

;;;; Macros

(defmacro noop! (&rest _body)
  "Do nothing with BODY and return nil.
Unlike `ignore', produce no side effects."
  (declare (indent defun))
  nil)

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be
used instead of `setopt'.  Unlike `setq', this triggers custom setters
on variables.  Unlike `setopt', this won't needlessly pull in
dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                              ',var ,val))))

;; via <https://github.com/doomemacs/doomemacs/blob/bbadabda511027e515f02ccd7b70291ed03d8945/lisp/doom-lib.el#L628C1-L673C1>
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

;; via <https://github.com/bling/dotemacs/blob/97c72c8425c5fb40ca328d1a711822ce0a0cfa26/core/core-boot.el#L83C1-L88C25>
(defmacro defer! (secs &rest body)
  "Run BODY when Emacs is idle for SECS seconds."
  (declare (indent defun) (debug t))
  `(run-with-idle-timer
    ,secs
    nil
    (lambda () ,@body)))

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

(defmacro abbrevs! (table &rest defs)
  "Expand `abbrev' definitions DEFS for the given TABLE.
DEFS is a sequence of any of the following:

  - Pair of strings mapping ABBREV to its literal EXPANSION

  - Pair of string to symbol mapping ABBREV to a function EXPANSION
    returning the expanded string

\(fn TABLE &rest [ABBREV EXPANSION]...)"
  (declare (indent 1))
  (unless (zerop (% (length defs) 2))
    (error "Uneven number of abbrev/expansion pairs: %s" defs))
  `(if (abbrev-table-p ,table)
       (progn
         ,@(mapcar
            (lambda (pair)
              (let ((abbrev (nth 0 pair))
                    (expansion (nth 1 pair)))
               (if (stringp expansion)
                   `(define-abbrev ,table ,abbrev ,expansion)
                 `(define-abbrev ,table ,abbrev "" ,expansion))))
            (seq-split defs 2)))
     (error "%s is not an abbrev table" ,table)))

(defmacro use-feature! (name &rest args)
  "Configure feature NAME with `use-package' without package installation.
ARGS are as in `use-package', which see.

If `use-package-always-ensure' is non-nil, its effect will be ignored
here because `:ensure' will be nil."
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

;;;; Footer

(provide 'ceamx-lib)
;;; ceamx-lib.el ends here
