;;; lib-keys.el --- Keybinding helpers               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;;; Commentary:

;; FIXME: add copyright+license

;; Helper functions and macros for keybindings.

;; <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/doom-keybinds.el#L93C1-L109C56>

;;; Code:

;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar cmx-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `cmx/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun cmx/escape (&optional interactive)
  "Run `cmx-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'cmx-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(defconst cmx-namespace "cmx")

;; FIXME: doesn't work
;; reference: <https://github.com/abo-abo/hydra/blob/317e1de33086637579a7aeb60f77ed0405bf359b/hydra.el#L1265>
;; (defmacro def-leader-arm! (name key &rest defs)
;;   "Define a new leader arm NAME under KEY whose keymap contains DEFS."
;;   (declare (indent defun))
;;   (let* ((prefix (format "%s-%S" cmx-namespace name))
;;          (keymap-name (intern (format "%s-keymap" prefix)))
;;          (arm-name (intern (format "%s-leader" prefix)))
;;          (menu-name (format "%S..." name)))
;;     `(progn
;;        ;; (set (defvar ,arm-name
;;        ;;        nil
;;        ;;        "Something")
;;        ;;      ,(define-prefix-command ,arm-name ,keymap-name ,menu-name))
;;        (define-keymap :keymap ,keymap-name ,@defs)
;;        (keymap-set cmx-leader-keymap ,key ,arm-name))))
;; usage:
;; (def-leader-arm! foo-arm "x"
;;   "a" #'view-hello-file
;;   "b" #'helpful-key)

;; works.
;; TODO: docstring
(defmacro def-arm! (sym key description &rest defs)
  (declare (indent defun))
  `(progn
     (define-prefix-command (quote ,sym))
     (define-keymap
       :keymap ,sym
       ,@defs)
     (keymap-set cmx-leader-keymap ,key '(,description . ,sym))))

;; TODO: define in config
(defvar cmx-mode-specific-arm-key "m")

(require 'cl-lib)

(cl-defmacro def-mode-arm! (mode description &rest defs)
  "Define the mode-specific leader arm for MODE with DESCRIPTION and bindings DEFS."
  (declare (doc-string 2)
           (indent defun))
  (progn
    (let* ((mode-name (symbol-name mode))
           (keymap-sym (intern (format "cmx-%s-specific-map" mode-name)))
           (description description)
           (defs (or defs nil)))
      `(def-arm! ,keymap-sym "m"
         ,description
         ,@defs))))

(def-mode-arm! emacs-lisp-mode "Something"
  "v" #'zone)


;; (defvar cmx-emacs-lisp-mode-map nil)
;; (def-mode-arm! emacs-lisp-mode
;;   "e" '("eeeevvvv" . eval-defun))



(provide 'lib-keys)
;;; lib-keys.el ends here
