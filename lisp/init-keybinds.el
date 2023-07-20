;;; init-keybinds.el --- Keybindings -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;; Commentary:

;;  Keybindings configuration
;;
;; <https://github.com/noctuid/general.el#usage-recommendations-and-documentation-clarifications>

;;; Code:

(require 'lib-keybinds)

(defvar cmx-hydra-key "<f12>"
  "Key to bind `cmx-hydra/main/body' to.")

(defvar cmx-leader-key "SPC")
(defvar cmx-leader-alt-key "M-SPC")

(global-set-key (kbd cmx-hydra-key) 'cmx-hydra/main/body)


;; macOS: Remap modifier keys.
(when (and +sys-mac-p +graphical-p)
  (setq mac-control-modifier 'control
        mac-option-modifier 'meta
        ns-option-modifier 'meta
        mac-command-modifier 'super
        ns-command-modifier 'super
        ;; Free up the right-side option key for character composition.
        mac-right-option-modifier 'none
        ns-right-option-modifier 'none)
  ;; Common system clipboard hotkeys.
  (global-set-key [(s c)] 'kill-ring-save)
  (global-set-key [(s v)] 'yank)
  (global-set-key [(s x)] 'kill-region)
  (global-set-key [(s q)] 'kill-emacs))

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a
;;   byproduct of its history in the terminal, which can't distinguish them
;;   either, however, when GUIs came about Emacs greated separate input events
;;   for more contentious keys like TAB and RET. Therefore [return] != RET,
;;   [tab] != TAB, and [backspace] != DEL.
;;
;;   In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;;   Otherwise, it falls back to regular C-i keybinds.
;; 
;; Source: <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L47-L67>
(define-key key-translation-map [?\C-i]
            (cmd! (if (let ((keys (this-single-command-raw-keys)))
                        (and keys
                             (not (cl-position 'tab    keys))
                             (not (cl-position 'kp-tab keys))
                             (display-graphic-p)
                             ;; Fall back if no <C-i> keybind can be found, otherwise
                             ;; we've broken all pre-existing C-i keybinds.
                             (let ((key
                                    (cmx/lookup-key
                                     (vconcat (cl-subseq keys 0 -1) [C-i]))))
                               (not (or (numberp key) (null key))))))
                      [C-i] [?\C-i])))

;;
;;; Universal, non-nuclear escape
;;  Source: <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L70-L113>

;; `keyboard-quit' is too much of a nuclear option.
;; The following defines a ESC/C-g DWIM alternative
;; It serves four purposes (in order):
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
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'cmx/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'cmx/escape))


;;
;;; === GENERAL.EL =============================================================
;;  


;;; TODO: move vars to top of file



;;; --- load ---

(use-package general :demand t)
(elpaca-wait) 

;; Create alias macros corresponding to vimisms.
;; <https://github.com/noctuid/general.el/blob/feature/general-config-and-speed-documentation/README.org#vim-like-definers>
;; TODO: consider removal, i don't need this, but ensure it's unused
(general-evil-setup)


;;; --- aliases / macros ---

;; FIXME: remove these temporary safeguards against copypasta
(defalias 'gsetq #'general-setq)
(defalias 'gsetq-local #'general-setq-local)
(defalias 'gsetq-default #'general-setq-default)
;; TODO: maybe remove (added as copypasta safeguard)
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)

;; Prevent "X starts with non-prefix key Y" errors except at startup.
;; via doom <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L123-L124>
;; (add-hook 'doom-after-modules-init-hook #'general-auto-unbind-keys)

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `cmx-localleader-key' and `cmx-localleader-alt-key' to change the
localleader prefix."
  `(general-define-key
    :states '(normal visual motion emacs insert)
    :major-modes t
    :prefix cmx-localleader-key
    :non-normal-prefix cmx-localleader-alt-key
    ,@args))


(general-override-mode)
(general-auto-unbind-keys)



;;; -----------------------------------------------------

(defalias 'kbd! #'general-simulate-key)

(general-define-key
 :states '(normal visual motion)
 :keymaps 'override
 "SPC" 'cmx-hydra/main/body)


;;
;;; === WHICH-KEY ==============================================================
;;  

(use-package which-key
  :demand t
  :diminish which-key-mode

  :init
  (setq which-key-enable-extended-define-key t)
  (setq which-key-prefix-prefix "+")
  (setq which-key-separator " ")

  :custom
  (which-key-idle-delay 0.02)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)

  :config
  (setq which-key-sort-uppercase-first nil
        which-key-add-column-padding 1)
  (which-key-mode))

;; Wait until `which-key` is activated so its use-package keyword is installed
(elpaca-wait) 


;;
;;; === BINDINGS ===============================================================
;;  


(provide 'init-keybinds)
;;; init-keybinds.el ends here
