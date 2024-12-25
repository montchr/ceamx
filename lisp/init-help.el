;;; init-help.el --- Help -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
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

;;; Commentary:

;; "It looks like you're writing an Emacs.  Would you like help?"

;;; Code:

(require 'ceamx-lib)
(require 'lib-help)

;;; Configure window behavior for help buffers

;; Focus newly-opened help windows.
(setopt help-window-select t)

;; Also focus newly-opened manpages, which still do not follow `display-buffer'
;; rules (as of <2024-03-06>).
(setopt Man-notify-method 'aggressive)

;;; Bind commands to call ~consult-info~ filtered by commonly-used manual collections

(declare-function consult-info "consult-info")

;; Remove the default binding for the `describe-input-method' command.
(keymap-global-unset "C-h I" t)

(define-keymap :keymap (current-global-map)
  "C-h i"    #'ceamx/consult-info-dwim
  "C-h I c"  #'ceamx/completion-info
  "C-h I e"  #'ceamx/emacs-info
  "C-h I i"  #'consult-info
  "C-h I o"  #'ceamx/org-info)

;;; Peruse local ~devdocs~ docsets corresponding to the current major-mode

;; <https://github.com/astoff/devdocs.el>

;; NOTE: Must run ~devdocs-install~ before a docset is available for reference.
;;
;; TODO: Install docsets automatically.
;;       See ~lib-help~ for WIP.

(package! devdocs
  (define-keymap :keymap help-map
    ;; Replace default `apropos-documentation' binding.
    "d" #'devdocs-lookup
    "D" #'apropos-documentation)

  ;; FIXME: on a stale timer! every week! not every session...
  (devdocs-update-all))

(after! popper
  (add-to-list 'popper-reference-buffers "\\*devdocs\\*"))

;;; Display keyboard macros or latest interactive commands as Elisp via ~elmacro~

;; <https://github.com/Silex/elmacro>

;; Avoid enabling this mode globally. It may cause some recurring errors, and
;; the package has not been updated in years. By nature, it is also quite
;; invasive, and should probably only be used as a development tool as needed.

(package! elmacro
  (setopt elmacro-show-last-commands-default 30)

  ;; <https://github.com/Silex/elmacro/blob/master/README.md#org-mode-smartparens-etc>
  ;; <https://github.com/Silex/elmacro/blob/master/README.md#elmacro-processor-prettify-inserts>
  (setopt elmacro-processor-prettify-inserts
          (unless (or (bound-and-true-p lispy-mode) ; not actually sure about lispy-mode
                      (bound-and-true-p smartparens-mode)
                      (bound-and-true-p org-mode))))

  ;; "a" "b" "c" => "abc"
  ;; FIXME: maybe causes errors?
  (setopt elmacro-processor-concatenate-inserts t))

;;; Provide improved alternatives to the builtin `describe-*' utilities with ~helpful~

;; <https://github.com/Wilfred/helpful>

;; NOTE: there are some blocking bugs that have gone unfixed for quite a while
;;        some symbols' helpful pages cannot be displayed.
;;        <https://github.com/Wilfred/helpful/issues/329>

(package! helpful
  ;; Avoid a first-time lag when asking for help, which often happens before an
  ;; idle timer has the chance to run.
  (require 'helpful)
  (define-keymap :keymap help-map
    "c" #'helpful-callable
    "C" #'helpful-command
    "f" #'helpful-function              ; orig: `describe-face'
    "h" #'helpful-at-point
    ;; TODO: consider swapping with the original as a trial?
    "k" #'helpful-key                   ; orig: `describe-key-briefly'
    "o" #'helpful-symbol
    "v" #'helpful-variable

    ;; Parity with the corresponding unmodded keys.
    ;; Primarily for Meow keypad, but also sometimes feels more natural to keep
    ;; holding Ctrl anyway.
    "C-k" #'helpful-key
    "C-o" #'helpful-symbol

    ;; Rebind the originals
    "F" #'describe-face
    "K" #'describe-key-briefly

    ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
    "C-h" nil))

;;; Eldoc: Display multiple composed messages

(setopt eldoc-documentation-function #'eldoc-documentation-compose)

;;; Display usage examples for Elisp callables inside their help buffers

;; <https://github.com/xuchunyang/elisp-demos>

(package! elisp-demos
  (after! helpful
    (require 'elisp-demos)
    (setopt elisp-demos-user-files (list (expand-file-name  "docs/elisp-demos.org" user-emacs-directory)))
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;; DISABLED Improve display of hints in an active ~repeat-mode~ keymap

;; <https://github.com/karthink/repeat-help>

;; Essential reading for context and usage ideas:
;; <https://karthinks.com/software/it-bears-repeating/>

;; DISABLED:
;; - Hides default help, which I generally do want to see.
;; - No straightforward way to disable or customize per-map

(package! repeat-help
  (setopt repeat-help-auto nil)
  (setopt repeat-help-key (kbd "?"))

  ;; If `repeat-help' detects `which-key' because `embark' has not yet loaded,
  ;; then its default popup type will be `which-key', not `embark'. This does
  ;; not quite line up with the intended behavior as stated in the `repeat-help'
  ;; README.
  ;; Note, also, that Embark will not be loaded until one of its autoloaded
  ;; commands are invoked.
  ;; <https://github.com/karthink/repeat-help/issues/10>
  (with-eval-after-load 'embark
    (setopt repeat-help-popup-type 'embark))

  ;; (require 'repeat-help)
  ;; (add-hook 'repeat-mode-hook #'repeat-help-mode)
  )

;;; Provide "Casual" transient menus for complex modes

(package! casual-suite
  (require 'casual-suite)

  (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)

  ;; <https://github.com/kickingvegas/casual-avy>
  ;; M-g M-g
  (keymap-set goto-map "M-g" #'casual-avy-tmenu)

  ;; <https://github.com/kickingvegas/casual-calc>
  (after! calc
    (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))
  (after! calc-alg
    (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu))

  ;; <https://github.com/kickingvegas/casual-dired>
  (after! dired
    (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu))

  ;; <https://github.com/kickingvegas/casual-info>
  (after! info
    (keymap-set Info-mode-map "C-o" #'casual-info-tmenu))

  ;; <https://github.com/kickingvegas/casual-isearch>
  (after! isearch
    (keymap-set isearch-mode-map "<f2>" #'casual-isearch-tmenu))

  (after! ibuffer
    (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu))

  (after! re-builder
    (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
    (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu))

  (after! bookmark
    (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu))

  (after! org-agenda
    (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)))

;;; `Info-mode' enchantments

(add-hook 'Info-mode-hook #'hl-line-mode)
(add-hook 'Info-mode-hook #'scroll-lock-mode)

;;; Keybindings

(define-keymap :keymap help-map
  "l" #'find-library
  ;; I actually prefer the default `man' over `consult-man'.
  "m" #'man                     ; orig: `describe-mode'
  "M" #'describe-mode

  ;; FIXME: no lambda binding
  ;; "t" `("text-props (pt)" . ,(cmd!!
  ;;                              #'describe-text-properties
  ;;                              current-prefix-arg
  ;;                              (point)))

  ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
  "C-h" nil)

(provide 'init-help)
;;; init-help.el ends here
