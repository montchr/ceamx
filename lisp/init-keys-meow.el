;;; init-keys-meow.el --- Meow support               -*- lexical-binding: t; -*-

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


;;; Code:

(require 'lib-common)

(use-package meow
  :demand t
  :commands (meow-global-mode
             meow-insert-exit)
  :autoload (meow-normal-mode
             meow-leader-define-key
             meow-motion-overwrite-define-key
             meow-normal-define-key)
  :defines (meow-cheatsheet-layout-qwerty)

  :init
  ;; TODO: necessary? i don't think so
  (require 'meow)

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("u" . meow-universal-argument)     ; custom
   )

  (meow-motion-overwrite-define-key
   ;; TODO: why does <https://github.com/chuxubank/cat-emacs/blob/65155f642b336d14ca63f010ff45eea2c18cfdce/cats/%2Bmeow.el> disable these?
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore)

   (meow-normal-define-key
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)
    '("-" . negative-argument)
    '(";" . meow-reverse)
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    '("a" . meow-append)
    '("A" . meow-open-below)
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("c" . meow-change-save)           ; default: `meow-change'
    '("d" . meow-delete)
    '("D" . meow-backward-delete)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("f" . meow-find)
    '("g" . meow-cancel-selection)
    '("G" . meow-grab)
    '("h" . meow-left)
    '("H" . meow-left-expand)
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("j" . meow-next)
    '("J" . meow-next-expand)
    '("k" . meow-prev)
    '("K" . meow-prev-expand)
    '("l" . meow-right)
    '("L" . meow-right-expand)
    '("m" . meow-join)
    '("n" . meow-search)
    '("o" . meow-block)
    '("O" . meow-to-block)
    '("p" . meow-yank)
    '("q" . meow-quit)
    '("Q" . meow-goto-line)
    '("r" . meow-replace)
    '("R" . meow-swap-grab)
    '("s" . meow-kill)
    '("t" . meow-till)
    ;; '("T" . meow-till-expand)          ; custom addition
    '("u" . meow-undo)
    '("U" . meow-undo-in-selection)
    '("v" . meow-visit)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    '("x" . meow-line)
    '("X" . meow-goto-line)
    '("y" . meow-save)
    '("Y" . meow-sync-grab)
    '("z" . meow-pop-selection)
    ;; '("Z" . meow-pop-all-selection)    ; custom addition
    '("'" . repeat)
    '("<escape>" . ignore)
    '(":" . avy-goto-char-2)))

  ;; FIXME: errors i think (`appendq!' seems broken overall, i haven't found it ever to do what i expected)
  ;; (appendq! meow-mode-state-list
  ;;   (diary-mode . normal)
  ;;   (help-mode . normal)
  ;;   (eshell-mode . insert)
  ;;   (comint-mode . insert))

  ;; NOTE: This is not a customizable variable, although it is required for meow.
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  :config

  ;; Because I'm still getting the hang of meow again. I'm not fast enough to
  ;; remember exactly what to do. Once this delay feels too long, then it can be
  ;; changed.
  (setopt meow-expand-hint-remove-delay 10.0)

  (setopt meow-keypad-leader-dispatch nil)

  ;; TODO: investigate effects -- copied from <https://github.com/chuxubank/cat-emacs/blob/65155f642b336d14ca63f010ff45eea2c18cfdce/cats/%2Bmeow.el>
  ;; (setopt meow-expand-exclude-mode-list nil)

  ;; Prevent leader binding collision with `cmx-git-map'.
  (setopt meow-keypad-ctrl-meta-prefix nil)

  ;; Improve state indicator appearance (e.g. in modeline).
  (setopt meow-replace-state-name-list '((normal . "🅝")
                                         (beacon . "🅑")
                                         (insert . "🅘")
                                         (motion . "🅜")
                                         (keypad . "🅚")))

  ;; Don't pass through keys that aren't in keypad.
  (setopt meow-keypad-self-insert-undefined nil)

  ;; Activate Meow.
  (meow-global-mode 1))


(provide 'init-keys-meow)
;;; init-keys-meow.el ends here
