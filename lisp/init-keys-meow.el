;;; init-keys-meow.el --- Meow support               -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;; Requirements


;; [[file:../config.org::*Requirements][Requirements:1]]
(require 'lib-common)
(require 'lib-keys-meow)
;; Requirements:1 ends here

;; Install and load Meow


;; [[file:../config.org::*Install and load Meow][Install and load Meow:1]]
(package! meow)
;; Install and load Meow:1 ends here

;; Load meow after Which-Key to circumvent Meow-Keypad UI issues

;; Meow's leader functionality has a custom keybinding visualization helper that
;; looks almost exactly like =which-key= except that it is very buggy. It does try
;; to smooth over some of the guts of Meow, as =which-key= is cluttered with
;; keybinding entries for =meow-digit-argument=. But even Meow's maintainers do not
;; recommend using the custom keypad helper UI.

;; Unfortunately, AFAIK there is no way to disable the UI feature explicitly
;; without disabling the Keypad entirely. The only way to prevent this feature from
;; causing workflow issues is by installing =which-key= and letting Meow defer to
;; =which-key= for this kind of interface.

;; And so, we load Meow after Which-Key:


;; [[file:../config.org::*Load meow after Which-Key to circumvent Meow-Keypad UI issues][Load meow after Which-Key to circumvent Meow-Keypad UI issues:1]]
(with-eval-after-load 'which-key
  (require 'meow))
;; Load meow after Which-Key to circumvent Meow-Keypad UI issues:1 ends here



;; And allow Elpaca to finish processing its queues, since everything afterwards depends on Meow.


;; [[file:../config.org::*Load meow after Which-Key to circumvent Meow-Keypad UI issues][Load meow after Which-Key to circumvent Meow-Keypad UI issues:2]]
(elpaca-wait)
;; Load meow after Which-Key to circumvent Meow-Keypad UI issues:2 ends here

;; Add initial keybindings to the Meow Leader keymap


;; [[file:../config.org::*Add initial keybindings to the Meow Leader keymap][Add initial keybindings to the Meow Leader keymap:1]]
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
 '("?" . meow-cheatsheet))

(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))
;; Add initial keybindings to the Meow Leader keymap:1 ends here

;; Add initial keybindings to the NORMAL state keymap


;; [[file:../config.org::*Add initial keybindings to the NORMAL state keymap][Add initial keybindings to the NORMAL state keymap:1]]
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
 '("c" . meow-change-save)            ; default: `meow-change'
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
 ;; FIXME: duplicated with "X" binding
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("s" . meow-kill)
 '("t" . meow-till)
 '("T" . meow-till-expand)            ; custom addition
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 ;; FIXME: duplicated with "Q" binding
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 ;; There's no documentation, but this will essentially return to the
 ;; original position prior to beginning the selection.
 '("z" . meow-pop-selection)
 ;; TODO: no idea what the difference is at a glance, no docs
 ;; '("Z" . meow-pop-all-selection)    ; custom addition
 '("'" . repeat)
 '("<escape>" . ignore)
 '(":" . avy-goto-char-2))
;; Add initial keybindings to the NORMAL state keymap:1 ends here

;; Tell Meow about our keyboard layout to arrange its cheatsheet


;; [[file:../config.org::*Tell Meow about our keyboard layout to arrange its cheatsheet][Tell Meow about our keyboard layout to arrange its cheatsheet:1]]
;; NOTE: This is not a customizable variable, although it is required for meow.
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;; Tell Meow about our keyboard layout to arrange its cheatsheet:1 ends here

;; Configure the Keypad


;; [[file:../config.org::*Configure the Keypad][Configure the Keypad:1]]
;; Don't pass through keys that aren't in keypad.
(setopt meow-keypad-self-insert-undefined nil)
;; Configure the Keypad:1 ends here

;; Avoid the default binding for =meow-keypad= in motion state

;; The goal here is to preserve the established SPC/DEL keybindings to scroll in Info mode and some other similar modes.


;; [[file:../config.org::*Avoid the default binding for =meow-keypad= in motion state][Avoid the default binding for =meow-keypad= in motion state:1]]
(keymap-unset meow-motion-state-keymap "SPC" t)
;; Avoid the default binding for =meow-keypad= in motion state:1 ends here

;; ACTIVE Leader keymap from =meow-keymap-alist= (default)

;; When =meow-keypad-leader-dispatch= is nil, the leader will dispatch to the
;; leader keymap in =meow-keymap-alist=.

;; This is the default behavior.

;; | Pros                                               | Cons                                              |
;; |----------------------------------------------------+---------------------------------------------------|
;; | Filters out noise                                  | Requires additional keystroke =c= to get to =C-c= |
;; | With =c= press, Ctrl mod is primed (i.e. =C-c C-=) |                                                   |


;; [[file:../config.org::*ACTIVE Leader keymap from =meow-keymap-alist= (default)][ACTIVE Leader keymap from =meow-keymap-alist= (default):1]]
(setopt meow-keypad-leader-dispatch nil)
;; ACTIVE Leader keymap from =meow-keymap-alist= (default):1 ends here

;; Customize the appearance of the mode-line indicator for the current Meow state

;; This should happen before defining any new states so that they may provide their
;; own values upon definition.


;; [[file:../config.org::*Customize the appearance of the mode-line indicator for the current Meow state][Customize the appearance of the mode-line indicator for the current Meow state:1]]
(setopt meow-replace-state-name-list
        '( (normal . "🅝")
           (beacon . "🅑")
           (insert . "🅘")
           (motion . "🅜")
           (keypad . "🅚")))
;; Customize the appearance of the mode-line indicator for the current Meow state:1 ends here

;; Map preferred initial Meow states for some additional major-modes


;; [[file:../config.org::*Map preferred initial Meow states for some additional major-modes][Map preferred initial Meow states for some additional major-modes:1]]
(pushnew! meow-mode-state-list
          ;; shells
          ;; TODO: use `ceamx-repl-modes-list'
          '(comint-mode . insert)
          '(eat-mode . insert)
          '(eshell-mode . insert)

          ;; writing
          '(diary-mode . normal)

          ;; read-only
          ;; TODO: how to lock state? i.e. dont allow switching
          ;; TODO: set for all read-only buffers?
          '(Info-mode . motion)
          '(read-only-mode . motion)
          '(help-mode . motion))
;; Map preferred initial Meow states for some additional major-modes:1 ends here

;; Improve Meow integration with the default Emacs kill-ring and system clipboard

;; [[https://github.com/meow-edit/meow/issues/543][Clipboard Confusing Defaults and Documentation · Issue #543 · meow-edit/meow · GitHub]]


;; [[file:../config.org::*Improve Meow integration with the default Emacs kill-ring and system clipboard][Improve Meow integration with the default Emacs kill-ring and system clipboard:1]]
(setopt meow-use-clipboard t)
;; Improve Meow integration with the default Emacs kill-ring and system clipboard:1 ends here

;; Expand upon the default Meow pair-things

;; See the helper functions/macros.


;; [[file:../config.org::*Expand upon the default Meow pair-things][Expand upon the default Meow pair-things:1]]
(meow-pair! 'angle "a" "<" ">")

(ceamx-meow-bind-thing 'round "(")
(ceamx-meow-bind-thing 'round ")")
(ceamx-meow-bind-thing 'curly "{")
(ceamx-meow-bind-thing 'curly "}")

;; TODO: i don't really thing i want to do this, but here for reference
;; (ceamx-meow-unbind-thing "r")
;; Expand upon the default Meow pair-things:1 ends here

;; Activate Meow


;; [[file:../config.org::*Activate Meow][Activate Meow:1]]
(meow-global-mode 1)
;; Activate Meow:1 ends here

(provide 'init-keys-meow)
;;; init-keys-meow.el ends here
