;;; init-help.el --- Help -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;; "It looks like you're writing an Emacs. Would you like help?"

;;; Code:

(require 'lib-common)
(require 'lib-help)
(require 'lib-keys)

;;; Bind commands to call `consult-info' filtered by commonly-used manual collections

;; Remove the default binding for the `describe-input-method' command.
(keymap-global-unset "C-h I" t)

(global-keys!
  "C-h I c"  #'ceamx/completion-info
  "C-h I e"  #'ceamx/emacs-info
  "C-h I o"  #'ceamx/org-info)

;;
;;; Packages

;;;; `devdocs' :: <https://github.com/astoff/devdocs.el>

;; NOTE: Must run `devdocs-install' before use.

;; TODO: Install devdocs automatically.
;;       See `lib-help' for progress.

(use-package devdocs
  :commands (devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)

  :config
  (define-keymap :keymap help-map
    ;; Replace default `apropos-documentation' binding.
    "d" #'devdocs-lookup
    "D" #'apropos-documentation)

  (def-hook! +devdocs-install-nix-docs ()
    '(nix-mode-hook nix-ts-mode-hook)
    "Install `devdocs' documents for the Nix language."
    (ceamx/devdocs-ensure-doc "nix"))

  (devdocs-update-all))

;;;; elmacro :: <https://github.com/Silex/elmacro>

;; Display keyboard macros or latest interactive commands as Elisp.

;; Avoid enabling this mode globally. It may cause some recurring errors, and
;; the package has not been updated in years. By nature, it is also quite
;; invasive, and should probably only be used as a development tool as needed.

;; FIXME: Its user options do not seem to be available immediately?

(use-package elmacro
  :commands (elmacro-mode)
  :defines (elmacro-show-last-commands-default elmacro-processor-prettify-inserts elmacro-processor-concatenate-inserts)

  :config
  (setopt elmacro-show-last-commands-default 30)

  ;; <https://github.com/Silex/elmacro/blob/master/README.md#org-mode-smartparens-etc>
  ;; <https://github.com/Silex/elmacro/blob/master/README.md#elmacro-processor-prettify-inserts>
  (setopt elmacro-processor-prettify-inserts (unless (or (bound-and-true-p lispy-mode) ; not actually sure about lispy-mode
                                                       (bound-and-true-p smartparens-mode)
                                                       (bound-and-true-p org-mode))))

  ;; "a" "b" "c" => "abc"
  ;; FIXME: maybe causes errors?
  (setopt elmacro-processor-concatenate-inserts t))

;;;; `helpful' :: <https://github.com/Wilfred/helpful>

;; NOTE: there are some blocking bugs that have gone unfixed for quite a while
;;        some symbols' helpful pages cannot be displayed.
;;        <https://github.com/Wilfred/helpful/issues/329>

(use-package helpful
  ;; Avoid a first-time lag when asking for help, which often happens before an
  ;; idle timer has the chance to run.
  :demand t
  :commands ( helpful-at-point helpful-command helpful-callable
              helpful-key helpful-symbol helpful-variable))

(use-feature! eldoc
  :config
  ;; via <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L2800-L2810>
  (def-advice! +eldoc-better-display-message-p-a (&rest _)
    :override #'eldoc--message-command-p
    "Make ElDoc smarter about when to display its messages.
From the original author:

\"By default ElDoc has a customizable whitelist of commands that
it will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't.\""
    (member (current-message) (list nil eldoc-last-message))))

;;;; `elisp-demos' :: <https://github.com/xuchunyang/elisp-demos>

;;  Display usage examples inside help buffers for Emacs Lisp callables.

(use-package elisp-demos
  :after (helpful)
  :autoload (elisp-demos-advice-helpful-update)
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  :config
  (setopt elisp-demos-user-files (list (expand-file-name  "docs/elisp-demos.org" user-emacs-directory))))

(provide 'init-help)
;;; init-help.el ends here
