;;; init-hydras.el --- Unleash the hydras -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>
;; Copyright (c) 2016-2023  John Kitchin <jkitchin@andrew.cmu.edu> and the Scimax contributors

;; Maintainer:  Chris Montgomery <chris@cdom.io>
;; Author:  Chris Montgomery <chris@cdom.io>
;;          John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Modified: 09 July 2023
;; Created: 09 July 2023
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

;;  Hydra configuration, helper packages, and hydras definitions.
;;
;;  The actual binding of hydras is handled in `init-keybinds'.

;;  Inspired by Scimax's `scimax-hydras.el'.
;;
;;  <https://github.com/jkitchin/scimax/blob/43d91a4e218af8e3d807c1138b3489355767bf24/scimax-hydra.el>
;;  
;;  From the original header:
;;
;;  > The goal of this library is to emulate Spacemacs with hydras. You can access
;;  > a lot of the commands we use a lot with just 2-3 keystrokes. The hydras are
;;  > saved in a stack as you visit them so you can go to the previous one with ,
;;  > (comma). You can get to M-x by pressing x in any of these hydras, and / to
;;  > undo. Not every command will be shorter, e.g. C-a is shorter than f12 n a,
;;  > but this shows you tips of what you can do, and doesn't require any chording.

;;  Scimax also credits the following sources:
;;
;;  - <https://ericjmritz.wordpress.com/2015/10/14/some-personal-hydras-for-gnu-emacs/>

;;; Code:

(require 'cl)


(use-package hydra
  :demand t
  :commands defhydra)
(elpaca-wait)

;; Add `:hydra' keyword to `use-package'.
(use-package use-package-hydra
  :after '(hydra))


;; 
;;; pretty-hydra :: Major mode leader key powered by Hydra.
;;  <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra> 

(use-package pretty-hydra
  :after '(hydra))


;; 
;;; major-mode-hydra :: Major mode leader key powered by Hydra.
;;  <https://github.com/jerrypnz/major-mode-hydra.el> 

(use-package major-mode-hydra
  :after '(hydra pretty-hydra)
  :bind
  ;; TODO: verify this is what we want
  ("SPC m" . major-mode-hydra))


;; 
;;; Utilities
;;

(defalias 'hydra-def! #'pretty-hydra-define)


;; 
;;; Shared Hydra Parts
;;

(defhydra scimax-base (:color blue)
  "base"
  ("," scimax-hydra-pop "back" :color blue)
  ("x" execute-extended-command "M-x")
  ("C-s" save-buffer "Save")
  ("/" undo "undo" :color red)
  ("\\" redo "redo" :color red)
  ("8" (switch-to-buffer "*scratch*") "*scratch*")
  ("?" scimax-hydra-help "Menu help")

  ;; FIXME: 'm'
  ;; ("." scimax-dispatch-mode-hydra "Major mode hydras")

  ("u" (hydra--universal-argument current-prefix-arg) "C-u" :color red)

  "`"  '("other buffer" . (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))))
  "!"  'shell-command
  ":"  'eval-expression
  "."  'repeat
  "SPC"  'project-find-file

  ("q" nil "quit"))


;; 
;;; Main Hydra
;;

(defhydra scimax ( :color blue
                   :inherit (scimax-base/heads)
                   :columns 4
                   :body-pre (scimax-hydra-reset)
                   :idle 0.5)
  "scimax"

  ;;   "/" '(nil :which-key "search...")
  ;;   "[" '(nil :which-key "previous...")
  ;;   "]" '(nil :which-key "next...")

  ("a" (scimax-open-hydra scimax-applications/body) "Applications")
  ("b" (scimax-open-hydra scimax-buffers/body) "Buffers")
  ;; TODO: 'B' for 'bookmarks'
  ;; TODO: 'c' for 'code'/lsp
  ;; d ... ?

  ;; TODO: consider 'eval'?
  ;; FIXME: why this ambiguity?
  ("e" (scimax-open-hydra scimax-errors/body) "Edit/Errors")

  ("f" (scimax-open-hydra scimax-files/body) "Files")

  ;; TODO: 'F' for 'frame'
  ;; ("F" (scimax-open-hydra scimax-frame/body) "Frame")

  ;; FIXME: git
  ;; ("g" (scimax-open-hydra scimax-google/body) "Google")

  ("h" (scimax-open-hydra scimax-help/body) "Help")
  ("i" (scimax-open-hydra scimax-insert/body) "Insert")
  ("j" (scimax-open-hydra scimax-jump/body) "Jump")
  ("k" (scimax-open-hydra scimax-bookmarks/body) "Bookmarks")
  ;; TODO: compare with doom et al
  ;;   "l" '(nil :which-key "link...")
  ;; ("l" (scimax-open-hydra scimax-lisp/body) "Lisp")

  ;; FIXME: swap with major modes
  ;; ("m" (scimax-open-hydra scimax-minor-modes/body) "Minor modes/mark")
  ;; FIXME: swap with minor modes
  ;; ("s-m" scimax-dispatch-mode-hydra "Major mode hydras")

  ;; TODO: consider...?
  ("M" (scimax-open-hydra scimax-smerge/body) "smerge")

  ;; FIXME: 'notes'? but that might be better in 'org'
  ("n" (scimax-open-hydra scimax-navigation/body) "Navigation")

  ;; TODO: consider whether to keep this somewhat-common binding.
  ;;       doom-emacs has `SPC o' mapped to "open" bindings, but many other
  ;;       folks use "applications" ('SPC a') for this, and i can't think of
  ;;       anything that would conflict except for maybe 'agenda', which would
  ;;       be better off in 'org' anyway...
  ;;   "o" '(nil :which-key "open...")
  ("o" (scimax-open-hydra scimax-org/body) "org")

  ("p" (scimax-open-hydra hydra-projectile/body) "Project")
  ;; q ... is for quitting in `scimax-base', don't reassign
  ;; r ... ?
  ("s" (scimax-open-hydra scimax-search/body) "Search")

  ;; TODO: i have this set to 'toggle' based on doom, but i like what scimax does with 'text'
  ;;   "t" '(nil :which-key "toggle...")
  ("t" (scimax-open-hydra scimax-text/body) "Text")

  ;; TODO:   "T" '(nil :which-key "tabs...")

  ;; NOTE: u is a prefix arg in `scimax-base', do not reassign

  ;; FIXME: remap to 'g' for 'git' if only for muscle memory
  ;; ("v" (scimax-open-hydra scimax-version-control/body) "Version control")

  ("w" (scimax-open-hydra scimax-windows/body) "Windows")
  ;; x ... is for M-x in `scimax-base', don't reassign
  ;; y ... ?
  ("z" (scimax-open-hydra scimax-customize/body) "Customize"))


;;
;;; Applications
;;

(defun scimax-app-hints ()
  "Calculate some variables for the applications hydra."
  (setq elfeed-count
	      (s-pad-right 12 " "
		                 (if (get-buffer "*elfeed-search*")
			                   (format "RSS(%s)"
				                         (car (s-split "/" (with-current-buffer "*elfeed-search*"
						                                         (elfeed-search--count-unread)))))
		                   "RSS(?)"))))

(defhydra scimax-applications ( :hint nil
				                        :pre (scimax-app-hints)
				                        :color blue
				                        :inherit (scimax-base/heads))
  "applications"

  ;; TODO: verify command
  ;; ("a" (org-db-agenda "+2d") "agenda" :column "Emacs")

  ("d" dired "dired" :column  "Emacs")
  
  ;; FIXME: what does this even do? open something like vterm?
  ;; ("b" bash "bash" :column "OS")
  ;; FIXME: probably unnecessary
  ;; ("f" finder "Finder" :column "OS")
  ("e" eshell "eshell" :column "OS")

  ;; TODO: adjust or remove
  ;; ("c" google-calendar "Calendar" :column "Web")
  ("G" (scimax-open-hydra scimax-gsuite/body) "GSuite" :column "Web")

  ("j" scimax-journal/body "journal" :column "Emacs")

  ("m" compose-mail "Compose mail" :column "commands")

  ;; FIXME: don't miss this one, in case it's not from the same source file
  ;; ("n" nb-hydra/body "notebook" :column "Emacs")

  ("p" (scimax-open-hydra cmx-hydra-packages) "Packages (Elpaca)" :column "Emacs")
  ("r" elfeed "elfeed" :column "Emacs"))


(defhydra cmx-hydra-packages ( :hint nil
                               :color blue
                               :inherit (scimax-base/heads))
  "Packages (Elpaca)"
  ("i" (info "Elpaca") "info")
  ("l" elpaca-log "log")
  ("m" elpaca-manager "manager")
  ("r" elpaca-rebuild "rebuild")
  ("s" elpaca-status "status")
  ("t" elpaca-try "try package...")
  ("u" elpaca-update "update package...")
  ("U" elpaca-update-all "update all")
  ("v" elpaca-visit))

(defhydra scimax-gsuite (:color blue)
  "GSuite"
  ("d" (browse-url "https://docs.google.com/document/u/0/" "GDoc"))
  ("h" (browse-url "https://docs.google.com/spreadsheets/u/0/" "GSheet"))
  ("s" (browse-url "https://docs.google.com/presentation/u/0/" "GSlides"))
  ("v" (browse-url "https://drive.google.com/drive/u/0/my-drive") "GDrive"))


;;
;;; Buffers
;;

(defhydra scimax-buffers (:color blue :inherit (scimax-base/heads) :columns 3 :hint nil)
  "buffer"
  ("K" kill-other-buffers      "close others" :column "Close")
  ("M" view-echo-area-messages "*Messages*"   :column "Misc")
  ("R" rename-buffer           "rename..."    :column "File")
  ("S" save-some-buffers       "save all..."  :column "File")

  ("b" switch-to-buffer       "switch"        :column "Switch")
  ("d" kill-current-buffer 		"close buf"     :column "Close" :color red)
  ("k" kill-this-buffer       "close buf+win" :column "Close" :color red)
  ("l" ibuffer                "ibuffer"       :column "Switch")
  ("n" next-buffer            "next"          :column "Switch" :color red)
  ("o" mode-line-other-buffer	"other buf"		  :column "Switch" :color red)
  ("p" previous-buffer        "prev"          :column "Switch" :color red)
  ("r" revert-buffer          "revert..."     :column "File")
  ("s" save-buffer            "save"          :column "File")
  ("x" scratch-buffer         "*scratch*"     :column "Misc"))


;;
;;; Code / LSP
;;

;; NOTE: watch out for potential performance issues here, in case the act of
;;       defining the hydra causes lsp-mode to load now.

(defhydra cmx-hydra-code (:color blue :inherit (scimax-base/heads))
  "code/lsp"
  ("a" lsp-execute-code-action "action"    :column "Refactor")
  ("r" lsp-rename              "rename..." :column "Refactor"))

;;
;;; Emacs Lisp
;;

;; (defhydra)


;; TODO: move to `init-lang-elisp'?
(+general-global-menu! "eval" "e"
  "b" 'eval-buffer
  "d" 'eval-defun
  "e" 'elisp-eval-region-or-buffer
  "E" 'eval-expression
  "I" '("init.el" . (lambda () (interactive) (load-file user-init-file)))
  "p" 'pp-eval-last-sexp
  "r" 'eval-region
  "s" 'eval-last-sexp)

(+general-global-menu! "file" "f"
  "f"  'find-file

  "d"   '("diff with file" . (lambda (&optional arg)
                               (interactive "P")
                               (let ((buffer (when arg (current-buffer))))
                                 (diff-buffer-with-file buffer))))

  ;; "e"   '(:ignore t :which-key "edit")
  ;; FIXME: eval is what we want
  ;; "eR"  '("reload config" (lambda () (interactive) (load-file user-init-file)))

  "s"   'save-buffer
  "S"   '("save as..." . write-file)

  ;; TODO
  ;;"u"  #'+sudo-find-file
  ;;    "U"  #'+sudo-this-file
  ;;"y"  #'+yank-this-file-name

  )

(+general-global-menu! "frame" "F"
  "F" 'select-frame-by-name

  "D" 'delete-other-frames
  "O" 'other-frame-prefix

  ;; FIXME: replace with theme / `modus-themes' bindings
  "c" '(:ignore t :which-key "color")
  "cb" 'set-background-color
  "cc" 'set-cursor-color
  "cf" 'set-foreground-color

  "f" '("set font..." . fontaine-set-preset)
  "m" 'make-frame-on-monitor
  "n" 'next-window-any-frame
  "o" 'other-frame
  "p" 'previous-window-any-frame
  "r" 'set-frame-name)

(+general-global-menu! "git/version-control" "g")

(+general-global-menu! "help" "h"
  ;; FIXME: this seems suspect...
  ;; "h"  (kbd! "C-h" :which-key "help")

  "b"  'describe-bindings
  "f"  'describe-function
  "F"  'describe-face
  "k"  'describe-key
  "l"  'apropos-library
  "o"  'describe-symbol
  "v"  'describe-variable

  ;; FIXME: somehow these inherit the `which-key' labels from the top-level leader menu
  "d"   '(:ignore t :which-key "describe")
  "db"  'describe-bindings
  "df"  'describe-function
  "dF"  'describe-face
  "dk"  'describe-key
  "dt"  '("text props" . (lambda () (interactive)
                           (describe-text-properties (point))))
  "dv"  'describe-variable)

(+general-global-menu! "link" "l")

(+general-global-menu! "narrow" "n"
  "d" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(+general-global-menu! "project" "p"
  "b"  '(:ignore t :which-key "buffer")
  "f"  '("find-file..." . project-find-file))

(+general-global-menu! "quit" "q"
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs
  "Q" 'kill-emacs)

(+general-global-menu! "search" "s"
  "l"  '("+find-library" .
         (lambda () (interactive "P")
           (call-interactively
            (if % #'find-library-other-window #'find-library))))
  "v"  'find-variable-at-point
  "V"  'find-variable)

(+general-global-menu! "tabs" "t"
  "n" '("new" . tab-new))

(+general-global-menu! "toggle" "T"
  "L" '("linums" . line-number-mode)
  "f" '("flycheck" . flycheck-mode))

(+general-global-menu! "window" "w"
  "?" 'split-window-vertically
  "=" 'balance-windows
  "/" 'split-window-horizontally
  "O" 'delete-other-windows
  "X" '("kill-other-buffer-and-window" .
        (lambda () (interactive)
          (call-interactively #'other-window)
          (kill-buffer-and-window)))
  "d" 'delete-window
  "D" 'kill-buffer-and-window
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "o" 'other-window
  "t" '("toggle window dedication" .
        (lambda () (interactive)
          "toggle window dedication"
          (set-window-dedicated-p
           (selected-window)
           (not (window-dedicated-p)))))

  ;; TODO: move to `r' prefix?
  "."  '(:ignore :which-key "resize")
  ".h" '("divider left" .
         (lambda () (interactive)
           (call-interactively
            (if (window-prev-sibling)
                #'enlarge-window-horizontally
              #'shrink-window-horizontally))))

  ".l" '("divider right" .
         (lambda () (interactive)
           (call-interactively
            (if (window-next-sibling)
                #'enlarge-window-horizontally
              #'shrink-window-horizontally))))

  ".j" '("divider up" .
         (lambda () (interactive)
           (call-interactively
            (if (window-next-sibling)
                #'enlarge-window
              #'shrink-window))))

  ".k" '("divider down" .
         (lambda () (interactive)
           (call-interactively
            (if (window-prev-sibling)
                #'enlarge-window
              #'shrink-window)))))



(provide 'init-hydras)
;;; init-hydras.el ends here
