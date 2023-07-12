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

(require 'lib-hydras)



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

(elpaca-wait)

;; 
;;; Utilities
;;



;; (defmacro /bindings/define-prefix-keys (keymap prefix &rest body)
;;   (declare (indent defun))
;;   `(progn
;;      ,@(cl-loop for binding in body
;;                 collect
;;                 `(let ((seq ,(car binding))
;;                        (func ,(cadr binding))
;;                        (desc ,(caddr binding)))
;;                    (define-key ,keymap (kbd seq) func)
;;                    (when desc
;;                      (which-key-add-key-based-replacements
;;                        (if ,prefix
;;                            (concat ,prefix " " seq)
;;                          seq)
;;                        desc))))))

;; (defmacro /bindings/define-keys (keymap &rest body)
;;   (declare (indent defun))
;;   `(/bindings/define-prefix-keys ,keymap nil ,@body))

;; (defmacro /bindings/define-key (keymap sequence binding &optional description)
;;   (declare (indent defun))
;;   `(/bindings/define-prefix-keys ,keymap nil
;;      (,sequence ,binding ,description)))

;; 

;; (setq /bindings/normal-space-leader-map (make-sparse-keymap))
;; (/bindings/define-prefix-keys /bindings/normal-space-leader-map "SPC"
;;   ("SPC" #'execute-extended-command "M-x")
;;   ("t" #'/hydras/toggles/body "toggle...")
;;   ("q" #'/hydras/quit/body "quit...")
;;   ("e" #'/hydras/errors/body "errors...")
;;   ("b" #'/hydras/buffers/body "buffers...")
;;   ("j" #'/hydras/jumps/body "jump...")
;;   ("f" #'/hydras/files/body "files...")
;;   ("s" #'/hydras/search/body "search...")
;;   ("l" #'/hydras/jumps/lambda-l-and-exit "lines(current)")
;;   ("L" #'/hydras/jumps/lambda-L-and-exit "lines(all)")
;;   ("o" #'/hydras/jumps/lambda-i-and-exit "outline")
;;   ("v" vc-prefix-map)
;;   ("4" ctl-x-4-map)
;;   ("5" ctl-x-5-map "frames...")
;;   ("'" #'/eshell/new-split "shell")
;;   ("y" (bind
;;         (cond
;;          ((eq dotemacs-switch-engine 'ivy)
;;           (call-interactively #'counsel-yank-pop))
;;          ((eq dotemacs-switch-engine 'consult)
;;           (call-interactively #'consult-yank-pop))
;;          ((eq dotemacs-switch-engine 'helm)
;;           (call-interactively #'helm-show-kill-ring)))) "kill-ring"))

;; 
;;; Shared Hydra Parts
;;

(defhydra cmx-hydra/base (:color blue)
  "base"
  ("," cmx-hydra-pop "back" :color blue)
  ;; ("x" execute-extended-command "M-x")
  ("C-s" save-buffer "Save")
  ("/" undo "undo" :color red)
  ("\\" redo "redo" :color red)
  ("?" cmx-hydra-help "Menu help")

  ;; FIXME: 'm'
  ;; ("." cmx-dispatch-mode-hydra "Major mode hydras")

  ("`" (switch-to-buffer (other-buffer (current-buffer) 1)) "other buffer")
  ("SPC" #'project-find-file "")
  ("q" nil "quit"))


;; 
;;; Main Hydra
;;


(pretty-hydra-define cmx-hydra/main
  ( :title "ceamx/"
    :color blue
    :inherit (cmx-hydra/base/heads)
    :body-pre (cmx-hydra-reset)
    :idle 0.5)

  (""
   (;;   "/" '(nil :which-key "search...")
    ;;   "[" '(nil :which-key "previous...")
    ;;   "]" '(nil :which-key "next...")

    ("a" (enter-hydra! cmx-hydra/applications/body) "Applications")
    ("b" (enter-hydra! cmx-hydra/buffers/body) "Buffers")
    ;; TODO: 'B' for 'bookmarks'
    ;; TODO: 'c' for 'code'/lsp
    ;; d ... ?

    ;; TODO: consider 'eval'?
    ;; FIXME: why this ambiguity?
    ("e" (enter-hydra! cmx-hydra/errors/body) "Edit/Errors")

    ("f" (enter-hydra! cmx-hydra/files/body) "Files")

    ;; TODO: 'F' for 'frame'
    ;; ("F" (enter-hydra! cmx-hydra/frame/body) "Frame")

    ;; FIXME: git
    ("g" (enter-hydra! cmx-hydra/git/body) "Git")

    ;;("h" (enter-hydra! cmx-hydra/help/body) "Help")
    ;;("i" (enter-hydra! cmx-hydra/insert/body) "Insert")
    ;;("j" (enter-hydra! cmx-hydra/jump/body) "Jump")
    ;;("k" (enter-hydra! cmx-hydra/bookmarks/body) "Bookmarks")
    ;; TODO: compare with doom et al
    ;;   "l" '(nil :which-key "link...")
    ;; ("l" (enter-hydra! cmx-hydra/lisp/body) "Lisp")

    ;; FIXME: swap with major modes
    ;; ("m" (enter-hydra! cmx-hydra/minor-modes/body) "Minor modes/mark")
    ;; FIXME: swap with minor modes
    ;; ("s-m" cmx-dispatch-mode-hydra "Major mode hydras")

    ;; TODO: consider...?
    ;; ("M" (enter-hydra! cmx-hydra/smerge/body) "smerge")

    ;; FIXME: 'notes'? but that might be better in 'org'
    ;; ("n" (enter-hydra! cmx-hydra/navigation/body) "Navigation")

    ;; ("o" (enter-hydra! cmx-hydra/org/body) "org")

    ;; ("p" (enter-hydra! hydra-projectile/body) "Project")
    ;; q ... is for quitting in `cmx-hydra/base', don't reassign
    ;; r ... ?
    ;; ("s" (enter-hydra! cmx-hydra/search/body) "Search")

    ;; TODO: i have this set to 'toggle' based on doom, but i like what scimax does with 'text'
    ;;   "t" '(nil :which-key "toggle...")
    ;; ("t" (enter-hydra! cmx-hydra/text/body) "Text")

    ;; TODO:   "T" '(nil :which-key "tabs...")

    ;; NOTE: u is a prefix arg in `cmx-hydra/base', do not reassign

    ;; FIXME: remap to 'g' for 'git' if only for muscle memory
    ;; ("v" (enter-hydra! cmx-hydra/version-control/body) "Version control")

    ;; ("w" (enter-hydra! cmx-hydra/windows/body) "Windows")
    ;; x ... is for M-x in `cmx-hydra/base', don't reassign
    ;; y ... ?
    ;; ("z" (enter-hydra! cmx-hydra/customize/body) "Customize")
    )))


;;
;;; Applications
;;

;; (defun cmx-hydra--app-hints ()
;;   "Calculate some variables for the applications hydra."
;;   (setq elfeed-count
;; 	      (s-pad-right 12 " "
;; 		                 (if (get-buffer "*elfeed-search*")
;; 			                   (format "RSS(%s)"
;; 				                         (car (s-split "/" (with-current-buffer "*elfeed-search*"
;; 						                                         (elfeed-search--count-unread)))))
;; 		                   "RSS(?)"))))

(pretty-hydra-define cmx-hydra/applications
  ( :title "ceamx/applications/" 
    :hint nil
	  ;; :pre (cmx-hydra--app-hints)
	  :color blue
	  :inherit (cmx-hydra/base/heads))

  ("Emacs"
   (;; TODO: verify command
    ;; ("a" (org-db-agenda "+2d") "agenda" :column "Emacs")
    ("d" dired "dired")
    ;;  ("j" cmx-hydra/journal/body "journal")
    ;; FIXME: don't miss this one -- it's not from the same source file
    ;; ("n" nb-hydra/body "notebook" :column "Emacs")
    ("p" (enter-hydra! cmx-hydra/packages/body) "Packages")
    ("r" #'elfeed "elfeed"))

   "OS"
   (("e" eshell "eshell"))

   "Web"
   (;; TODO: adjust or remove
    ;; ("c" google-calendar "Calendar" :column "Web")
    ("G" (enter-hydra! cmx-hydra/gsuite/body) "GSuite"))

   "Commands"
   (("m" compose-mail "Compose mail"))))


(pretty-hydra-define cmx-hydra/packages
  ( :title "ceamx/applications/packages/" 
    :hint nil
    :color blue
    :columns 3
    :inherit (cmx-hydra/base/heads))

  ("Elpaca"
   (("i" (info "Elpaca") "info")
    ("l" #'elpaca-log "log")
    ("m" #'elpaca-manager "manager")
    ("r" #'elpaca-rebuild "rebuild")
    ("s" #'elpaca-status "status")
    ("t" #'elpaca-try "try package...")
    ("u" #'elpaca-update "update package...")
    ("v" #'elpaca-visit)
    ("U" #'elpaca-update-all "update all"))))

(defhydra cmx-hydra/gsuite (:color blue :inherit (cmx-hydra/base/heads))
  "ceamx/applications/gsuite/"
  ("d" (browse-url "https://docs.google.com/document/u/0/") "GDoc")
  ("h" (browse-url "https://docs.google.com/spreadsheets/u/0/") "GSheet")
  ("s" (browse-url "https://docs.google.com/presentation/u/0/") "GSlides")
  ("v" (browse-url "https://drive.google.com/drive/u/0/my-drive") "GDrive"))


;;
;;; Buffers
;;

(pretty-hydra-define cmx-hydra/buffers
  ( :title "ceamx/buffers/"
    :color blue
    :inherit (cmx-hydra/base/heads)
    :columns 3
    :hint nil)

  ("Switch"
   (("b" switch-to-buffer       "switch")
    ("l" ibuffer                "ibuffer")
    ("n" next-buffer            "next"       :color red)
    ("o" mode-line-other-buffer	"other buf"	 :color red)
    ("p" previous-buffer        "prev"       :color red))

   "File"
   (("r" revert-buffer          "revert...")
    ("R" rename-buffer          "rename...")
    ("s" save-buffer            "save")
    ("S" save-some-buffers      "save all..."))

   "Close"
   (("d" kill-current-buffer 		"close buf"      :color red)
    ("k" kill-this-buffer       "close buf+win"  :color red)
    ("K" kill-other-buffers     "close others"))

   "Misc"
   (("M" view-echo-area-messages "*Messages*")
    ("x" scratch-buffer          "*scratch*"))))


;;
;;; Code / LSP
;;

;; NOTE: watch out for potential performance issues here, in case the act of
;;       defining the hydra causes lsp-mode to load now.

(pretty-hydra-define cmx-hydra/code
  ( :title "ceamx/code/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ("Refactor"
   (("a" #'lsp-execute-code-action "action"    :column "Refactor")
    ("r" #'lsp-rename              "rename..." :column "Refactor"))))

;;
;;; Emacs Lisp
;;

;; FIXME: prob should be some other name
(defhydra cmx-hydra/eval (:color blue :inherit (cmx-hydra/base/heads))
  "ceamx/eval/"
  ("b" #'eval-buffer)
  ("d" #'eval-defun)
  ("e" #'elisp-eval-region-or-buffer)
  ("p" #'pp-eval-last-sexp)
  ("r" #'eval-region)
  ("s" #'eval-last-sexp)

  ("E" #'eval-expression)
  ("I" (load-file user-init-file) "init.el"))


;;
;;; Files
;;

(pretty-hydra-define cmx-hydra/file
  ( :title "ceamx/file/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ;; TODO:
  ;; "e"   '(:ignore t :which-key "edit")
  ;; FIXME: eval is what we want
  ;; "eR"  '("reload config" (lambda () (interactive) (load-file user-init-file)))
  ;;"u"  #'+sudo-find-file
  ;;    "U"  #'+sudo-this-file
  ;;"y"  #'+yank-this-file-name

  ("File"
   (("d" (lambda (&optional arg)
           (interactive "P")
           (let ((buffer (when arg (current-buffer))))
             (diff-buffer-with-file buffer)))
     "diff with file")
    ("f" #'find-file)
    ("S" #'write-file "save as...")
    ("s" #'save-buffer))))

(pretty-hydra-define cmx-hydra/git
  ( :title "ceamx/git/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ("Working Tree"
   (("b" #'magit-branch "branch")
    ("B" #'magit-blame "blame")
    ("g" #'magit-status "status"))

   "File"
   (("ff" #'magit-find-file "find...")
    ("fh" #'magit-log-buffer-file "history...")
    ("S" #'magit-stage-file "stage")
    ("U" #'magit-unstage-file "unstage"))

   "Repo"
   (("c" #'magit-clone "clone...")
    ("i" #'magit-init "init")
    ("L" #'magit-list-repos "list repos"))

   ""
   (("d" #'magit-dispatch))))


;; (+general-global-menu! "help" "h"
;;   ;; FIXME: this seems suspect...
;;   ;; "h"  (kbd! "C-h" :which-key "help")

;;   "b"  'describe-bindings
;;   "f"  'describe-function
;;   "F"  'describe-face
;;   "k"  'describe-key
;;   "l"  'apropos-library
;;   "o"  'describe-symbol
;;   "v"  'describe-variable

;;   ;; FIXME: somehow these inherit the `which-key' labels from the top-level leader menu
;;   "d"   '(:ignore t :which-key "describe")
;;   "db"  'describe-bindings
;;   "df"  'describe-function
;;   "dF"  'describe-face
;;   "dk"  'describe-key
;;   "dt"  '("text props" . (lambda () (interactive)
;;                            (describe-text-properties (point))))
;;   "dv"  'describe-variable)

;; (+general-global-menu! "link" "l")

;; (+general-global-menu! "narrow" "n"
;;   "d" 'narrow-to-defun
;;   "p" 'narrow-to-page
;;   "r" 'narrow-to-region
;;   "w" 'widen)

;; (+general-global-menu! "project" "p"
;;   "b"  '(:ignore t :which-key "buffer")
;;   "f"  '("find-file..." . project-find-file))

;; (+general-global-menu! "quit" "q"
;;   "q" 'save-buffers-kill-emacs
;;   "r" 'restart-emacs
;;   "Q" 'kill-emacs)

;; (+general-global-menu! "search" "s"
;;   "l"  '("+find-library" .
;;          (lambda () (interactive "P")
;;            (call-interactively
;;             (if % #'find-library-other-window #'find-library))))
;;   "v"  'find-variable-at-point
;;   "V"  'find-variable)

;; (+general-global-menu! "tabs" "t"
;;   "n" '("new" . tab-new))

;; (+general-global-menu! "toggle" "T"
;;   "L" '("linums" . line-number-mode)
;;   "f" '("flycheck" . flycheck-mode))

;; (+general-global-menu! "window" "w"
;;   "?" 'split-window-vertically
;;   "=" 'balance-windows
;;   "/" 'split-window-horizontally
;;   "O" 'delete-other-windows
;;   "X" '("kill-other-buffer-and-window" .
;;         (lambda () (interactive)
;;           (call-interactively #'other-window)
;;           (kill-buffer-and-window)))
;;   "d" 'delete-window
;;   "D" 'kill-buffer-and-window
;;   "h" 'windmove-left
;;   "j" 'windmove-down
;;   "k" 'windmove-up
;;   "l" 'windmove-right
;;   "o" 'other-window
;;   "t" '("toggle window dedication" .
;;         (lambda () (interactive)
;;           "toggle window dedication"
;;           (set-window-dedicated-p
;;            (selected-window)
;;            (not (window-dedicated-p)))))

;;   ;; TODO: move to `r' prefix?
;;   "."  '(:ignore :which-key "resize")
;;   ".h" '("divider left" .
;;          (lambda () (interactive)
;;            (call-interactively
;;             (if (window-prev-sibling)
;;                 #'enlarge-window-horizontally
;;               #'shrink-window-horizontally))))

;;   ".l" '("divider right" .
;;          (lambda () (interactive)
;;            (call-interactively
;;             (if (window-next-sibling)
;;                 #'enlarge-window-horizontally
;;               #'shrink-window-horizontally))))

;;   ".j" '("divider up" .
;;          (lambda () (interactive)
;;            (call-interactively
;;             (if (window-next-sibling)
;;                 #'enlarge-window
;;               #'shrink-window))))

;;   ".k" '("divider down" .
;;          (lambda () (interactive)
;;            (call-interactively
;;             (if (window-prev-sibling)
;;                 #'enlarge-window
;;               #'shrink-window)))))



(provide 'init-hydras)
;;; init-hydras.el ends here
