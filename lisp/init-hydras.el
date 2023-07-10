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


;; Add a `:hydra' keyword to `use-package'.
(use-package use-package-hydra
  :after '(hydra))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; pretty-hydra :: Major mode leader key powered by Hydra.
;;  <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra> 

(use-package pretty-hydra
  :after '(hydra))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; major-mode-hydra :: Major mode leader key powered by Hydra.
;;  <https://github.com/jerrypnz/major-mode-hydra.el> 

(use-package major-mode-hydra
  :after '(hydra pretty-hydra)
  :bind
  ;; TODO: verify this is what we want
  ("SPC m" . major-mode-hydra))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; scimax-hydra utilities
;;

;; Lexical closure to encapsulate the stack variable.
(lexical-let ((scimax-hydra-stack '()))
             (defun scimax-hydra-push (expr)
               "Push an EXPR onto the stack."
               (push expr scimax-hydra-stack))

             (defun scimax-hydra-pop ()
               "Pop an expression off the stack and call it."
               (interactive)
               (let ((x (pop scimax-hydra-stack)))
                 (when x
	                 (call-interactively x))))

             (defun scimax-hydra ()
               "Show the current stack."
               (interactive)
               (with-help-window (help-buffer)
                 (princ "Scimax-hydra-stack\n")
                 (pp scimax-hydra-stack)))

             (defun scimax-hydra-reset ()
               "Reset the stack to empty."
               (interactive)
               (setq scimax-hydra-stack '())))

;; TODO: add `!' suffix in line with conventions
(defmacro scimax-open-hydra (hydra)
  "Push current HYDRA to a stack.
This is a macro so I don't have to quote the hydra name."
  `(progn
     (scimax-hydra-push hydra-curr-body-fn)
     (call-interactively ',hydra)))

(defun scimax-hydra-help ()
  "Show help buffer for current hydra."
  (interactive)
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (unless (featurep 'emacs-keybinding-command-tooltip-mode)
	      (require 'emacs-keybinding-command-tooltip-mode))
      (emacs-keybinding-command-tooltip-mode +1))
    (let ((s (format "Help for %s\n" hydra-curr-body-fn)))
      (princ s)
      (princ (make-string (length s) ?-))
      (princ "\n"))

    (princ (mapconcat
	          (lambda (head)
	            (format "%s%s"
		                  ;;  key
		                  (s-pad-right 10 " " (car head))
		                  ;; command
		                  (let* ((hint (if (stringp (nth 2 head))
				                               (concat " " (nth 2 head))
				                             ""))
			                       (cmd (cond
				                           ;; quit
				                           ((null (nth 1 head))
				                            "")
				                           ;; a symbol
				                           ((symbolp (nth 1 head))
				                            (format "`%s'" (nth 1 head)))
				                           ((and (listp (nth 1 head))
					                               (eq 'scimax-open-hydra (car (nth 1 head))))
				                            (format "`%s'" (nth 1 (nth 1 head))))
				                           ((listp (nth 1 head))
				                            (with-temp-buffer
				                              (pp (nth 1 head) (current-buffer))
				                              (let ((fill-prefix (make-string 10 ? )))
					                              (indent-code-rigidly
					                               (save-excursion
					                                 (goto-char (point-min))
					                                 (forward-line)
					                                 (point))
					                               (point-max) 10))
				                              (buffer-string)))
				                           (t
				                            (format "%s" (nth 1 head)))))
			                       (l1 (format "%s%s" (s-pad-right 50 " " (car (split-string cmd "\n"))) hint))
			                       (s (s-join "\n" (append (list l1) (cdr (split-string cmd "\n"))))))
			                  (s-pad-right 50 " " s))))
	          (symbol-value
	           (intern
	            (replace-regexp-in-string
	             "/body$" "/heads"
	             (symbol-name  hydra-curr-body-fn))))
	          "\n"))))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; Hydras
;;

;;** TODO: set naming convention: `cmx-hydra-*'

;;;; shared hydra parts

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


;;;; main hydra

(defhydra scimax ( :color blue
                   :inherit (scimax-base/heads)
                   :columns 4
                   :body-pre (scimax-hydra-reset)
                   :idle 0.5)
  "scimax"
  ("a" (scimax-open-hydra scimax-applications/body) "Applications")
  ("b" (scimax-open-hydra scimax-buffers/body) "Buffers")

  ;; TODO: 'c' for 'code'/lsp

  ;; d ?

  ;; TODO: consider 'eval'?
  ;; FIXME: why this ambiguity?
  ("e" (scimax-open-hydra scimax-errors/body) "Edit/Errors")

  ("f" (scimax-open-hydra scimax-files/body) "Files")
  ;; FIXME: git
  ;; ("g" (scimax-open-hydra scimax-google/body) "Google")
  ("h" (scimax-open-hydra scimax-help/body) "Help")
  ("i" (scimax-open-hydra scimax-insert/body) "Insert")
  ("j" (scimax-open-hydra scimax-jump/body) "Jump")
  ("k" (scimax-open-hydra scimax-bookmarks/body) "Bookmarks")
  ;; TODO: compare with doom et al
  ("l" (scimax-open-hydra scimax-lisp/body) "Lisp")

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
  ("o" (scimax-open-hydra scimax-org/body) "org")

  ("p" (scimax-open-hydra hydra-projectile/body) "Project")

  ;; NOTE: q is for quit in `scimax-base', don't reassign

  ;; TODO: something
  ;; ("r" (scimax-open-hydra scimax-registers/body) "Registers/resume")

  ("s" (scimax-open-hydra scimax-search/body) "Search")

  ;; TODO: i have this set to 'toggle' based on doom, but i like what scimax does with 'text'
  ("t" (scimax-open-hydra scimax-text/body) "Text")

  ;; NOTE: u is a prefix arg in `scimax-base', do not reassign

  ;; FIXME: remap to 'g' for 'git' if only for muscle memory
  ;; ("v" (scimax-open-hydra scimax-version-control/body) "Version control")

  ("w" (scimax-open-hydra scimax-windows/body) "Windows")
  ;; NOTE: x is for M-x in `scimax-base', don't reassign
  ;; y ... ?
  ("z" (scimax-open-hydra scimax-customize/body) "Customize"))


;; (leader-def!
;;   ;; leader maps
;;   "/" '(nil :which-key "search...")
;;   "[" '(nil :which-key "previous...")
;;   "]" '(nil :which-key "next...")
;;   "a" '(nil :which-key "apps...")
;;   "b" '(nil :which-key "buffer...")
;;   "B" '(nil :which-key "bmarks...")
;;   "c" '(nil :which-key "code...")
;;   "e" '(nil :which-key "eval...")
;;   "f" '(nil :which-key "file...")
;;   "F" '(nil :which-key "frame...")
;;   "g" '(nil :which-key "git...")
;;   "h" '(nil :which-key "help...")
;;   "i" '(nil :which-key "insert...")
;;   "j" '(nil :which-key "jump...")
;;   "l" '(nil :which-key "link...")
;;   "o" '(nil :which-key "open...")
;;   "p" '(nil :which-key "project...")
;;   "q" '(nil :which-key "quit...?")
;;   "t" '(nil :which-key "toggle...")
;;   "T" '(nil :which-key "tabs...")
;;   "w" '(nil :which-key "window...")


;;;; applications

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
  ("j" scimax-journal/body "journal" :column "Emacs")
  ;; FIXME: don't miss this one, in case it's not from the same source file
  ;; ("n" nb-hydra/body "notebook" :column "Emacs")
  ("r" elfeed "elfeed" :column "Emacs")
  
  ;; FIXME: what does this even do? open something like vterm?
  ;; ("b" bash "bash" :column "OS")
  ;; FIXME: probably unnecessary
  ;; ("f" finder "Finder" :column "OS")
  ("e" eshell "eshell" :column "OS")

  ;; TODO: adjust or remove
  ;; ("c" google-calendar "Calendar" :column "Web")
  ;; ("g" google "Google" :column "Web")
  ("G" (scimax-open-hydra scimax-gsuite/body) "GSuite" :column "Web")
  ;; ("s" slack/body "Slack" :column "Web")
  
  ;; FIXME: find another binding, connect with elpaca 
  ;; ("k" package-list-packages "List packages" :column "commands")

  ("m" compose-mail "Compose mail" :column "commands"))

(defhydra cmx-hydra-packages (:hint nil
                                    :color blue :inherit scimax-base/heads) "Packages (elpaca)" ("r" elpaca-i))


(+general-global-menu! "application" "a"
  "p" '(:ignore t "elpaca")
  "pr"  '("rebuild" . (lambda () (interactive)
                        (let ((current-prefix-arg (not current-prefix-arg)))
                          (call-interactively #'elpaca-rebuild))))
  "pm" 'elpaca-manager
  "pl" 'elpaca-log
  "pi" '("elpaca-info" . (lambda () (interactive) (info "Elpaca")))
  "ps" 'elpaca-status
  "pt" 'elpaca-try
  "pu" 'elpaca-update
  "pU" 'elpaca-update-all
  "pv" 'elpaca-visit)



(defhydra scimax-gsuite (:color blue)
  "GSuite"
  ("v" (browse-url "https://drive.google.com/drive/u/0/my-drive") "GDrive")
  ("d" (browse-url "https://docs.google.com/document/u/0/" "GDoc"))
  ("h" (browse-url "https://docs.google.com/spreadsheets/u/0/" "GSheet"))
  ("s" (browse-url "https://docs.google.com/presentation/u/0/" "GSlides"))
  ("j" (browse-url "https://jamboard.google.com/" "Jamboard")))

(+general-global-menu! "buffer" "b"
  "h"  '("prev" . previous-buffer)
  ;; TODO: is there an alternative with better ui? maybe: bufler.el
  "i"  'ibuffer
  "d"  '("delete" . kill-current-buffer)
  "l"  '("next" . next-buffer)
  "k"  '("kill" . kill-current-buffer)
  "M"  '("messages" . view-echo-area-messages)
  "n"  '("next" . next-buffer)
  "o"  '("other..." . mode-line-other-buffer)
  "p"  '("prev" . previous-buffer)
  "r"  '("revert" . revert-buffer)
  "R"  '("rename..." . rename-buffer)
  "s"  '("save" . save-buffer)
  "S"  #'save-some-buffers
  "x"  '("scratch" . scratch-buffer)

  "["  'previous-buffer
  "]"  'next-buffer
  "TAB"  'mode-line-other-buffer)

(+general-global-menu! "bookmark" "B")

(+general-global-menu! "code" "c")

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
