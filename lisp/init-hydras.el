;;; init-hydras.el --- Unleash the hydras -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>
;; Copyright (c) 2016-2023  John Kitchin <jkitchin@andrew.cmu.edu> and the Scimax contributors

;; Maintainer:  Chris Montgomery <chris@cdom.io>
;; Author:  Chris Montgomery <chris@cdom.io>
;;          John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Modified: 19 July 2023
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

;;; Common heads

(defhydra cmx-hydra/base (:color blue)
  "base"

  (","   #'cmx-hydra-pop  "back" :color blue)
  ("C-s" #'save-buffer    "save")
  ("/"   #'undo           "undo" :color red)
  ("\\"  #'undo-redo      "redo" :color red)
  ("?"   #'cmx-hydra-help "help")

  ;; FIXME: 'm'
  ;; ("." cmx-dispatch-mode-hydra "Major mode hydras")

  ("`" (switch-to-buffer
        (other-buffer (current-buffer) 1))
   "other buffer")
  ("SPC" #'project-find-file "find file..."))

;;; Main Hydra

(pretty-hydra-define cmx-hydra/main
  ( :title "ceamx/"
    :color blue
    :inherit (cmx-hydra/base/heads)
    :body-pre (cmx-hydra-reset)
    :columns 4)

  (""
   (;;   "/" '(nil :which-key "search...")
    ;;   "[" '(nil :which-key "previous...")
    ;;   "]" '(nil :which-key "next...")

    ("a" (enter-hydra! cmx-hydra/applications/body) "Applications")
    ("b" (enter-hydra! cmx-hydra/buffer/body) "Buffer")
    ("B" (enter-hydra! cmx-hydra/bookmark/body) "Bookmark")
    ("c" (enter-hydra! cmx-hydra/code/body) "Code")
    ;; d ... ?
    ("e" (enter-hydra! cmx-hydra/eval/body) "Eval")
    ("f" (enter-hydra! cmx-hydra/file/body) "File")
    ("F" (enter-hydra! cmx-hydra/frame/body) "Frame")
    ("g" (enter-hydra! cmx-hydra/git/body) "Git")
    ("h" (enter-hydra! cmx-hydra/help/body) "Help")
    ;;("i" (enter-hydra! cmx-hydra/insert/body) "Insert")
    ;;("j" (enter-hydra! cmx-hydra/jump/body) "Jump")
    ;;("J" (enter-hydra! cmx-hydra/journal/body) "Journal")
    ;; k ... bookmarks ... ?
    ;; l ... links ... ?
    ;; m --- RESERVED: for major mode map
    ;; M ... major mode hydras?
    ;; TODO: 'notes'
    ;; ("n" (enter-hydra! cmx-hydra/notes/body) "Notes")
    ("o" (enter-hydra! cmx-hydra/org/body) "Org-Mode")
    ("p" (enter-hydra! cmx-hydra/project/body) "Project")
    ("q" (enter-hydra! cmx-hydra/session/body) "Session")
    ;; r ... ?
    ("s" (enter-hydra! cmx-hydra/search/body) "Search")
    ("t" (enter-hydra! cmx-hydra/toggle/body) "Toggles")
    ("T" (enter-hydra! cmx-hydra/tab/body) "Tabs")
    ;; u ... ?
    ;; v ... ?
    ("w" (enter-hydra! cmx-hydra/window/body) "Windows")
    ;; x ... ?
    ("X" (enter-hydra! cmx-hydra/org-capture/body) "Capture")
    ;; y ... ?
    ;; z ... ?
    )))

;;; Applications

(pretty-hydra-define cmx-hydra/applications
  ( :title "ceamx/applications/"
    :hint nil
	  :color blue
	  :inherit (cmx-hydra/base/heads))

  ("Emacs"
   (;; TODO: verify command
    ;; ("a" (org-db-agenda "+2d") "agenda" :column "Emacs")
    ("d" dired "dired")
    ;;  ("j" cmx-hydra/journal/body "journal")
    ;; FIXME: don't miss this one -- it's not from the same source file
    ;; ("n" nb-hydra/body "notebook" :column "Emacs")
    ("n" #'newsticker-show-news "news")
    ("p" (enter-hydra! cmx-hydra/packages/body) "Packages"))

   "Shells"
   (("e" eshell "eshell"))

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
    ("m" #'elpaca-manager "manager")
    ("r" #'elpaca-rebuild "rebuild")
    ("s" #'elpaca-status "status")
    ("t" #'elpaca-try "try package...")
    ("u" #'elpaca-update "update package...")
    ("v" #'elpaca-visit)
    ("U" #'elpaca-update-all "update all"))))

;;; Bookmarks

(pretty-hydra-define cmx-hydra/bookmark
  ( :title "ceamx/bookmark/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ("Save"
   (("F" #'burly-bookmark-frames  "frames")
    ("W" #'burly-bookmark-windows "windows"))))

;;; Buffers

(pretty-hydra-define cmx-hydra/buffer
  ( :title "ceamx/buffer/"
    :color blue
    :inherit (cmx-hydra/base/heads)
    :columns 3)

  ("Switch"
   (("b" #'consult-project-buffer "in prj...")
    ("B" #'consult-buffer         "any...")
    ("o" #'mode-line-other-buffer	"other"	     :color red)
    ("[" #'previous-buffer        "prev"       :color red)
    ("]" #'next-buffer            "next"       :color red))

   "File"
   (("r" #'revert-buffer          "revert...")
    ("R" #'rename-buffer          "rename...")
    ("s" #'save-buffer            "save")
    ("S" #'save-some-buffers      "save all..."))

   "Close"
   ;; TODO: red feels weird, maybe reconsider
   (("d" #'kill-current-buffer 		"close buf"      :color red)
    ("k" #'kill-this-buffer       "close buf+win"  :color red)
    ("K" #'kill-other-buffers     "close others"))

   "Misc"
   (("M" #'view-echo-area-messages "*Messages*")
    ;; TODO: why does it need to be evil?
    ("N" #'evil-buffer-new       "new")
    ("x" #'scratch-buffer          "*scratch*"))))

;;; Code / LSP

(pretty-hydra-define cmx-hydra/code
  ( :title "ceamx/code/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ("Refactor"
   (("a" #'lsp-execute-code-action "action"    :column "Refactor")
    ("r" #'lsp-rename              "rename..." :column "Refactor"))))

;;; Emacs Lisp / "Eval"

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

;;; Files

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

  ("Current"
   (("s" #'save-buffer          "save")
    ("S" #'write-file           "save as...")
    ("C" #'cmx/copy-this-file   "copy...")
    ("D" #'cmx/delete-this-file "delete")
    ("R" #'cmx/move-this-file   "move..."))

   "Others"
   (("d" (lambda (&optional arg)
           (interactive "P")
           (let ((buffer (when arg (current-buffer))))
             (diff-buffer-with-file buffer)))
     "diff with...")
    ("f" #'find-file "find..."))))

;;; Frames

(pretty-hydra-define cmx-hydra/frame
  ( :title "ceamx/frame/"
    :inherit (cmx-hydra/base/heads))

  ("Switch"
   (("F" #'select-frame-by-name      "by name...")
    ("o" #'other-frame               "other")
    ("[" #'previous-window-any-frame "prev")
    ("]" #'next-window-any-frame     "next"))

   "Manage"
   (("b" #'burly-bookmark-frames         "save workspace")
    ("n" #'make-frame-on-current-monitor "create frame")
    ("N" #'make-frame-on-monitor         "create frame on monitor...")
    ("R" #'set-frame-name                "rename frame"))))

;;; Git / Version Control

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
    ("t"  #'git-timemachine "timemachine")
    ("S"  #'magit-stage-file "stage")
    ("U"  #'magit-unstage-file "unstage"))

   "Repo"
   (("c" #'magit-clone "clone...")
    ("i" #'magit-init "init")
    ("L" #'magit-list-repos "list repos"))

   ""
   (("d" #'magit-dispatch))))

;;; Help

(pretty-hydra-define cmx-hydra/help
  ( :title "ceamx/help/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  (""
   (("h" #'help-for-help))

   "Describe"
   (("b" #'embark-bindings                  "bindings...")
    ("f" #'describe-function                "function...")
    ("F" #'describe-face                    "face...")
    ("k" #'describe-key                     "key...")
    ("o" #'describe-symbol                  "symbol...")
    ("t" (describe-text-properties (point)) "text properties...")
    ("v" #'describe-variable                "variable..."))

   "Apropos"
   (("l" #'apropos-library))))

;;; Org-Mode

(pretty-hydra-define cmx-hydra/org
  ( :title "ceamx/org/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ("Tasks"
   (("t" #'org-todo-list "todo list"))

   ""
   (("c" (enter-hydra! cmx-hydra/org-capture/body)))))

(defhydra cmx-hydra/org-capture
  ( :color blue
    :inherit (cmx-hydra/base/heads))
  "org-capture"
  ("c" #'org-capture "capture..."))

;;; Project

(pretty-hydra-define cmx-hydra/project
  ( :title "ceamx/project/"
    :color blue
    :inherit (cmx-hydra/base/heads))

  ("Files"
   (("f" #'projectile-find-file-dwim "find..."))

   "Meta"
   (("a" #'projectile-add-known-project "add project...")
		("i" #'projectile-invalidate-cache  "invalidate cache")
    ("p" #'projectile-switch-project    "switch project..."))))

;;; Session

(defhydra cmx-hydra/session (:color blue)
  "[ceamx/session]"
  ("r" #'restart-emacs           "restart")
  ("q" #'save-buffers-kill-emacs "save and quit"))

;;; Search

(defhydra cmx-hydra/search
  ( :color blue
    :columns 3
    :inherit (cmx-hydra/base/heads))
  "
[ceamx/search]
--------------
Text                    Symbols            Misc
-------------------------------------------------------------------
_s_: line...        _j_: in file...        _l_: libraries/features...
_o_: outline...     _J_: in workspace...   _h_: search history...
_p_: in project...  _v_: this var         
                    _V_: some var...
                    _x_: refs
"
  ("i" #'consult-isearch-history)
  ("j" #'consult-lsp-file-symbols)
  ("J" #'consult-lsp-symbols      )
  ("l" (lambda () (interactive "P")
         (call-interactively
          (if % #'find-library-other-window
            #'find-library))))
  ("o" #'consult-outline)
  ("p" #'consult-ripgrep)
  ("s" #'consult-line)
  ("v" #'find-variable-at-point )
  ("V" #'find-variable)
  ("x" #'projectile-find-references))


;;; Tabs

(defhydra cmx-hydra/tab
  ( :color blue
    :inherit (cmx-hydra/base/heads))
  "[ceamx/tab]"
  ("n" #'tab-new))

;;; Toggles

(defhydra cmx-hydra/toggle
  (:color blue
          :inherit (cmx-hydra/base/heads))
  "
[ceamx/toggle]
---------------------------------------------------------------------------------
_f_: flycheck
_L_: linums
"
  ("L" #'line-number-mode)
  ("f" #'flycheck-mode))

;;; Window Management

(defhydra cmx-hydra/window (:hint nil)
  "
[ceamx/window]
---------------------------------------------------------
^Movement^    ^Split^         ^Switch^		  ^Resize^
---------------------------------------------------------
  ^_k_^       _v_ertical      _b_uffer		  _q_ X←
_h_   _l_     _x_ horizontal	_f_ind files	_w_ X↓
  ^_j_^       _z_ undo      	_a_ce 1		    _e_ X↑
              _Z_ reset      	_s_wap		    _r_ X→
_F_ollow      _D_lt Other   	_S_ave		    max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
  ("h" #'windmove-left   :color blue)
  ("j" #'windmove-down   :color blue)
  ("k" #'windmove-up     :color blue)
  ("l" #'windmove-right  :color blue)
  ("q" #'hydra-move-splitter-left)
  ("w" #'hydra-move-splitter-down)
  ("e" #'hydra-move-splitter-up)
  ("r" #'hydra-move-splitter-right)
  ("b" #'consult-buffer)
  ("f" #'projectile-find-file)
  ("F" #'follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         ;; FIXME: does this exist?
         (add-hook 'ace-window-end-once-hook
                   #'cmx-hydra/window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'cmx-hydra/window/body)))
  ("S" #'burly-bookmark-windows)
  ("d" #'delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   #'cmx-hydra/window/body)))
  ("o" #'delete-other-windows  :color blue)
  ("i" #'ace-maximize-window   :color blue)
  ("z" (progn
         (winner-undo)
         (setq this-command #'winner-undo)))
  ("Z" #'winner-redo)
  ("SPC" nil))


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


;; via <https://github.com/abo-abo/hydra/wiki/Rectangle-Operations#rectangle-2>
(require 'rect)
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))      ;; ok

;; TODO: move this to `init-keybinds'
(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)


(provide 'init-hydras)
;;; init-hydras.el ends here
