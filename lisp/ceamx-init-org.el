;; -*- lexical-binding: t -*-

(require 'f)

(require 'ceamx-lib)
(require 'ceamx-note)

;; Set up essential files & directories :paths:

;; These must be set before loading Org-Mode or any of its sub-features
;; are used.

;; Most notes will be stored in ~ceamx-notes-dir~, defined in ~ceamx-paths~.

;; Because these directories are managed by Syncthing, creating them
;; automatically is not a great idea if they do not already exist.  A
;; better workaround to the issue of Org-Mode failing to load might be
;; setting the default target to a subdirectory of ~user-emacs-directory~.

;; Why?  Because you do not want to end up with two Syncthing entries
;; with the same intended target path but with different IDs.  If
;; Syncthing is not set up yet, then any directory created via the Emacs
;; configuration will probably result in a conflict when Syncthing tries
;; to take control of these paths.


(defcustom ceamx-agenda-dir
  (file-name-as-directory (concat ceamx-note-dir "g2d"))
  "Base directory for Org-Agenda."
  :type 'directory
  :group 'ceamx)

(defcustom ceamx-default-agenda-files
  (file-expand-wildcards (file-name-concat ceamx-agenda-dir "*.org"))
  "List of absolute paths of all files to include in the agenda."
  :type '(repeat file)
  :group 'ceamx)

(defcustom ceamx-default-todo-file
  (expand-file-name "todo.org" ceamx-agenda-dir)
  "Absolute path to default Something-Doing file."
  :type 'file
  :group 'ceamx)



;; The value of ~org-directory~ will be used as a default destination for
;; new notes, especially as they relate to tasks and agendas.  For that
;; reason, use the ~ceamx-agenda-dir~.


(progn
  (defvar org-directory ceamx-agenda-dir)
  (make-directory org-directory t))



;; Let’s immediately override ~ceamx-default-agenda-files~ to keep down the noise:


(setopt ceamx-default-agenda-files
        (list (file-name-concat ceamx-agenda-dir "todo.org")
              (file-name-concat ceamx-agenda-dir "work.org")))

(setopt org-agenda-files ceamx-default-agenda-files)

;; Install the Org-Mode package instead of using the builtin version :package:

;; But watch out, you might want to pin to some stable version!  As long as
;; you keep it updated.


(package! (org :autoloads "org-loaddefs.el"))

;; Baseline Org-Mode customizations


(after! org
  ;;
  ;; Links & IDs

  (setopt org-link-context-for-files t)
  (setopt org-link-keep-stored-after-insertion nil)
  (setopt org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setopt org-clone-delete-id t)

  ;;
  ;; Editing

  (setopt org-return-follows-link t)
  (setopt org-special-ctrl-a/e t
          org-special-ctrl-k t
          org-special-ctrl-o t
          org-ctrl-k-protect-subtree t)
  (setopt org-reverse-note-order nil)
  (setopt org-list-use-circular-motion t)
  (setopt org-blank-before-new-entry
          '((heading . auto) (plain-list-item . auto)))

  (setopt org-M-RET-may-split-line '((default . nil))
          ;; Invoke `org-insert-heading-respect-content' directly with
          ;; [C-<return>], but let `org-insert-heading' ([M-<return>])
          ;; insert a heading wherever.
          org-insert-heading-respect-content nil)

  ;;
  ;; Folding

  (setopt org-cycle-emulate-tab t)
  (setopt org-fold-catch-invisible-edits 'show-and-error)
  ;; <https://github.com/jcfk/savefold.el#usage>
  (setopt org-startup-folded
          (if (bound-and-true-p savefold-mode)
              'showeverything
            'content))

  ;;
  ;; Priority

  (setopt org-priority-start-cycle-with-default nil)

  ;;
  ;; Workflow states

  (setopt org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t)

  (setopt org-todo-keywords '((sequence
                               "TODO(t)"
                               "INPRG(i@/!)"
                               "BLOCKED(b@)"
                               "HOLD(h@)"
                               "PROJ(p)"
                               "|"
                               "DONE(d!)"
                               "CANCELLED(x@/!)")))

  ;;
  ;; Clocking

  (setopt org-clock-in-switch-to-state "INPRG")

  ;;
  ;; Logging

  (setopt org-log-done 'time
          org-log-redeadline 'time
          org-log-refile 'time)
  (setopt org-log-into-drawer t)
  (setopt org-log-states-order-reversed nil)
  (setopt org-log-note-clock-out nil)

  ;;
  ;; Rich media & attachments

  (setopt org-image-actual-width 480)
  (setopt org-startup-with-inline-images t)

  ;;
  ;; Miscellaneous

  (setopt org-structure-template-alist
          '(("s" . "src")
            ("e" . "src emacs-lisp")
            ("E" . "src emacs-lisp :results value code :lexical t")
            ("t" . "src emacs-lisp :tangle FILENAME")
            ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
            ("x" . "example")
            ("X" . "export")
            ("q" . "quote"))))

;; Enforce the correct ~tab-width~ to prevent errors :hack:formatting:

;; <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>


(defun +org-mode--local-set-tab-width-h ()
  "Set the `tab-width' in `org-mode' buffers to 8 columns.
Any `tab-width' value other than 8 will result in an error.

This should be set as late as possible, after all other
`org-mode-hook' functions added by packages and
configurations.  Hence the use of `after-change-major-mode-hook',
which runs at the very end of major-mode activation.

Intended for use as a local hook function on
`after-change-major-mode-hook' as added within `org-mode-hook'."

  ;; This check is necessary to handle, for example, `org-edit-src-code', which
  ;; clones the `org-mode' buffer and changes its major-mode.
  (when (derived-mode-p 'org-mode)
    (setq tab-width 8)))

(def-hook! +org-mode-enforce-tab-width-h ()
  'org-mode-hook
  "Add a local hook to control `tab-width' on `after-change-major-mode-hook'."
  (add-hook 'after-change-major-mode-hook #'+org-mode--local-set-tab-width-h 0 t))

;; Appearance: Typography & Fontification :ui:font:


(after! org
  (add-hook 'org-mode-hook #'prettify-symbols-mode)

  (setopt org-auto-align-tags nil
          org-tags-column 0
          org-agenda-tags-column 0)
  (setopt org-pretty-entities t
          org-pretty-entities-include-sub-superscripts nil)
  (setopt org-src-fontify-natively t)
  ;; TODO: show markers for bold and italic, hide everything else
  (setopt org-hide-emphasis-markers t)
  (setopt org-link-descriptive t)
  ;;  (setopt org-ellipsis " ⇢")            ; prefix is nbsp

  ;; Bring attention when point is on `org-ellipsis'.
  ;; FIXME: not correct
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

;; =org-modern= :ui:

;; + Package :: <https://github.com/minad/org-modern>


(package! org-modern
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (setopt org-modern-checkbox nil
          org-modern-internal-target nil
          org-modern-keyword nil
          org-modern-priority t
          org-modern-radio-target nil
          org-modern-star nil
          org-modern-tag t
          org-modern-timestamp nil
          org-modern-todo t
          org-modern-table nil)
  (setopt org-modern-list
          '((?- . "⁃")
            (?* . "•")
            (?+ . "◦")))

  (after! org
    (keymap-set org-mode-map "C-c t p" #'org-modern-mode))
  (after! org-agenda
    (keymap-set org-agenda-mode-map "C-c t p" #'org-modern-mode)))

;; =org-appear= :: display the underlying markup of stylized elements on focus :ui:

;; + Package :: <https://github.com/awth13/org-appear>

;; #+DOWNLOADED: https://github.com/awth13/org-appear/raw/master/demo.gif @ 2025-07-19 17:43:03
;; [[file:Features/2025-07-19_17-43-03_demo.gif]]



(package! org-appear
  (after! org
    (add-hook 'org-mode-hook #'org-appear-mode))

  (setopt org-appear-autoemphasis t
          org-appear-autolinks t
          org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-autokeywords t
          org-appear-inside-latex t)
  (setopt org-appear-delay 0.4)
  (setopt org-appear-trigger 'always))

;; Appearance: Indentation :ui:


(after! org
  (setopt org-indent-indentation-per-level 2))

;; Method № 1: Org-Indent-Mode

;; Enable =org-indent-mode= by default:


(after! org
  (setopt org-startup-indented t))



;; ~org-modern-indent~ provides support for using ~org-modern-mode~ in
;; combination with ~org-indent-mode~.


(package! (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  (after! org-modern
    ;; Adds extra indentation.
    (setopt org-modern-hide-stars nil)

    (add-hook 'org-mode-hook #'org-modern-indent-mode 90)))

;; Appearance: Display visual feedback pulse after actions  :ui:

;; - Source :: <https://github.com/protesilaos/dotfiles/blob/4d4e82fc63dd74971a7bf7895e4e0e24c3d446da/emacs/.emacs.d/prot-emacs-modules/prot-emacs-org.el#L112-L115>


(after! (org pulsar)
  (dolist (hook '( org-agenda-after-show-hook
                   org-follow-link-hook
                   org-cycle
                   org-shifttab))
    (add-hook hook #'pulsar-recenter-center)
    (add-hook hook #'pulsar-reveal-entry))

  (dolist (fn '( org-cycle
                   org-shifttab
                 ))
    (cl-pushnew fn pulsar-pulse-functions)))

;; =doct= :: a template engine for ~org-capture~

;; - Source code :: <https://github.com/progfolio/doct>


(package! doct
  (require 'doct))

;; =org-ql= :: a query-builder for ~org-mode~ elements

;; - Source code :: <https://github.com/alphapapa/org-ql>


(use-package org-ql
  ;; :ensure t
  )

;; =org-node= :: a lightweight intertwingler

;; + Package :: <https://github.com/meedstrom/org-node>


(package! org-node
  (setq! org-node-extra-id-dirs (list ceamx-agenda-dir)))

(after! org-node
  (add-to-list 'org-node-extra-id-dirs-exclude ceamx-note-journal-dir)

  (org-node-backlink-mode)
  (org-node-cache-mode))

;; =org-contrib= :: a library of unmaintained community packages

;; - Website :: <https://orgmode.org/worg/org-contrib/>


(use-package org-contrib
  ;; :ensure t
  :after (org)

  :init
  (require 'org-checklist)
  (require 'org-choose)
  (add-to-list 'org-modules 'org-checklist)
  (add-to-list 'org-modules 'org-choose))

;; Navigation & Refiling


(defvar ceamx-org-outline-search-max-level 5)

(after! org
  (setopt org-imenu-depth ceamx-org-outline-search-max-level))

(after! org-goto
  (setopt org-goto-interface 'outline-path-completion
          org-goto-max-level ceamx-org-outline-search-max-level))

(after! org-refile
  (setopt org-outline-path-complete-in-steps nil)

  (setopt org-refile-use-outline-path 'file)
  (setopt org-refile-allow-creating-parent-nodes 'confirm)
  (setopt org-refile-use-cache nil)

  (setopt org-refile-targets `((,ceamx-default-todo-file . (:level . 1))
                               ;; (org-agenda-files . (:maxlevel . 1))
                               (nil . (:maxlevel . ,ceamx-org-outline-search-max-level))))

  ;; TODO: how to accept any value of `:maxlevel'?
  (add-to-list 'safe-local-variable-values
      '(org-refile-targets (nil :maxlevel . 4)))
  (add-to-list 'safe-local-variable-values
      '(org-refile-targets (nil :maxlevel . 6))))

;; Customize the ~org-navigation-repeat-map~ repeat map :keybinds:repeat:


(after! org
  (define-keymap :keymap org-navigation-repeat-map
    ;;"C-b" #'org-backward-heading-same-level
    "b" '("⇐ back" . org-backward-heading-same-level)
    ;; "C-f" #'org-forward-heading-same-level
    "f" '("forward ⇒" . org-forward-heading-same-level)
    ;; "C-n" #'org-next-visible-heading
    "n" '("next visible ⇥" . org-next-visible-heading)
    ;; "C-p" #'org-previous-visible-heading
    "p" '("⇤ prev visible" . org-previous-visible-heading)
    ;; "C-u" #'org-up-heading
    "u" '("⇑ up" . org-up-heading)
    ;; "C-<" #'org-promote-subtree
    "<" '("⮲ promote" . org-promote-subtree)
    ;; "C->" #'org-demote-subtree
    ">" '("⮱ demote" . org-demote-subtree)))

;; Archiving


(after! org-archive
  (setopt org-archive-save-context-info
          '(time file category todo itags olpath ltags)))

;; Agenda


(use-feature! org-agenda
  :config
  (setq! org-agenda-tags-column 0)
  (setq! org-agenda-block-separator ?─)
  (setq! org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq! org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────"))

;; =org-super-agenda= :: enchanted groupings for Org-Agenda

;; The agenda groups are declared in [[id:815ead58-41ac-406b-b1fe-4d6c4c838617][Thee Something-Doing Flow → Org-Agenda groups]]


(package! org-super-agenda)

;; Literate programming


(after! org-src
  ;; Changing the indentation of source code is unhelpful and destructive.
  (setopt org-edit-src-content-indentation 0)

  (setopt org-edit-src-persistent-message nil)
  (setopt org-src-ask-before-returning-to-edit-buffer nil)
  (setopt org-src-preserve-indentation t)
  (setopt org-src-tab-acts-natively t)

  ;; TODO: current window when narrow/short frame, but otherwise reorganize-frame is good
  ;; (setopt org-src-window-setup 'other-window)
  (setopt org-src-window-setup 'current-window))

(after! org
  ;; Ensure common languages are loaded.
  (setopt org-babel-load-languages '((emacs-lisp . t)
                                     (shell . t)
                                     (sql . t))))

;; Org-Babel: Load other supported languages on-demand during execution

;; + source :: <https://github.com/Icy-Thought/emacs.d/blob/e9c75d87bf61c456b26332787cde27bdfc188830/config.org#org-babel-language-on-demand>


(after! ob-core
  (def-advice! +org-babel-load-language-on-demand-a (orig-fun &rest args)
    :around #'org-babel-execute-src-block
    "Load language if needed before executing a source block."
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      (apply orig-fun args))))

;; =org-download= :: support dragging-and-dropping images into Org buffers :media:network:package:

;; <https://github.com/abo-abo/org-download>


(package! org-download
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable))

;; =org-web-tools= :: view, capture, and archive webpages in org-mode :package:network:web:ATTACH:


(package! org-web-tools)

;; =org-sidebar= :: provide a sidebar for Org buffers :package:

;; <https://github.com/alphapapa/org-sidebar>


(package! org-sidebar)

;; =org-remark= :: annotate documents with Org-Mode


(package! org-remark)

;; =ox-gfm= :: org-export to GitHub Flavored Markdown (GFM) :package:


(package! ox-gfm
  (after! org
    (require 'ox-gfm))

  (add-to-list 'safe-local-variable-values
      '(eval add-hook 'after-save-hook #'org-gfm-export-to-markdown t t)))

;; =auto-tangle-mode= :: a minor-mode to automatically tangle Org files


(package! (auto-tangle-mode :host github :repo "progfolio/auto-tangle-mode.el")
  (after! minions
    (add-to-list 'minions-prominent-modes #'auto-tangle-mode)))

(provide 'ceamx-init-org)
;;; ceamx-init-org.el ends here
