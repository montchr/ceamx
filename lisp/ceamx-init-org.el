;; -*- lexical-binding: t -*-

(require 'ceamx-lib)



;; The value of ~org-directory~ will be used as a default destination for
;; new notes, especially as they relate to tasks and agendas.  For that
;; reason, use the ~ceamx-agenda-dir~.


(defvar org-directory ceamx-agenda-dir)

;; TODO: I would prefer to check for the directory's existence
;; explicitly -- this feels strange at the top-level.  Maybe move this
;; to the section where `ceamx-agenda-dir' is defined.
(f-mkdir-full-path org-directory)

(setopt org-agenda-files ceamx-default-agenda-files)

;; Baseline customizations


(after! org
  (setopt org-blank-before-new-entry '((heading . auto)
                                       (plain-list-item . nil)))

  ;;
  ;; Links

  (setopt org-link-context-for-files t)
  (setopt org-link-keep-stored-after-insertion nil)
  (setopt org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;;
  ;; Editing

  (setopt org-special-ctrl-a/e t
          org-special-ctrl-k t
          org-special-ctrl-o t
          org-ctrl-k-protect-subtree t)
  (setopt org-reverse-note-order nil)
  (setopt org-list-use-circular-motion t)

  (setopt org-M-RET-may-split-line '((default . nil))
          ;; Invoke `org-insert-heading-respect-content' directly with
          ;; [C-<return>], but let `org-insert-heading' ([M-<return>])
          ;; insert a heading wherever.
          org-insert-heading-respect-content nil)
  (keymap-set org-mode-map "C-M-<return>" #'org-insert-subheading)
  (keymap-set org-mode-map "C-M-S-<return>" #'org-insert-todo-subheading)

  ;;
  ;; Tags

  (setopt org-auto-align-tags nil
          org-tags-column 0)

  ;;
  ;; Folding

  (setopt org-cycle-emulate-tab t)
  (setopt org-startup-folded 'content)
  (setopt org-fold-catch-invisible-edits 'show-and-error)

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

;; Appearance

;; + Package :: <https://github.com/minad/org-modern>
;; + Package :: <https://github.com/awth13/org-appear>


(package! org-modern
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (after! org
    (keymap-set org-mode-map "C-c t p" #'org-modern-mode))
  (after! org-agenda
    (keymap-set org-agenda-mode-map "C-c t p" #'org-modern-mode)))

(package! org-appear
  (add-hook 'org-mode-hook #'org-appear-mode))

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
  (setopt org-ellipsis " ⇢")            ; prefix is nbsp

  ;; Bring attention when point is on `org-ellipsis'.
  ;; FIXME: not correct
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(after! org-modern
  (setopt org-modern-star nil))

(after! org-appear
  (setopt org-appear-autoemphasis t
          org-appear-autolinks t
          org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-autokeywords t
          org-appear-inside-latex t)
  (setopt org-appear-delay 0.25)
  (setopt org-appear-trigger 'always))

;; Appearance: Display visual feedback after actions

;; - Source :: <https://github.com/protesilaos/dotfiles/blob/4d4e82fc63dd74971a7bf7895e4e0e24c3d446da/emacs/.emacs.d/prot-emacs-modules/prot-emacs-org.el#L112-L115>


(after! (org pulsar)
  (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-center)
    (add-hook hook #'pulsar-reveal-entry)))

;; ~doct~ :: a template engine for ~org-capture~

;; - Source code :: <https://github.com/progfolio/doct>


(use-package doct
  ;; :ensure t
  :demand t
  :functions (doct))

;; ~org-ql~ :: a query-builder for ~org-mode~ elements

;; - Source code :: <https://github.com/alphapapa/org-ql>


(use-package org-ql
  ;; :ensure t
  )

;; ~org-contrib~ :: a library of unmaintained community packages

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

  (setopt org-refile-targets `((,ceamx-default-todo-file . (:level . 2))
                               ;; (org-agenda-files . (:maxlevel . 1))
                               (nil . (:maxlevel . ,ceamx-org-outline-search-max-level))))

  ;; TODO: how to accept any value of `:maxlevel'?
  (add-to-list 'safe-local-variable-values
      '(org-refile-targets (nil :maxlevel . 4)))
  (add-to-list 'safe-local-variable-values
      '(org-refile-targets (nil :maxlevel . 6))))

;; Customize the ~org-navigation-repeat-map~ :keybinds:

;; I find myself accidentally exiting out of this repeat map immediately
;; because I instinctively want to continue pressing the final key in the
;; original command that triggered the repeat map.  For example, if I
;; press [C-c C-n] for ~org-next-visible-heading~, I want to continue
;; pressing [C-n] to repeat the command.  I thought there was a setting
;; for this behavior, but neither ~repeat-keep-prefix~ or ~repeat-check-key~
;; seem to have an effect.


(after! org
  (define-keymap :keymap org-navigation-repeat-map
    "C-b" #'org-backward-heading-same-level
    "C-f" #'org-forward-heading-same-level
    "C-n" #'org-next-visible-heading
    "C-p" #'org-previous-visible-heading
    "C-u" #'org-up-heading))

;; Archiving


(after! org-archive
  (setopt org-archive-save-context-info
          '(time file category todo itags olpath ltags)))

;; Capture

;; Define the Org-Capture templates:


(after! (org-capture doct)
  (setopt org-capture-templates
          (doct `(("Inbox"
                   :keys "t"
                   ;; TODO: make sure this icon spec is up to date with 2024
                   :icon ("checklist" :set "octicon" :color "green")
                   ;; TODO: should this be evaled/expanded?
                   :file ceamx-default-capture-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))))))

;; Agenda


(after! org-agenda
  (setopt org-agenda-tags-column 0)
  (setopt org-agenda-block-separator ?─)
  (setopt org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setopt org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────"))

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

;; ~org-download~ :: support dragging-and-dropping images into Org buffers :media:network:package:

;; <https://github.com/abo-abo/org-download>


(package! org-download
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable))

;; ~org-web-tools~ :: view, capture, and archive webpages in org-mode :package:network:web:


(package! org-web-tools
  (keymap-set org-mode-map "C-c i l" #'org-web-tools-insert-link-for-url))

;; ~org-sidebar~ :: provide a sidebar for Org buffers :package:

;; <https://github.com/alphapapa/org-sidebar>


(package! org-sidebar)

;; ~org-bookmark-heading~ :: Support heading bookmarks :bookmarks:package:


(package! org-bookmark-heading
  (after! org
    (require 'org-bookmark-heading)))

;; TODO ~org-remark~


(use-package org-remark
  ;; :ensure t
  )

;; =ox-gfm= :: org-export to GitHub Flavored Markdown (GFM) :package:


(package! ox-gfm
  (after! org
    (require 'ox-gfm))

  (add-to-list 'safe-local-variable-values
      '(eval add-hook 'after-save-hook #'org-gfm-export-to-markdown t t)))

;; ~auto-tangle-mode~ :: a minor-mode to automatically tangle Org files


(use-package auto-tangle-mode
  :ensure (auto-tangle-mode :host github :repo "progfolio/auto-tangle-mode.el")
  :commands (auto-tangle-mode))

(provide 'ceamx-init-org)
;;; ceamx-init-org.el ends here
