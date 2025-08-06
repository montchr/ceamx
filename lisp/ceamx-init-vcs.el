;; -*- lexical-binding: t;  -*-

(require 'ceamx-paths)
(require 'ceamx-lib)

;; Set up ~ediff~


(progn
  (setq! ediff-keep-variants nil
         ediff-make-buffers-readonly-at-startup nil
         ediff-merge-revisions-with-ancestor t
         ediff-show-clashes-only t
         ;; Keep the ~ediff~ control panel in the same frame.
         ediff-window-setup-function #'ediff-setup-windows-plain))

;; Set up ~diff-mode~


(progn
  (setq! diff-default-read-only t)
  (setq! diff-advance-after-apply-hunk t)
  (setq! diff-update-on-the-fly t)
  (setq! diff-refine 'font-lock)
  (setq! diff-font-lock-prettify t
         diff-font-lock-syntax 'hunk-also))

;; Set up version control integration with ~vc~


(progn
  ;; Version control support is essential as soon as possible.
  (require 'vc)

  (setq! vc-follow-symlinks t)
  (setq! vc-handled-backends '(Git))

  (define-keymap :keymap (current-global-map)
    "C-x v B" #'vc-annotate
    "C-x v d" #'vc-diff                 ; orig. `vc-dir'
    "C-x v e" #'vc-ediff
    "C-x v G" #'vc-log-search
    "C-x v ." #'vc-dir-root
    "C-x v RET" #'vc-dir-root))



;; Customize the =vc-dir= feature:


(progn
  (setq! vc-dir-save-some-buffers-on-revert t) ; Emacs 31+

  (after! vc-dir
    (define-keymap :keymap vc-dir-mode-map
      "d" #'vc-diff
      "o" #'vc-dir-find-file-other-window
      "O" #'vc-log-outgoing)))



;; Customize Git-specific features:


(progn
  (setq! vc-git-diff-switches
         '("--patch-with-stat"
           ;; NOTE: According to the documentation for ~diff-hl~, the
           ;; diff algorithm cannot be determined based on the user's
           ;; global git config =diff.algorithm= setting.  The website
           ;; source they linked to has disappeared with no archived
           ;; page available.  So I have not verified this for certain.
           "--histogram"))
  (setq! vc-git-log-switches '("--stat"))
  (setq! vc-git-print-log-follow t)
  (setq! vc-git-revision-complete-only-branches t)

  ;; <https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html>
  (setq! vc-git-log-edit-summary-target-len 50
         vc-git-log-edit-summary-max-len 72)

  (after! vc-git
    (define-keymap :keymap vc-git-stash-shared-map
      "A" #'vc-git-stash-apply-at-point
      "P" #'vc-git-stash-pop-at-point
      "z" #'vc-git-stash
      "Z" #'vc-git-stash-snapshot)))



;; Customize the behavior of =vc-annotate=:


(progn
  (setq! vc-annotate-display-mode 'scale)

  (after! vc-annotate
    (define-keymap :keymap vc-annotate-mode-map
      "RET" #'vc-annotate-find-revision-at-line
      "C-c C-c" #'vc-annotate-goto-line
      "M-q" #'vc-annotate-toggle-annotation-visibility)))



;; Customize the behavior of =log-edit=:


(progn
  ;; FIXME: figure out why this does not always get loaded and why
  ;; `log-edit-mode' is sometimes unavailable
  (require 'log-edit)

  (setq! log-edit-confirm 'changed)
  (setq! log-edit-keep-buffer nil)
  (setq! log-edit-require-final-newline t)
  (setq! log-edit-setup-add-author t)

  (after! log-edit
    (define-keymap :keymap log-edit-mode-map
      "M-r" nil                    ; prefer `consult-history'
      "M-s" nil)))

(after! log-view
  (define-keymap :keymap log-view-mode-map
    "<return>" #'log-view-find-revision
    "<tab>" #'log-view-toggle-entry-display
    "f" #'vc-log-incoming
    "F" #'vc-update
    "o" #'vc-log-outgoing
    "P" #'vc-push
    "s" #'vc-log-search))

;; Set up project management with =project.el=


(progn
  (define-keymap :keymap (current-global-map)
    "C-x p ." #'project-dired
    "C-x p RET" #'project-dired
    "C-x p DEL" #'project-forget-project)
  (setq! project-vc-extra-root-markers '(".project"))
  (setq! project-key-prompt-style t))

;; Rudimentary jujutsu (=jj=) VCS support


(progn
  (cl-pushnew ".jj" project-vc-extra-root-markers)

  (after! lsp-mode
    (cl-pushnew "[/\\\\]\\.jj\\'" lsp-file-watch-ignored-directories)))

;; =vc-jj= :: Support for the Jujutsu version control system :package:

;; + Package :: https://codeberg.org/emacs-jj-vc/vc-jj.el
;; + Bug :: <https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/82>

;; Currently it seems to break ~dired-do-rename~.  I’ve opened a
;; bug report on the project’s issue tracker:

;; <https://codeberg.org/emacs-jj-vc/vc-jj.el/issues/82>

;; Note that this package requires Jujutsu to be configurated for
;; compatibility with Git diff formatting style.  This can be done globally
;; in =$XDG_CONFIG_HOME/jj/config.toml= or in the repository-specific
;; configuration with =jj config edit --repo=.  Here is an example of the
;; TOML configuration required:

;; #+begin_example toml
;; [ui]
;; conflict-marker-style = "git"
;; diff-formatter = ":git"
;; #+end_example

;; Now here is the package setup:


(package! vc-jj
  (require 'vc-jj))

;; =diff-hl= :: display version control status indicators in margins

;; - Website :: <https://github.com/dgutov/diff-hl>

;; NOTE: Fringe indicators will conflict with Flycheck.


(package! diff-hl
  (global-diff-hl-mode 1)

  (if (display-graphic-p)
      (progn
        (diff-hl-show-hunk-mouse-mode 1))
    ;; Display indicators in margins instead of fringes.  This will work
    ;; in terminal sessions and also avoid the fringe conflict with
    ;; other indicators like Flycheck errors.
    (diff-hl-margin-mode 1))

  ;; Committing changes using a package other than `vc' requires integration.
  ;; <https://github.com/dgutov/diff-hl#integration>
  (after! magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (after! dired
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

;; =difftastic= :: Integration with =difftastic=

;; - Package :: https://github.com/pkryger/difftastic.el


(package! difftastic
  ;; Automatically configure keybindings
  (difftastic-bindings-mode))

;; =git-modes= :: Major-modes for git-related files

;; - website :: <https://github.com/magit/git-modes>


(package! git-modes)

;; =git-timemachine= :: Interactively explore files' git histories

;; <https://codeberg.org/pidu/git-timemachine>


(package! git-timemachine
  (keymap-global-set "C-x v t" #'git-timemachine))

(after! git-timemachine
  ;; XXX: broken, see `ceamx/git-timemachine-dispatch'
  ;; (add-hook 'git-timemachine-mode-hook #'ceamx/git-timemachine-dispatch)

  (define-keymap :keymap git-timemachine-mode-map
    "M-p" #'git-timemachine-show-previous-revision
    "M-n" #'git-timemachine-show-next-revision
    "M-b" #'git-timemachine-blame
    "M-c" #'git-timemachine-show-commit)

  ;; FIXME: like `ceamx/window-dispatch', this breaks because the commands
  ;; should be run in the original buffer/window
  (transient-define-prefix ceamx/git-timemachine-dispatch ()
    "Transient menu for `git-timemachine-mode'."
    ;; :transient-suffix 'transient--do-stack
    [["Navigation"
      ("p" "previous revision" git-timemachine-show-previous-revision :transient t)
      ("n" "next revision" git-timemachine-show-next-revision :transient t)]
     ["Display"
      ("b" "blame" git-timemachine-blame)
      ("c" "commit" git-timemachine-show-commit )]
     [""
      ("q" "quit" git-timemachine-quit :transient nil)]])

  (declare-function git-timemachine--show-minibuffer-details "git-timemachine")

  ;; via <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/modules/emacs/vc/config.el#L76C1-L90C47>
  (def-advice! +git-timemachine--details-in-header-line-a (revision)
    :override #'git-timemachine--show-minibuffer-details
    "Show REVISION details in the header-line instead of the minibuffer."
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative)))))

;; =git-commit-ts-mode= :: A major-mode for editing Git commit messages

;; + Package :: <https://github.com/danilshvalov/git-commit-ts-mode/>

;; This mode requires the =tree-sitter-gitcommit= Tree-Sitter language
;; grammar installed.  As of [2025-02-23 Sun], it is not currently
;; available in Nixpkgs.  For that reason, if the =gitcommit= grammar is
;; not available, it will be installed imperatively.


(package! (git-commit-ts-mode
           :host github
           :repo "danilshvalov/git-commit-ts-mode")

  (pushnew! treesit-language-source-alist
            '(gitcommit . ("https://github.com/gbprod/tree-sitter-gitcommit")))
  (unless (treesit-language-available-p 'gitcommit)
    (treesit-install-language-grammar 'gitcommit))

  (when (treesit-language-available-p 'gitcommit)
    (after! git-commit
      (setopt git-commit-major-mode #'git-commit-ts-mode))
    (after! git-commit-ts-mode
      (setopt git-commit-ts-max-message-size 72))))

;; =magit= :: the git experience


(package! magit
  (define-keymap :keymap (current-global-map)
    "C-c g" #'magit-file-dispatch
    "C-x g" #'magit-status
    "C-c G" #'magit-dispatch
    "C-x M-g" #'magit-dispatch))

(after! magit
  ;; Save work-in-progress refs before some potentially-risky actions
  ;; where data loss is a possibility.  Note that this may cause a
  ;; performance hit.
  (setq! magit-wip-mode t)

  (setq! magit-diff-refine-hunk t)
  ;; Avoid side-effects (e.g. formatting-on-save)
  (setq! magit-save-repository-buffers nil)
  (setq! magit-process-finish-apply-ansi-colors t)

  (setq! magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setq! magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)

  (when (fboundp #'nerd-icons-insert)
    (setq! magit-format-file-function #'magit-format-file-nerd-icons))

  (define-keymap :keymap magit-status-mode-map
    "_" #'magit-revert
    ;; "V" nil
    "x" #'magit-discard)

  (transient-append-suffix 'magit-commit "-n"
    '("-S" "Disable GPG signing" "--no-gpg"))

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;; FIXME Keep ~magit-section~ sections at the top of the window

;; - Reference :: <https://emacs.stackexchange.com/questions/3380/how-to-scroll-up-when-expanding-a-section-in-magit-status#comment4819_3383>

;; This is not actually working?  Or at least, the behavior that bugs me
;; (which is…?) is still present.


(after! magit
  (remove-hook 'magit-section-movement-hook 'magit-hunk-set-window-start)
  (add-hook 'magit-section-movement-hook #'magit-section-set-window-start))

;; Show ~magit-process~ output popup after N seconds :popups:

;; This is pretty important when a project has =pre-commit= or =commit-msg=
;; hooks that are wont to fail.  I also find it helpful for getting
;; insight into long-running =post-receive= hooks (and similar).


(setq! magit-process-popup-time 3)

;; Configure the Git commit message mode


(dolist (mode '(git-commit-mode git-commit-ts-mode))
  (after! mode
    (cl-pushnew mode ceamx-lang-typo-mode-excluded-modes)))

;; =magit-todo= :: Display codetag comment reminders in ~magit-status~

;; - Docs :: <https://github.com/alphapapa/magit-todos/blob/master/README.org>


(package! magit-todos
  (after! magit
    (require 'magit-todos)
    (magit-todos-mode 1)))

;; =magit-repos=


(after! magit
  (require 'magit-repos)
  (setq! magit-repository-directories
          `((,(file-name-concat ceamx-projects-dir "work") . 2)
            (,(file-name-concat ceamx-projects-dir "sources") . 1)
            (,(file-name-concat ceamx-projects-dir "contrib") . 2))))

;; =forge= :: interact with online forges through magit

;; *Warning*: /development versions of ~forge~ may expect a different ~magit~
;; version!/


(package! forge)

(after! magit
  (require 'forge)

  (keymap-set vc-prefix-map "o" #'forge-browse-commit))

(provide 'ceamx-init-vcs)
;;; ceamx-init-vcs.el ends here
