;; -*- lexical-binding: t;  -*-

(require 'ceamx-paths)
(require 'ceamx-lib)

;; Set up ~ediff~
;; :PROPERTIES:
;; :ID:       81a5a03d-6bb7-401f-a9cb-54ff581b0f3d
;; :END:


(setopt ediff-keep-variants nil
        ediff-make-buffers-readonly-at-startup nil
        ediff-merge-revisions-with-ancestor t
        ediff-show-clashes-only t
        ;; Keep the ~ediff~ control panel in the same frame.
        ediff-window-setup-function #'ediff-setup-windows-plain)

;; Set up ~diff-mode~


(setopt diff-default-read-only t)
(setopt diff-advance-after-apply-hunk t)
(setopt diff-update-on-the-fly t)
(setopt diff-refine 'font-lock)
(setopt diff-font-lock-prettify t
        diff-font-lock-syntax 'hunk-also)

;; Set up version control integration with ~vc-mode~
;; :PROPERTIES:
;; :ID:       4e3a33c2-f9ac-4947-bbe2-837b91d5cb19
;; :END:


;; Version control support is essential as soon as possible.
(require 'vc)

(setopt vc-follow-symlinks t)
(setopt vc-handled-backends '(Git))

;; NOTE: According to the documentation for ~diff-hl~, the diff
;; algorithm cannot be determined based on the user's global git
;; config =diff.algorithm= setting.  The website source they linked to
;; has disappeared with no archived page available.  So I have not
;; verified this for certain.
(setopt vc-git-diff-switches '("--histogram"))

;; Set up project management with =project.el=


(progn
  (define-keymap :keymap (current-global-map)
    "C-x p ." #'project-dired
    "C-x p RET" #'project-dired
    "C-x p DEL" #'project-forget-project)
  (setopt project-vc-extra-root-markers '(".project"))
  (setopt project-key-prompt-style t))

;; Rudimentary jujutsu (=jj=) VCS support

;; The =vc-jj= package currently has some issues (see the entry on that
;; package below).  In lieu of full-fledged support, let’s try to make the
;; =jj= experience in Emacs somewhat more bearable.


(after! lsp-mode
  (cl-pushnew "[/\\\\]\\.jj\\'" lsp-file-watch-ignored-directories))

;; =diff-hl= :: display version control status indicators in margins
;; :PROPERTIES:
;; :ID:       6a8b57d7-091a-44a6-bcf1-564f45d9bc7e
;; :END:

;; - Website :: <https://github.com/dgutov/diff-hl>

;; NOTE: Fringe indicators will conflict with Flycheck.


(package! diff-hl
  (add-hook 'ceamx-after-init-hook #'global-diff-hl-mode)

  ;; Display indicators in margins instead of fringes.  This will work
  ;; in terminal sessions and also avoid the fringe conflict with
  ;; other indicators like Flycheck errors.
  ;; (add-hook 'ceamx-after-init-hook #'diff-hl-margin-mode)

  ;; Support mouse click on indicator to show hunk.
  (when (display-graphic-p)
    (add-hook 'ceamx-after-init-hook #'diff-hl-show-hunk-mouse-mode))

  ;; Committing changes using a package other than `vc' requires integration.
  ;; <https://github.com/dgutov/diff-hl#integration>
  (after! magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (after! dired
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

;; =difftastic= :: Integration with =difftastic=
;; :PROPERTIES:
;; :ID:       362df672-d6c5-41e4-990e-c8d7ef519a62
;; :END:

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
;; :PROPERTIES:
;; :ID:       034124e8-0166-499f-b0f2-6c298e475d63
;; :END:

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
;; :PROPERTIES:
;; :ID:       5a436e2b-7c80-4055-acdc-b2ef8e640f83
;; :END:


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
  (setopt magit-wip-mode t)

  (setopt magit-diff-refine-hunk t)
  ;; Avoid side-effects (e.g. formatting-on-save)
  (setopt magit-save-repository-buffers nil)
  (setopt magit-process-finish-apply-ansi-colors t)

  (setopt magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setopt magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)

  (when (locate-library "nerd-icons")
    (setopt magit-format-file-function #'magit-format-file-nerd-icons))

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

;; Keep ~magit-section~ sections at the top of the window
;; :PROPERTIES:
;; :ID:       19010ecb-b1e0-4168-a554-2cad3e410bfe
;; :END:

;; - Reference :: <https://emacs.stackexchange.com/questions/3380/how-to-scroll-up-when-expanding-a-section-in-magit-status#comment4819_3383>


(after! magit
  (remove-hook 'magit-section-movement-hook 'magit-hunk-set-window-start)
  (add-hook 'magit-section-movement-hook #'magit-section-set-window-start))

;; Show ~magit-process~ output popup after N seconds :popups:

;; This is pretty important when a project has =pre-commit= or =commit-msg=
;; hooks that are wont to fail.  I also find it helpful for getting
;; insight into long-running =post-receive= hooks (and similar).


(after! magit
  (setopt magit-process-popup-time 3))

;; =magit-todo= :: Display codetag comment reminders in ~magit-status~
;; :PROPERTIES:
;; :ID:       567769b0-8153-4f97-9c85-6f0b1e401f3f
;; :END:

;; - Docs :: <https://github.com/alphapapa/magit-todos/blob/master/README.org>


(package! magit-todos
  (after! magit
    (require 'magit-todos)
    (magit-todos-mode 1)))

;; =magit-repos=
;; :PROPERTIES:
;; :ID:       2c0ff466-ba79-4208-8210-af6ac2ffaa06
;; :END:


(after! magit
  (require 'magit-repos)
  (setopt magit-repository-directories
          `((,(file-name-concat ceamx-projects-dir "work") . 2)
            (,(file-name-concat ceamx-projects-dir "sources") . 1)
            (,(file-name-concat ceamx-projects-dir "contrib") . 2))))

;; =forge= :: interact with online forges through magit
;; :PROPERTIES:
;; :ID:       c77a863e-56bd-437f-aeb8-303b0183c9fa
;; :END:

;; *Warning*: /development versions of ~forge~ may expect a different ~magit~
;; version!/


(package! (forge))

(after! magit
  (require 'forge)

  (keymap-set vc-prefix-map "o" #'forge-browse-commit))

(provide 'ceamx-init-vcs)
;;; ceamx-init-vcs.el ends here
