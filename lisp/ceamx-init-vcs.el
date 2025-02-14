;; -*- lexical-binding: t;  -*-

(require 'ceamx-paths)
(require 'ceamx-lib)

;; Set up ~ediff~


(setup ediff
  (:autoload #'ediff-buffers
             #'ediff-files
             #'ediff-buffers3
             #'ediff-files3)
  (setopt ediff-keep-variants nil
          ediff-make-buffers-readonly-at-startup nil
          ediff-merge-revisions-with-ancestor t
          ediff-show-clashes-only t
          ;; Keep the ~ediff~ control panel in the same frame.
          ediff-window-setup-function #'ediff-setup-windows-plain))

;; Set up ~diff-mode~


(setup diff-mode
  (setopt diff-default-read-only t)
  (setopt diff-advance-after-apply-hunk t)
  (setopt diff-update-on-the-fly t)
  (setopt diff-refine 'font-lock)
  (setopt diff-font-lock-prettify t
          diff-font-lock-syntax 'hunk-also))

;; Set up version control integration with ~vc-mode~


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


(setup project
  (:global "C-x p ." #'project-dired
           "C-x p RET" #'project-dired
           "C-x p DEL" #'project-forget-project)
  (setopt project-vc-extra-root-markers '(".project"))
  (setopt project-key-prompt-style t))

;; =diff-hl= :: display version control status indicators in margins

;; - Website :: <https://github.com/dgutov/diff-hl>

;; NOTE: Fringe indicators will conflict with Flycheck.


(package! diff-hl
  (add-hook 'ceamx-after-init-hook #'global-diff-hl-mode)

  ;; Display indicators in margins instead of fringes.  This will work
  ;; in terminal sessions and also avoid the fringe conflict with
  ;; other indicators like Flycheck errors.
  (add-hook 'ceamx-after-init-hook #'diff-hl-margin-mode)

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

;; =git-modes= :: major modes for git-related files

;; - website :: <https://github.com/magit/git-modes>


(package! git-modes)

;; =git-timemachine= :: interactively explore files' git histories

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

;; =magit= :: the git experience
;; :PROPERTIES:
;; :ID:       5a436e2b-7c80-4055-acdc-b2ef8e640f83
;; :END:


(package! magit
  (define-keymap :keymap (current-global-map)
    "C-c g" #'magit-file-dispatch
    "C-c G" #'magit-dispatch
    "C-x g" #'magit-status
    "C-x M-g" #'magit-dispatch))

(after! magit
  (setopt magit-diff-refine-hunk t)
  ;; Avoid side-effects (e.g. formatting-on-save)
  (setopt magit-save-repository-buffers nil)
  (setopt magit-process-finish-apply-ansi-colors t)

  (setopt magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setopt magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)

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

;; =alphapapa/magit-todo= :: display codetag comment reminders in ~magit-status~

;; - Docs :: <https://github.com/alphapapa/magit-todos/blob/master/README.org>


(package! magit-todos
  (after! magit
    (require 'magit-todos)
    (magit-todos-mode 1)))

;; =magit-repos=


(setup magit-repos
  ;; FIXME: Make sure feature is available
  ;;  (:load-after magit)
  (:autoload #'magit-list-repositories)
  (setopt magit-repository-directories
          `((,(file-name-concat ceamx-projects-dir "work") . 2)
            (,(file-name-concat ceamx-projects-dir "sources") . 1)
            (,(file-name-concat ceamx-projects-dir "contrib") . 2))))

;; DISABLED =forge= :: interact with online forges through magit

;; *Warning*: /development versions of ~forge~ may expect a different ~magit~
;; version!/ Make sure to pin ~forge~ to a stable tag.  And then, make sure
;; ~magit~ is pinned to a stable tag.  And then, make sure every other
;; Magit dependency is pinned...


;; (package! (forge :tag "v0.4.6"))

;; (after! magit
;;   (require 'forge)

;;   ;; FIXME: not yet released
;;   ;; (keymap-set vc-prefix-map "o" #'forge-browse-commit)

;;   )

(provide 'ceamx-init-vcs)
;;; ceamx-init-vcs.el ends here
