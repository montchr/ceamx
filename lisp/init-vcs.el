;;; init-vcs.el --- Version control support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
;;; Code:

(require 'ceamx-paths)
(require 'ceamx-lib)
(setopt ediff-window-setup-function #'ediff-setup-windows-plain)
;; Version control support is essential as soon as possible.
(require 'vc)

(setopt vc-follow-symlinks t)

;; No need for all that other nonsense.
(setopt vc-handled-backends '(Git))

;; NOTE: According to the documentation for ~diff-hl~, the diff algorithm
;; cannot be determined based on the user's global git config =diff.algorithm=
;; setting. The website source they linked to has disappeared with no archived
;; page available. So I have not verified this for certain.
(setopt vc-git-diff-switches '("--histogram"))
(package! diff-hl
  (add-hook 'ceamx-after-init-hook #'global-diff-hl-mode)

  ;; Display indicators in margins instead of fringes.
  ;; This will work in terminal sessions and also avoid the fringe conflict with
  ;; other indicators like Flycheck errors.
  (add-hook 'ceamx-after-init-hook #'diff-hl-margin-mode)

  ;; Support mouse click on indicator to show hunk.
  (when (display-graphic-p)
    (add-hook 'ceamx-after-init-hook #'diff-hl-show-hunk-mouse-mode)))

;; Committing changes using a package other than `vc' requires integration.
;; <https://github.com/dgutov/diff-hl#integration>
(after! (diff-hl magit)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Enable `dired' integration.
(after! (diff-hl dired)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))
(package! git-commit
  (require 'git-commit)
  (add-hook 'ceamx-after-init-hook #'global-git-commit-mode))
(after! (git-commit evil)
  (declare-function evil-insert-state "evil")
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

(after! (git-commit meow)
  (declare-function meow-insert-mode "meow")
  (add-hook 'git-commit-mode-hook #'meow-insert-mode))
(package! git-modes)
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
(package! browse-at-remote
  (keymap-set vc-prefix-map "o" #'browse-at-remote))
(with-eval-after-load 'transient
  (defvar transient-map)
  (declare-function transient-quit-one "transient")

  ;; Always close transient with ESC
  (keymap-set transient-map "ESC" #'transient-quit-one))
(package! magit)
(with-eval-after-load 'magit
  (defvar magit-mode-map)
  (defvar magit-status-mode-map)

  (declare-function magit-discard "magit-apply")
  (declare-function magit-dispatch "magit")
  (declare-function magit-display-buffer-fullframe-status-v1 "magit-mode")
  (declare-function magit-file-dispatch "magit-files")
  (declare-function magit-restore-window-configuration "magit-mode")
  (declare-function magit-revert "magit-sequence")
  (declare-function magit-status "magit-status")

  (setopt magit-diff-refine-hunk t)     ; show granular diffs in selected hunk
  (setopt magit-save-repository-buffers nil) ; avoid side-effects (e.g. auto-format)
  ;; (setopt magit-revision-insert-related-refs nil) ; parent/related refs: rarely useful
  (setopt magit-process-finish-apply-ansi-colors t) ; render ANSI colors in process output

  (setopt magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(after! magit
  (remove-hook 'magit-section-movement-hook 'magit-hunk-set-window-start)
  (add-hook 'magit-section-movement-hook #'magit-section-set-window-start))
(package! forge
  (after! magit
    (require 'forge)))
(define-keymap :keymap (current-global-map)
  "C-c g"    #'magit-dispatch
  "C-c G"    #'magit-file-dispatch

  "C-x g"    #'magit-status
  "C-x M-g"  #'magit-dispatch)

(after! magit
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

(provide 'init-vcs)
;;; init-vcs.el ends here
