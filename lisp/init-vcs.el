;;; init-vcs.el --- Git/VCS -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Configurations for git.

;;; Code:

(require 'ceamx-paths)
(require 'lib-common)

(use-feature! ediff
  :config
  ;; Keep the Ediff control panel in the same frame.
  (setopt ediff-window-setup-function #'ediff-setup-windows-plain))

(use-feature! vc
  :demand t
  :config
  (setopt vc-follow-symlinks t)

  ;; No need for all that other nonsense.
  (setopt vc-handled-backends '(Git))

  ;; NOTE: According to the documentation for `diff-hl', the diff algorithm
  ;; cannot be determined based on the user's global git config =diff.algorithm=
  ;; setting. The website source they linked to has disappeared with no archived
  ;; page available. So I have not verified this for certain.
  (setopt vc-git-diff-switches '("--histogram")))

;;;; `diff-hl' :: <https://github.com/dgutov/diff-hl>

;; <https://github.com/purcell/emacs.d/blob/master/lisp/init-vc.el>

;; NOTE: Fringe indicators will conflict with Flycheck.

(use-package diff-hl
  :commands (global-diff-hl-mode
             diff-hl-next-hunk
             diff-hl-previous-hunk)
  :autoload (diff-hl-magit-pre-refresh diff-hl-magit-post-refresh)
  :defines (diff-hl-mode-map)

  :init
  (add-hook 'ceamx-after-init-hook #'global-diff-hl-mode)

  ;; Display indicators in margins instead of fringes.
  ;; This will work in terminal sessions and also avoid the fringe conflict with
  ;; other indicators like Flycheck errors.
  (add-hook 'ceamx-after-init-hook #'diff-hl-margin-mode)

    ;; Support mouse click on indicator to show hunk.
  (when (display-graphic-p)
    (add-hook 'ceamx-after-init-hook #'diff-hl-show-hunk-mouse-mode)


  ;; Committing changes using a package other than `vc' requires integration.
  ;; <https://github.com/dgutov/diff-hl#integration>
  (use-feature! magit
    :init
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Enable `dired' integration.
  (use-feature! dired
    :config
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

  :config

;;;;; Keybindings

  (keymap-set diff-hl-mode-map "C-M-]" #'diff-hl-next-hunk)
  (keymap-set diff-hl-mode-map "C-M-[" #'diff-hl-previous-hunk))

;;;; git-commit :: <https://magit.vc/>

(use-package git-commit
  :after (transient)
  :commands (global-git-commit-mode)

  :init
  (global-git-commit-mode 1)

  (after! 'evil
    (declare-function evil-insert-state "evil")
    (add-hook 'git-commit-mode-hook #'evil-insert-state))

  (after! 'meow
    (declare-function meow-insert-mode "meow")
    (add-hook 'git-commit-mode-hook #'meow-insert-mode)))


;;;; git-timemachine :: <https://codeberg.org/pidu/git-timemachine>

(use-package git-timemachine
  :defer t
  :commands (git-timemachine
              git-timemachine-show-previous-revision
              git-timemachine-show-next-revision
              git-timemachine-blame
              git-timemachine-show-commit)

  :init
  (keymap-global-set "C-x v t" #'git-timemachine)

  ;; XXX: broken, see `ceamx/git-timemachine-dispatch'
  ;; (add-hook 'git-timemachine-mode-hook #'ceamx/git-timemachine-dispatch)

  :config

  (keys! git-timemachine-mode-map
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

;;;; `browse-at-remote' :: <https://github.com/rmuslimov/browse-at-remote>

(use-package browse-at-remote
  :commands (browse-at-remote)
  :init
  (keymap-set vc-prefix-map "o" #'browse-at-remote))

;;;; `consult-gh' :: <https://github.com/armindarvish/consult-gh>

;; TODO: look through readme and review settings, add config as desired
;;
;; [2023-12-28]: I still haven't gotten it to work at all. Searches come up with
;; nothing and the tool keeps trying to write to the `gh' CLI's config file,
;; which is fortunately read-only in the Nix store.
;;
;; I really wish this tool was usuable but it's not surprising that GitHub would
;; change a bunch of API stuff, breaking integrations at any time.

;;
;; NOTE: The double-dashed option names are NOT private, despite the Elisp
;; naming convention. Try to ignore it.
;; (use-package consult-gh
;;   ;; FIXME: :elpaca (consult-gh :host github :repo "armindarvish/consult-gh")
;;   :defines ( consult-gh-default-clone-directory
;;              consult-gh-default-orgs-list)
;;   :init
;;   (dolist (owner '("montchr" "seadome"))
;;     (add-to-list 'consult-gh-default-orgs-list owner))

;;   ;; TODO: use/lose
;;   ;; use "gh org list" to get a list of all your organizations and adds them to default list
;;   ;; (setq consult-gh-default-orgs-list (append consult-gh-default-orgs-list (remove "" (split-string (or (consult-gh--command-to-string "org" "list") "") "\n"))))

;;   ;; Set the default folder for cloning repositories. By default Consult-GH will
;;   ;; confirm this before cloning.
;;   (setopt consult-gh-default-clone-directory
;;     (concat ceamx-projects-dir "repos")))

(provide 'init-vcs)
;;; init-vcs.el ends here
