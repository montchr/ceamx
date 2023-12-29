;;; init-vcs.el --- Git/VCS -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

;;  Configurations for git + magit.
;;  And other version control systems, if they exist...

;;; Code:

(require 'ceamx-paths)

(setopt vc-follow-symlinks t)

(after! [evil]
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

;;; <https://github.com/dgutov/diff-hl>
(use-package diff-hl
  :after (vc-mode)
  :commands (global-diff-hl-mode)
  :config
  (global-diff-hl-mode +1))

;;
;;; Magit -- <https://magit.vc/>
;;

(autoload '+magit-display-buffer-fn "lib-magit")
(autoload '+magit-mark-stale-buffers-h "lib-magit" "Revert all visible buffers and mark buried buffers as stale.")
(autoload '+magit-revert-buffer-maybe-h "lib-magit" "Update `vc' and `git-gutter' if out of date.")
(autoload '+magit/quit "lib-magit" "Bury the current magit buffer. <...>" t)
(autoload '+magit/quit-all "lib-magit" "Kill all magit buffers for the current repository." t)

(use-package magit
  :defer t

  :config
  ;; TODO: is this still necessary?
  (after! [evil-collection]
    (evil-collection-init 'magit))

  (setopt magit-diff-refine-hunk t)     ; show granular diffs in selected hunk
  (setopt magit-save-repository-buffers nil) ; avoid side-effects (e.g. auto-format)
  (setopt magit-revision-insert-related-refs nil) ; parent/related refs: rarely useful
  (setopt magit-process-finish-apply-ansi-colors t) ; render ANSI colors in process output

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)

  (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
    "Revert repo buffers and invalidate caches upon checkout."
    :after '(magit-checkout magit-branch-and-checkout)
    ;; Since the project likely now contains new files, best we undo the
    ;; projectile cache so it can be regenerated later.
    (after! 'projectile
      (projectile-invalidate-cache nil))
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'on-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers.
  (defvar +magit--pos nil)
  (add-hook 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))
  (add-hook 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))

  ;; <https://magit.vc/manual/magit/Switching-Buffers.html#index-magit_002ddisplay_002dbuffer_002dfullframe_002dstatus_002dv1>
  (setopt magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all))

(use-package treemacs-magit
  :after (treemacs magit))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; git-timemachine
;;  <https://codeberg.org/pidu/git-timemachine>
;;  <https://github.com/emacsmirror/git-timemachine>

(use-package git-timemachine
  :config

  ;; Show revision details in `header-line-format' instead of the minibuffer,
  ;; for better visibility.
  ;; via <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/modules/emacs/vc/config.el#L76C1-L90C47>
  (setopt git-timemachine-show-minibuffer-details t)
  (defadvice! +vc-update-header-line-a (revision)
    "Show revision details in the header-line, instead of the minibuffer."
    :override #'git-timemachine--show-minibuffer-details
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative)))))

;;
;;; `browse-at-remote' :: <https://github.com/rmuslimov/browse-at-remote>
;;

(use-package browse-at-remote
  :commands browse-at-remote
  :config
  (keymap-set cmx-git-map "o" #'browse-at-remote))

;;
;;; `consult-gh' :: <https://github.com/armindarvish/consult-gh>
;;

 ;; TODO: look through readme and review settings, add config as desired

;; NOTE: The double-dashed option names are NOT private, despite the Elisp
;; naming convention. Try to ignore it.
;; (use-package consult-gh
;;   :config
;;   (dolist (owner '("montchr" "seadome"))
;;     (add-to-list 'consult-gh-default-orgs-list owner))

;;   ;; TODO: use/lose
;;   ;; use "gh org list" to get a list of all your organizations and adds them to default list
;;   ;; (setq consult-gh-default-orgs-list (append consult-gh-default-orgs-list (remove "" (split-string (or (consult-gh--command-to-string "org" "list") "") "\n"))))

;;   ;; Set the default folder for cloning repositories. By default Consult-GH will
;;   ;; confirm this before cloning.
;;   (setq consult-gh-default-clone-directory
;;     (concat cmx-projects-dir "repos")))

(provide 'init-vcs)
;;; init-vcs.el ends here
