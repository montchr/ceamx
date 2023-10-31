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

(setq vc-follow-symlinks t)

(after! [evil]
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

(use-package diff-hl
  :after (vc-mode)
  :commands (global-diff-hl-mode)
  :init (global-diff-hl-mode +1))

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

  (setq magit-diff-refine-hunk t)                  ; show granular diffs in selected hunk
  (setq magit-save-repository-buffers nil)         ; avoid side-effects (e.g. auto-format)
  (setq magit-revision-insert-related-refs nil)    ; parent/related refs: rarely useful
  (setq magit-process-finish-apply-ansi-colors t)  ; render ANSI colors in process output

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)

  ;; FIXME: on `on-switch-buffer-hook'
  ;; (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
  ;;   :after '(magit-checkout magit-branch-and-checkout)
  ;;   ;; Since the project likely now contains new files, best we undo the
  ;;   ;; projectile cache so it can be regenerated later.
  ;;   (projectile-invalidate-cache nil)
  ;;   ;; Use a more efficient strategy to auto-revert buffers whose git state has
  ;;   ;; changed: refresh the visible buffers immediately...
  ;;   (+magit-mark-stale-buffers-h))
  ;; ;; ...then refresh the rest only when we switch to them, not all at once.
  ;; (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

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
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)


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
  (setq git-timemachine-show-minibuffer-details t)
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

(provide 'init-vcs)
;;; init-vcs.el ends here
