;;; init-vcs.el --- Git/VCS -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
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

;;  Configurations for git + magit.
;;  And other version control systems, if they exist...

;;; Code:

;; Follow symlinks.
(setq vc-follow-symlinks t)

(use-package diff-hl
  :after (vc-mode)
  :init
  (global-diff-hl-mode +1))


;;
;;; === MAGIT ======================================================================================
;;  <https://magit.vc/>

(autoload '+magit-display-buffer-fn "lib-magit")
(autoload '+magit-mark-stale-buffers-h "lib-magit" "Revert all visible buffers and mark buried buffers as stale.")
(autoload '+magit-revert-buffer-maybe-h "lib-magit" "Update `vc' and `git-gutter' if out of date.")
(autoload '+magit/quit "lib-magit" "Bury the current magit buffer. <...>" t)
(autoload '+magit/quit-all "lib-magit" "Kill all magit buffers for the current repository." t)

;; (elpaca magit-section)

(use-package magit
  ;; :defer t
  ;; :after (general magit-section)

  :general
  (+general-global-git/version-control
    "g"  'magit-status
    "b"  'magit-branch
    "B"  'magit-blame
    "c"  'magit-clone

    "f"  '(:ignore t :which-key "file")
    "ff" 'magit-find-file
    "fh" 'magit-log-buffer-file

    "i"  'magit-init
    "L"  'magit-list-repositories
    "m"  'magit-dispatch
    "S"  'magit-stage-file
    "s"  'magit-status
    "U"  'magit-unstage-file)

  :config
  (setq magit-diff-refine-hunk t)                  ; show granular diffs in selected hunk
  (setq magit-save-repository-buffers nil)         ; avoid side-effects (e.g. auto-format)
  (setq magit-revision-insert-related-refs nil)    ; parent/related refs: rarely useful
  (setq magit-process-finish-apply-ansi-colors t)  ; render ANSI colors in process output

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)

  ;; FIXME: replace `doom-switch-buffer-hook'
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


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; git-timemachine
;;  <https://codeberg.org/pidu/git-timemachine>
;;  <https://github.com/emacsmirror/git-timemachine>

(use-package git-timemachine
  :general
  (+general-global-git/version-control
    "t" #'git-timemachine))

(provide 'init-vcs)
;;; init-vcs.el ends here
