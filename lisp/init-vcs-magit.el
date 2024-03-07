;;; init-vcs-magit.el --- Magit support              -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  <https://magit.vc/>

;;; Code:

(require 'lib-common)
(require 'lib-vcs-magit)

(use-feature! transient)

(use-package magit
  :commands (magit-status
              magit-dispatch
              magit-file-dispatch
              magit-revert
              magit-discard)
  :after (seq transient git-commit)

  :config
  (after! [evil-collection]
    (declare-function evil-collection-init "evil-collection")
    (evil-collection-init 'magit))

  (setopt magit-diff-refine-hunk t)     ; show granular diffs in selected hunk
  (setopt magit-save-repository-buffers nil) ; avoid side-effects (e.g. auto-format)
  (setopt magit-revision-insert-related-refs nil) ; parent/related refs: rarely useful
  (setopt magit-process-finish-apply-ansi-colors t) ; render ANSI colors in process output

  ;; Close transient with ESC
  ;; FIXME: move to configuration for `transient'
  (define-key transient-map [escape] #'transient-quit-one)

  ;; FIXME: meow won't allow
  ;; TODO: what is the default magit binding for "j"?
  ;; (keymap-set magit-status-mode-map "j" nil)

  ;; These should be bound automatically when `magit-define-global-key-bindings'
  ;; is =default= (which is the default value), but that does not seem to work.
  (define-keymap :keymap (current-global-map)
    "C-x g"    #'magit-status
    "C-x M-g"  #'magit-dispatch
    "C-c M-g"  #'magit-file-dispatch)

  ;; TODO: why does this not have an effect?
  (define-keymap :keymap magit-status-mode-map
    "_" #'magit-revert
    "V" nil
    "x" #'magit-discard)

  (keymap-set magit-status-mode-map "_" #'magit-revert)
  ;; TODO: rebind?
  (keymap-set magit-status-mode-map "V" nil)
  ;; NOTE: Overrides default binding of `magit-reset-quickly'.
  (keymap-set magit-status-mode-map "x" #'magit-discard)

  ;; FIXME: move to `:init'
  ;; via doomemacs (exact location forgotten)
  (defun +magit--revert-repo-buffers-deferred-a (&rest _)
    "Revert repo buffers and invalidate caches upon checkout."
    ;; Since the project likely now contains new files, best we undo the
    ;; projectile cache so it can be regenerated later.
    (after! 'projectile
      (declare-function projectile-invalidate-cache "projectile")
      (projectile-invalidate-cache nil))
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))
  (advice-add 'magit-checkout :after #'+magit--revert-repo-buffers-deferred-a)
  (advice-add 'magit-branch-and-checkout :after #'+magit--revert-repo-buffers-deferred-a)
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'on-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers.
  ;; FIXME: these are a really uniquely terrible way of defining hook functions...
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

  (transient-append-suffix 'magit-commit "-n"
    '("-S" "Disable GPG signing" "--no-gpg"))
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all))

(provide 'init-vcs-magit)
;;; init-vcs-magit.el ends here
