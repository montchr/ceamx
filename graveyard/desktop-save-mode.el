;;; Save and restore Emacs sessions with `desktop-save-mode'

;; Does not work well with `use-feature!' on `emacs' or `desktop'.
;; Actually, I can't tell when it works well at all.

(noop!
  ;; FIXME: This is, somehow, always reset to the default value on init! This
  ;; makes `desktop-save-mode' unusable. Considering this is not the first
  ;; frustrating issue I've had with it, I give up.
  (setq desktop-dirname ceamx-var-dir)

  (let ((filename "ceamx.desktop"))
    (setopt desktop-base-file-name filename)
    (setopt desktop-base-lock-name (concat filename ".lock")))

  ;; Save session state after five minutes idle.
  ;; (setopt desktop-auto-save-timeout (* 60 5))

  ;; If the desktop file is still locked, that probably means something went
  ;; wrong during the previous Emacs session because each session should remove
  ;; its locks when exiting. Play it safe and do nothing.
  (setopt desktop-load-locked-desktop nil)

  ;; All other buffers will be restored lazily during idle.
  (setopt desktop-restore-eager 20)

  ;; This is kind of the whole point to me.
  (setopt desktop-restore-frames t)

  ;; Always save the session without asking and regardless of whether a previously-saved
  ;; session file exists.
  (setopt desktop-save t)

  (desktop-save-mode 1))
