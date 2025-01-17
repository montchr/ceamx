;;; -*- lexical-binding: t -*-

(require 'ceamx-lib)

;; General Dired customizations


(setopt delete-by-moving-to-trash t)
(setopt mouse-drag-and-drop-region-cross-program t)

(after! dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; cf. `dired-omit-files', `dired-omit-lines', `dired-omit-extensions'
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  ;; -A => dotfiles without . and ..
  ;; -F => append special chars to special files
  ;; -G => omit group name
  ;; -h => human-readable file sizes
  ;; -l => long listing, required by dired
  ;; -v => sort files by version number, not lexicographic
  (setopt dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

  (setopt dired-auto-revert-buffer #'dired-directory-changed-p)
  (setopt dired-do-revert-buffer (##not (file-remote-p %)))

  ;; When there are multiple Dired panes open, automatically use the
  ;; other pane as target for some actions (e.g. copying, moving).
  (setopt dired-dwim-target t)
  (setopt dired-kill-when-opening-new-dired-buffer t)
  (setopt dired-vc-rename-file t)
  (setopt dired-clean-confirm-killing-deleted-buffers nil)
  (setopt dired-clean-up-buffers-too t)
  (setopt dired-create-destination-dirs 'ask
          dired-create-destination-dirs-on-trailing-dirsep t
          dired-recursive-deletes 'always
          dired-recursive-copies 'always
          dired-backup-overwrite 'always)
  (setopt dired-free-space nil)
  (setopt dired-make-directory-clickable t)
  (setopt dired-mouse-drag-files t)

  ;;
  ;; Keybindings

  (define-keymap :keymap dired-mode-map
    "C-+" #'dired-create-empty-file
    "C-RET" #'dired-do-open))

;; ~dired-subtree~ :: insert subdirs arboreally


(package! dired-subtree
  (after! dired
    (define-keymap :keymap dired-mode-map
      "<tab>" #'dired-subtree-toggle
      "TAB" #'dired-subtree-toggle
      "<backtab>" #'dired-subtree-remove
      "S-TAB" #'dired-subtree-remove)))

(after! dired-subtree
  (setopt dired-subtree-use-backgrounds nil))

;; Writable Dired buffers with the ~wdired~ feature


(after! dired
  (keymap-set dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(after! wdired
  (keymap-set wdired-mode-map "C-c C-k" #'wdired-change-to-dired-mode)

  (setopt wdired-create-parent-directories t)
  (setopt wdired-allow-to-change-permissions t))

;; Preview images in Dired with the ~image-dired~ feature


(after! dired
  (keymap-set dired-mode-map "C-c t i" #'image-dired))

(after! image-dired
  (keymap-set image-dired-thumbnail-mode-map "RET" #'image-dired-thumbnail-display-external)

  (setopt image-dired-thumbnail-storage 'standard)
  (setopt image-dired-external-viewer "xdg-open")
  (setopt image-dired-thumb-relief 2
          image-dired-thumbs-per-row 4))

;; ~trashed~ :: interact with operating system trash diredly


(package! trashed)
(after! trashed
  (setopt trashed-action-confirmer #'y-or-n-p)
  (setopt trashed-use-header-line t)
  (setopt trashed-sort-key '("Date deleted" . t))
  (setopt trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; ~diredfl~ :: additional syntax highlighting in dired buffers

;; + Package :: <https://github.com/purcell/diredfl>


(package! diredfl
  (add-hook 'ceamx-after-init-hook #'diredfl-global-mode)
  (after! diredfl
    (set-face-attribute 'diredfl-dir-name nil :bold t)))

;; ~nerd-icons-dired~ :: icons for list items :icons:


(package! nerd-icons-dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;; ~dired-preview~ :: a file previewer

;; + Website :: <https://protesilaos.com/emacs/dired-preview>


(package! dired-preview
  (defer! 3
    (dired-preview-global-mode))
  (after! dired-preview
    (setopt dired-preview-delay 0.7)
    (setopt dired-preview-max-size (expt 2 20))
    (setopt dired-preview-ignored-extensions-regexp
            (concat "\\."
                    "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                    "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                    "\\|iso\\|epub\\|pdf\\)"))

    (keymap-set dired-mode-map "C-c t p" #'dired-preview-mode)))

(provide 'ceamx-init-dired)
;;; ceamx-init-dired.el ends here
