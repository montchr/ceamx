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

(setopt vc-follow-symlinks t)

(use-package git-commit
  :defer t
  :after (transient))

(after! 'evil
  (add-hook 'git-commit-mode-hook #'evil-insert-state))
(after! 'meow
  (add-hook 'git-commit-mode-hook #'meow-insert-mode))


;;; git-timemachine :: <https://codeberg.org/pidu/git-timemachine>
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

;;; `browse-at-remote' :: <https://github.com/rmuslimov/browse-at-remote>
(use-package browse-at-remote
  :commands browse-at-remote
  :init
  (keymap-set cmx-git-map "o" #'browse-at-remote))

;;
;;; `consult-gh' :: <https://github.com/armindarvish/consult-gh>
;;

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
;;     (concat cmx-projects-dir "repos")))

(provide 'init-vcs)
;;; init-vcs.el ends here
