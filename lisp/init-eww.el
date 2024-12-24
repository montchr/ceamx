;;; init-eww.el --- Surfing the Wild Web of Words  -*- lexical-binding: t; -*-

;; TODO: headers

;;; Commentary:

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)

(require 'ceamx-lib)
(require 'lib-eww)

(autoload 'eww "eww")

;;; Configuration

;; Configure EWW settings before loading


(setopt shr-use-colors t)
(setopt shr-folding-mode t)
(setopt shr-bullet "• ")

(setopt eww-search-prefix "https://duckduckgo.com/html?q=")

;; HTTP headers may contain user information, which we can limit as needed.
;; When providing a list of symbols, the symbols indicate what NOT to send.
;; TODO: Move this elsewhere, as it probably affects other HTTP requests.
(setopt url-privacy-level '(email lastloc))

;; EWW: Keybindings


(after! eww
  (define-keymap :keymap eww-mode-map
    "," '("scroll down" . scroll-up-command)
    "." '("scroll up" . scroll-down-command)
    "o" '("open link" . link-hint-open-link)))

;; Cleanup the rendering of some pages


(add-hook 'eww-after-render-hook #'ceamx-eww-rerender)

(provide 'init-eww)
;;; init-eww.el ends here
