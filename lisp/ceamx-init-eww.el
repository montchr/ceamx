;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

(setopt browse-url-browser-function 'eww-browse-url)

(setopt shr-use-colors t)
(setopt shr-folding-mode t)
(setopt shr-bullet "• ")

(setopt eww-search-prefix "https://duckduckgo.com/html?q=")

;; HTTP headers may contain user information, which we can limit as needed.
;; When providing a list of symbols, the symbols indicate what NOT to send.
;; TODO: Move this elsewhere, as it probably affects other HTTP requests.
(setopt url-privacy-level '(email lastloc))

(after! eww
  (define-keymap :keymap eww-mode-map
    "," '("scroll down" . scroll-up-command)
    "." '("scroll up" . scroll-down-command)
    "o" '("open link" . link-hint-open-link)))

(use-feature! ceamx-eww
  :after eww
  :functions (ceamx-eww-rerender)
  :init
  (add-hook 'eww-after-render-hook #'ceamx-eww-rerender))

(provide 'ceamx-init-eww)
;;; ceamx-init-eww.el ends here
