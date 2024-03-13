;;; init-eww.el --- Surfing the Wild Web of Words  -*- lexical-binding: t; -*-

;; TODO: headers

;;; Commentary:

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)
(require 'lib-keys)

(autoload 'eww "eww")

;;; Configuration

;; (setopt browse-url-browser-function 'eww-browse-url)

(setopt shr-use-colors t)
(setopt shr-folding-mode t)
(setopt shr-bullet "• ")

(setopt eww-search-prefix "https://duckduckgo.com/html?q=")

;; HTTP headers may contain user information, which we can limit as needed.
;; When providing a list of symbols, the symbols indicate what NOT to send.
;; TODO: Move this elsewhere, as it probably affects other HTTP requests.
(setopt url-privacy-level '(email lastloc))

(keys! ceamx-launch-map
  "b" #'eww)

(provide 'init-eww)
;;; init-eww.el ends here
