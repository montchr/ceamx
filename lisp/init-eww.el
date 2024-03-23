;;; init-eww.el --- Surfing the Wild Web of Words  -*- lexical-binding: t; -*-

;; TODO: headers

;;; Commentary:

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)

(require 'lib-common)
(require 'lib-eww)

(autoload 'eww "eww")

;;; Configuration

;; DISABLED Tell Emacs we want its Web Wowser to handle URL browsing by default

;; Currently, since so many URLs point to GitHub, and GitHub will not function
;; without JavaScript, it does not make sense to use EWW by default.


;; [[file:../config.org::*DISABLED Tell Emacs we want its Web Wowser to handle URL browsing by default][DISABLED Tell Emacs we want its Web Wowser to handle URL browsing by default:1]]
;; (setopt browse-url-browser-function 'eww-browse-url)
;; DISABLED Tell Emacs we want its Web Wowser to handle URL browsing by default:1 ends here

;; Configure EWW settings before loading


;; [[file:../config.org::*Configure EWW settings before loading][Configure EWW settings before loading:1]]
(setopt shr-use-colors t)
(setopt shr-folding-mode t)
(setopt shr-bullet "• ")

(setopt eww-search-prefix "https://duckduckgo.com/html?q=")

;; HTTP headers may contain user information, which we can limit as needed.
;; When providing a list of symbols, the symbols indicate what NOT to send.
;; TODO: Move this elsewhere, as it probably affects other HTTP requests.
(setopt url-privacy-level '(email lastloc))
;; Configure EWW settings before loading:1 ends here

;; EWW: Keybindings


;; [[file:../config.org::*EWW: Keybindings][EWW: Keybindings:1]]
(define-keymap :keymap ceamx-launch-map
  "b" #'eww
  "W" #'ceamx/eww-wiki)

(after! 'eww
  (define-keymap :keymap eww-mode-map
    "," '("scroll down" . scroll-up-command)
    "." '("scroll up" . scroll-down-command)
    "o" '("open link" . link-hint-open-link)))
;; EWW: Keybindings:1 ends here

;; Cleanup the rendering of some pages


;; [[file:../config.org::*Cleanup the rendering of some pages][Cleanup the rendering of some pages:1]]
(add-hook 'eww-after-render-hook #'ceamx-eww-rerender)
;; Cleanup the rendering of some pages:1 ends here

(provide 'init-eww)
;;; init-eww.el ends here
