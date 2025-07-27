;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

(setup browse-url
  (:when-loaded
    (:with-feature eww
      (setopt browse-url-browser-function #'eww-browse-url))))

(setup shr
  (:when-loaded
    (setopt shr-use-colors t)
    (setopt shr-folding-mode t)
    (setopt shr-bullet "• ")))

(setup eww
  (:also-load ceamx-eww)
  (:bind "," '("scroll down" . scroll-up-command)
         "." '("scroll up" . scroll-down-command)
         "o" '("open link" . link-hint-open-link))
  (:with-map ceamx-launch-prefix
    (:bind "b" #'eww))
  (:with-feature ceamx-eww
    (:with-map ceamx-launch-prefix
      (:bind "W" #'ceamx/eww-wiki)))
  ;; Avoid loading this feature until needed.
  (:when-loaded
    (setopt eww-search-prefix "https://duckduckgo.com/html?q=")))

(setup url
  ;; HTTP headers may contain user information, which we can limit as needed.
  ;; When providing a list of symbols, the symbols indicate what NOT to send.
  ;; TODO: Move this elsewhere, as it probably affects other HTTP requests.
  (setopt url-privacy-level '(email lastloc)))

(setup eww
  (:when-loaded
    (:with-feature ceamx-eww
      (:with-function #'ceamx-eww-rerender
        (:hook-into eww-after-render-hook)))))

(provide 'ceamx-init-eww)
;;; ceamx-init-eww.el ends here
