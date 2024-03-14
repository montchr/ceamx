;;; lib-eww.el --- Helpers for Ceamx EWW  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery
;; Copyright (C) 2018  Howard X. Abrams

;; Author: Chris Montgomery <chris@cdom.io>
;;         Howard X. Abrams <howard.abrams@workday.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;;; Commentary:

;;; Sources:

;; - <https://gitlab.com/howardabrams/spacemacs.d/-/blob/51196e861da9a76a02f1159397ba85b936cdfe27/layers/ha-eww/funcs.el>
;; - <https://writequit.org/org/settings.html#sec-1-61>

;;; Code:

;;; Requirements

(require 'cl-lib)
(require 'url)

;;; Variables

(defun ceamx/eww-wiki (text)
  "Search Wikipedia for TEXT."
  (interactive (list (read-string "Wiki for: ")))
  (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
               (url-encode-url text))))

(defvar ceamx-eww-reddit-comment-header-regexp
  (rx "level "
      (one-or-more digit)
      (zero-or-more anything)
      line-end

      (group (one-or-more anything))
      line-end

      (one-or-more digit)
      " points"
      (one-or-more anything)
      line-end)
  "Regular expression for matching Reddit comments.")

(defvar ceamx-eww-github-repo-landing-readme-header-regexp
  (rx line-start
      "• "
      (one-or-more anything) ; SVG icon for the readme
      "README"
      line-end)
  "Regular expression matching the header for the README file content.")

(defvar ceamx-eww-github-begin-file-content-regexp
  (rx
   (one-or-more digit)
   " lines ("
   (one-or-more digit)
   " sloc) "
   (one-or-more digit)
   " ")
  "Regular expression for matching the start of a repo file on GitHub.")

;; TODO: support formats other than markdown too
(defvar ceamx-eww-github-footer-text-regexp
  (rx line-start
      (one-or-more anything)            ; SVG of GitHub logo
      " © "
      (one-or-more digit)
      " GitHub, Inc."
      line-end)
  "GitHub's copyright line is a good indication of the end of the content.
Note that as of 2024-03-13, repo file views no longer have a footer.")

;;; Functions

(defun ceamx/eww-clean-reddit ()
  "Remove a lot of the cruft in a rendered Reddit page."
  (interactive)
  (read-only-mode -1)

  ;; 2 comments
  ;; 79% Upvoted
  ;; What are your thoughts? Log in or Sign uplog insign up
  ;; Sort by

  ;; level 1
  ;; vale_fallacia
  ;; 1 point · 12 hours ago

  (flush-lines (rx line-start
                   (zero-or-more whitespace)
                   "Submit"))
  (while (re-search-forward ceamx-eww-reddit-comment-header-regexp nil t)
    (replace-match (concat "** " (match-string 1))))
  (read-only-mode 1))

(defun ceamx/eww-clean-github ()
  "Jump to the beginning of the content on a GitHub repo page."
  (interactive)
  (when (re-search-forward ceamx-eww-github-footer-text-regexp nil t)
    (read-only-mode -1)
    ;; (previous-line 2) ; "interactive use only"
    (forward-line -2)
    (delete-region (point) (point-max))
    (goto-char (point-min)))

  (when (or (re-search-forward ceamx-eww-github-begin-file-content-regexp nil t)
            (re-search-forward ceamx-eww-github-repo-landing-readme-header-regexp nil t))
    (forward-line 2)
    (recenter-top-bottom 0)))

(defun ceamx/eww-clean-stackoverflow ()
  "Jump to the start of interesting content on a Stack Overflow page."
  (interactive)
  (read-only-mode -1)

  ;; (mapcar (lambda (regex) (flush-lines regex))
  ;;         '("^up vote "
  ;;           "^answered "
  ;;           "^asked [A-Z]" "^edited [A-Z]"
  ;;           "^add a comment "
  ;;           "^share|"
  ;;           "^active oldest"))
  (mapc (lambda (regex) (flush-lines regex))
        '("^up vote "
          "^answered "
          "^asked [A-Z]" "^edited [A-Z]"
          "^add a comment "
          "^share|"
          "^active oldest"))

  (goto-char 0)
  (re-search-forward "Ask Question" nil t)
  (backward-paragraph 2)
  (forward-line)
  (recenter-top-bottom 0)

  (flush-lines "^Ask Question")
  (read-only-mode 1))

(defun ceamx-eww-rerender ()
  "Invoke a rerenderer function based on the URL to be displayed."
  (declare-function eww-current-url "eww")
  (declare-function eww-readable "eww")

  (let* ((url  (url-generic-parse-url (eww-current-url)))
         (host (url-host url))
         (path (car (url-path-and-query url)))
         (bits (split-string host "\\."))
         (site (cl-first (last bits 2))))
    (cond
     ((equal site "google")        (eww-readable))
     ((equal site "reddit")        (ceamx/eww-clean-reddit))
     ((equal site "github")        (ceamx/eww-clean-github))
     ((equal site "stackoverflow") (ceamx/eww-clean-stackoverflow)))))

;; FIXME: dependency i am not using -- replace function
;; (defun ceamx/eww-copy-feed-url ()
;;   "Take the EWW's current URL location and pass it to the `feed-discovery' function."
;;   (interactive)
;;   (feed-discovery-copy-feed-url (eww-current-url)))

(provide 'lib-eww)
;;; lib-eww.el ends here
