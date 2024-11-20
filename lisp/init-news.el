;;; init-news.el --- News feed (RSS/Atom) subscription support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
;; Keywords: news, local

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

;; But do I really want to know what's happening outside of Emacs?

;; FIXME: use <https://github.com/skeeto/elfeed>
;; FIXME: OPML instead of weird lists

;;; Code:

(defconst ceamx-reading-dir (concat ceamx-home-dir "Documents/reading/"))

(package! elfeed
  (keymap-set ceamx-launch-map "f" #'elfeed))

(package! elfeed-org
  (require 'elfeed-org)

  ;; When `elfeed' starts, `elfeed-org' will read the configuration.
  (elfeed-org)

  (setopt rmh-elfeed-org-files (list (locate-user-emacs-file "feeds.org")
                                     ;; (file-name-concat ceamx-reading-dir "000-feeds.org")
                                     )))



(provide 'init-news)
;;; init-news.el ends here
