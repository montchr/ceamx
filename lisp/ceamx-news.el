;;; ceamx-news.el --- Ceamx News helpers             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: news, extensions, local

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

;; Helpers for `elfeed' and the like.

;;; Code:

;;;; Variables

;;;; Customization

(defgroup ceamx-news nil
  "Ceamx News"
  :group 'ceamx)

(defcustom ceamx-news-activity-name "news"
  "Activity-Name name for reading the news.
The name will be prefixed with the value of
`ceamx-activity-name-prefix'."
  :type 'string
  :group 'ceamx-news)

;;;; Functions

;;;###autoload
(defun ceamx-news-list-dead-feeds (&optional years)
  "Return a list of feeds that have not updated in YEARS.
When YEARS is nil, the threshold will be one year."
  (let* ( (years (or years 1.0))
          (living-feeds (make-hash-table :test 'equal))
          (seconds (* years 365.0 24 60 60))
          (threshold (- (float-time) seconds)))
    (with-elfeed-db-visit (entry-feed)
      (let ((date (elfeed-entry-date entry)))
        (when (> date threshold)
          (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
    (seq-filter (##gethash % living-feeds) (elfeed-feed-list))))

;;;; Commands

;;;###autoload
(defun ceamx-news/open-entry (entry)
  "Open the currently-selected entry in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (elfeed-show-entry entry)))

;;;###autoload
(defun ceamx-news/delete-pane ()
  "Delete the split feed-reader pane."
  (interactive)
  (let* ( (buf (get-buffer "*elfeed-entry*"))
          (window (get-buffer-window buf)))
    (delete-window window)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

;;;; Minor-Modes



;; FIXME: wip
;; (defun ceamx/thee-news ()
;;   (interactive)
;;   (let* ( (activity-name (concat
;;                            ceamx-activity-name-prefix
;;                            ceamx-news-activity-name))
;;           (new-activity-p (not (member activity-name (activities-names)))))
;;     (when new-activity-p
;;       (make-activities-activity :name activity-name))
;;     (let ((activity (activities-named activity-name)))
;;       (activities-switch activity)
;;       (activities-set activity :state nil)
;;       (unless (memq (buffer-local-value 'major-mode
;;                       (window-buffer (selected-window)))
;;                 '( elfeed-show-mode
;;                    elfeed-search-mode))
;;         (elfeed))
;;       (activities-save activity :defaultp new-activity-p))))

(provide 'ceamx-news)
;;; ceamx-news.el ends here
