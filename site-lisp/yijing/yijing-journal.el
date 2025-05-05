;;; yijing-journal.el --- Yijing consultation journal  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: hypermedia

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

;;

;;; Code:

(require 'ht)
(require 'pcsv)

(require 'yijing)

(defgroup yijing-journal '()
  "Interface for working with structured records of Yijing consultations."
  :group 'yijing)

;;;; Internal variables

(defvar yijing-journal-consultations nil)

(defvar yijing-journal-menu-search-current-regexp yijing-journal-menu-initial-regexp
  "Regular expression for the currently-active search in
`yijing-journal-menu-mode'.")

;;;; Customization

(defcustom yijing-journal-default-consultations-file nil
  "Default file containing CSV data representing a set of Yijing consultations."
  :type 'file
  :group 'yijing-journal)

(defcustom yijing-journal-app-csv-columns
  [ :datetime
    :querent
    :setting
    :query
    :primary-hexagram-num
    :relating-hexagram-num
    :unknown-1
    :consultation-date
    :unknown-2
    :unknown-3
    :primary-hexagram-lines
    :relating-hexagram-lines
    :uuid]
  "Ordered columns of the CSV format recognized by the Yijing Library app.
The app does not provide documentation for its CSV import/export format,
so the meaning of some of the columns is currently unknown."
  :type '(list sexp)
  :group 'yijing-journal)

(defcustom yijing-journal-menu-date-column-width 17
  "Width for the Date column in `yijing-journal-menu-mode'."
  :type 'number
  :group 'yijing-journal)

(defcustom yijing-journal-menu-query-column-width 85
  "Width for the Query column in `yijing-journal-menu-mode'."
  :type 'number
  :group 'yijing-journal)

(defcustom yijing-journal-menu-action (lambda (_uuid) (ignore))
  "Function to invoke when a `yijing-journal-menu' item is activated.
The function accepts a single argument _UUID, which is the UUID of the
`yijing-journal-consultation' object corresponding to the item."
  :type 'function
  :group 'yijing-journal)

(defcustom yijing-journal-menu-search-initial-regexp "."
  "Regular expression used to initially populate the `yijing-journal-menu'
buffer with matching consultations."
  :type 'string
  :group 'yijing-journal)

;;;; Modes

(define-derived-mode yijing-journal-menu-mode tabulated-list-mode "Yijing Journal"
  "Major mode for browsing a list of Yijing consultations."
  :interactive nil
  (setq tabulated-list-format `[ ("Date" ,yijing-journal-menu-date-column-width t)
                                 ("Query" ,yijing-journal-menu-query-column-width t)
                                 ("Primary Hexagram" 17 nil)
                                 ("Relating Hexagram" 17 nil)])
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header)
  (tabulated-list-print))


(defun yijing-journal-menu-update-entries ()
  )

;;;; Commands

;;;###autoload
(defun yijing-journal-list-consultations ()
  "Display a list of Yijing consultations."
  (interactive)
  (let ( (buffer-name (format "*Yijing Consultations %s"
                       yijing-journal-default-entries-file)))
    (if (get-buffer buffer-name)
        (pop-to-buffer-same-window buffer-name)
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (setq buffer-file-coding-system 'utf-8)
          (setq yijing-journal-menu-search-current-regexp yijing-journal-menu-search-initial-regexp)
          (yijing-journal-menu-mode)
          (setq-local yijing-journal-consultations (yijing-journal-consultations))
          (yijing-journal-menu-update-entries))))))


;;;; Functions

;;;###autoload
(defun yijing-journal-consultations ()
  "Return a hash-table of the current `yijing-journal-consultation' objects."
  (let ((entries (yijing-journal--load-consultations)))
    (yijing-journal--consultations-from-alists entries)))

;;;###autoload
(defun yijing-journal-find-consultation (uuid)
  "Return the `yijing-journal-consultation' object identified by UUID."
  (map-elt (yijing-journal-consultations) uuid))

;;;;; Consultation menu helpers

(defun yijing-journal--menu-entries-to-queries ()
  "Return a list of consultation queries in the current buffer."
  (mapcar (lambda (entry))
    (funcall tabulated-list-entries)))

(defun yijing-journal--menu-entry-from-uuid (uuid)
  "Given UUID for a `yijing-journal-consultation', return an entry matching
the form of `tabulated-list-entries', which see."
  (let ((entry (yijing-journal-find-consultation uuid)))
    (list uuid
      [ (yijing-journal-consultation-datetime entry)
        (yijing-journal-consultation-query entry)
        (yijing-journal-consultation-primary entry)
        (yijing-journal-consultation-relating entry)])))

;;;;; Consultation objects

(cl-defstruct
  (yijing-journal-consultation
    (:constructor yijing-journal-consultation--create)
    (:copier nil))
  "Data describing a Yijing oracle consultation."
  uuid datetime setting query primary relating)

(defun yijing-journal--consultation-create-from-row (row)
  (yijing-journal-consultation--create
    :uuid (map-elt row :uuid)
    :datetime (map-elt row :datetime)
    :setting (map-elt row :setting)
    :query (map-elt row :query)
    :primary (map-elt row :primary-hexagram-lines)
    :relating (map-elt row :relating-hexagram-lines)))

(defun yijing-journal--consultations-from-alists (entries)
  (map-into
    (seq-map
      (## cons (map-elt % :uuid) (yijing-journal--consultation-create-from-row %))
      entries)
    'hash-table))

;;;;; CSV data parsing

(defun yijing-journal--load-consultations (&optional file)
  "Return a list of alists representing CSV rows loaded from FILE.
When FILE is nil, use the `yijing-journal-default-consultations-file'."
  (let* ( (file (or file yijing-journal-default-consultations-file)))
    (yijing-journal--parse-consultations-from-file file)))

(defun yijing-journal--parse-consultations-from-file (file)
  (let ((data (pcsv-parse-file file)))
    (yijing-journal--csv-label-rows
      ;; TODO: maybe just use the file's column headers?
      (seq-rest data)
      yijing-journal-app-csv-columns)))

(defun yijing-journal--entries-from-app-exported-file (file)
  (let* ( (pcsv-separator ?\;)
          (data (pcsv-parse-file file)))
    (yijing-journal--csv-label-rows data yijing-journal-app-csv-columns)))

(defun yijing-journal--csv-label-rows (data labels)
  (seq-map (## yijing-journal--csv-label-row % labels) data))

(defun yijing-journal--csv-label-row (data labels)
  (seq-mapn (## cons %1 %2) labels data))

(provide 'yijing-journal)
;;; yijing-journal.el ends here
