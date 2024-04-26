;;; curf.el --- Structured URL references            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: data, matching, convenience

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

(require 'cl-lib)
(require 'url)

(setq-local test-url "https://github.com/Mic92/dotfiles")

(cl-defstruct (curf-resource (:constructor curf-resource<--create)
                             (:type list)
                             (:named)
                             (:conc-name curf-resource<-))
  url host owner name)

(defun curf-url-meta<-create (url)
  "TODO"
  (let* ((url-struct (url-generic-parse-url url))
         (path (car (url-path-and-query url-struct)))
         (parts-regexp (rx
                        (group (1+ (any alnum "-_")))
                        "/"
                        (group (1+ (any alnum "-_")))
                        string-end))
         (parts (string-match parts-regexp path))
         (owner (match-string 1 path))
         (name (match-string 2 path)))
    (curf-resource<--create
     :url url-struct
     :host (url-host url-struct)
     :owner owner
     :name name)))

(defun curf-str ())

(setq-local test-meta (curf-url-meta<-create test-url))
(curf-resource<-owner test-meta)
(curf-resource<-name test-meta)

(defun curf--site-name (host)
  (pcase host
    (_ ())))

(setq-local test-url-struct (url-generic-parse-url test-url))
(url-path-and-query test-url-struct)


(provide 'curf)
;;; curf.el ends here
