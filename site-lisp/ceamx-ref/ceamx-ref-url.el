;;; ceamx-ref-url.el --- Ceamx-Ref URL helpers       -*- lexical-binding: t; -*-

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

;;;; Requirements

(require 'request)
(eval-when-compile
  (require 'dom)
  (require 'json)
  (require 'xml))

;;;; Variables

(defvar ceamx-ref-url-biblatex-template
  "@online{key,
title   = {${:title}},
author  = {${:author}},
url     = {${:url}}
year    = {${:year}},
urldate = {Online; accessed ${:urldate}}
}"
  "Biblatex entry template for online sources.")

;;;; Functions

(defun ceamx-ref-url--dom-head (url)
  (let ((res (request url
    :parser (##libxml-parse-html-region)
    :success
    (cl-function
      (lambda (&key data &allow-other-keys)
        (dom-by-tag data 'head))))))))

(defun ceamx-ref-url-json-ld (url)
  (request url
    :sync t
    :parser (##libxml-parse-html-region)
    :success
    (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((head (car (dom-by-tag data 'head)))
                (scripts (dom-by-tag head 'script))
                (jsonld-str (car (dom-search
                                   scripts
                                   (##string-equal "application/ld+json"
                                     (dom-attr % 'type)))))
                (jsonld (json-parse-string jsonld-str :object-type 'plist)))
          (json-alist-p jsonld))))))

(provide 'ceamx-ref-url)
;;; ceamx-ref-url.el ends here
