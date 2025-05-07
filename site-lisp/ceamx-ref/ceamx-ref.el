;;; ceamx-ref.el --- Ceamx Bibliographic library  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: bib, docs, hypermedia, convenience

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

;; (defalias 'ceamx-ref/url-to-bibtex ')

(provide 'ceamx-ref)
;;; ceamx-ref.el ends here
