;;; yijing.el --- Package for Yijing consultations   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: tools, local, convenience, help, calendar

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

;;;; Reference

;; The Boolean I Ching :: <https://www.russellcottrell.com/VirtualYarrowStalks/booleanIChing.htm>

;;; Code:

(defconst yijing-version "0.1.0")
(defconst yijing-base-directory (file-name-directory load-file-name))

(require 'cl-lib)
(require 'seq)
(require 'map)

(require 'llama)

;;;; Variables


;;;; Customization

(defgroup yijing '()
  "Package providing tools for working with the Yijing / I Ching / Book of Changes.")

(defcustom yijing-data-directory
  (file-name-as-directory (concat yijing-base-directory "data"))
  "Storage directory for the `yijing' source data files."
  :group 'yijing
  :type 'directory)

(defcustom yijing-translation-data-directory
  (file-name-as-directory (concat yijing-data-directory "translations"))
  "Storage directory for the `yijing' translation data files."
  :group 'yijing
  :type 'directory)

(defcustom yijing-bigrams-json-file
  (file-name-concat yijing-data-directory "bigrams.json")
  "JSON file containing Yijing bigram data."
  :group 'yijing
  :type 'file)

(defcustom yijing-trigrams-json-file
  (file-name-concat yijing-data-directory "trigrams.json")
  "JSON file containing Yijing trigram data."
  :group 'yijing
  :type 'file)

(defcustom yijing-hexagrams-json-file
  (file-name-concat yijing-data-directory "hexagrams.json")
  "JSON file containing Yijing hexagram data."
  :group 'yijing
  :type 'file)

(defcustom yijing-translation 'wilhelm-baynes
  "Yijing translation to use for hexagram content.
The translation content must exist in the directory specified by
`yijing-translation-data-directory', named corresponding to the symbol's
string value.  For example, the \"wilhelm-baynes\" translation should
live inside a file named \"wilhelm-baynes.json\"."
  :group 'yijing
  :type '(choice :tag "Yijing translation" :value wilhelm-baynes
           (const :tag "Richard Wilhelm & Cary F. Baynes translation" wilhelm-baynes)
           (const :tag "Stephen Karcher translation" karcher)
           (const :tag "Jack Balkin translation" balkin)))

;;;; Functions

;;;;; Unigram operations

(defun yijing--hexagram-number-to-ascii (hexagram)
  ""
  (concat
    (seq-reverse
      (seq-reduce
        (lambda (acc it)
          (push
            (pcase it
              (6 ?X)
              (7 ?|)
              (8 ?:)
              (9 ?O)
              (_ (error "Unexpected token %s in %s" it hexagram)))
            acc))
        hexagram nil))))

(defun yijing--hexagram-ascii-to-number (hexagram)
  ""
  (seq-reverse
    (seq-reduce
      (lambda (acc it)
        (push
          (pcase it
            (?X 6)
            (?| 7)
            (?: 8)
            (?O 9)
            (_ (error "Unexpected token %s in %s" it hexagram)))
          acc))
      hexagram nil)))

(defun yijing--numeric-unigram-to-bool (unigram)
  "Convert numeric UNIGRAM to a boolean value.
Note that this operation will destroy any changing-line data."
  (= 1 (% unigram 2)))

(defun yijing--bool-to-numeric-unigram (a)
  "Convert boolean A to a numeric unigram value."
  (if a 7 8))

(defun yijing-unigram-and (a b))

;;;;; Data parsing

(defun yijing--parse-json-file (file)
  "Parse the contents of a JSON file."
  (with-temp-buffer
    (insert-file-contents file)
    (beginning-of-buffer)
    (json-parse-buffer
      :array-type 'list
      :false-object nil)))

(defun yijing-bigrams ()
  "TODO"
  (yijing--parse-json-file yijing-bigrams-json-file))

(defun yijing-trigrams ()
  "TODO"
  (yijing--parse-json-file yijing-trigrams-json-file))

(defun yijing-hexagrams ()
  "TODO"
  (yijing--parse-json-file yijing-hexagrams-json-file))


(defun yijing-trigram-info (number)
  "TODO"
  (seq-find (##= number (map-elt % "number"))
    (yijing-trigrams)))

(defun yijing-hexagram-info (number)
  "TODO"
  (seq-find (##= number (map-elt % "number"))
    (yijing-hexagrams)))

(defun yijing-hexagram )

(provide 'yijing)
;;; yijing.el ends here
