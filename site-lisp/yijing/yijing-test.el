;;; yijing-test.el --- Testing for the Yijing package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords:

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

;;; Code:

(require 'yijing)

(ert-deftest yijing-test-ascii-to-number nil
  (should
    (equal (yijing--hexagram-ascii-to-number
             "||:O|::|:X:|")
      '(7 7 8 9 7 8 8 7 8 6 8 7)))
  (should-error (yijing--hexagram-ascii-to-number "||:|0:")))

(ert-deftest yijing-test-number-to-ascii nil
  (should
    (equal (yijing--hexagram-number-to-ascii
             '(7 7 8 9 7 8 8 7 8 6 8 7))
      "||:O|::|:X:|"))
  (should-error (yijing--hexagram-number-to-ascii '(7 7 8 9 7 5))))

;; Local Variables:
;; eval: (prettify-symbols-mode -1)
;; indent-tabs-mode: nil
;; End:
;;; yijing-test.el ends here
