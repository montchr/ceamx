;;; init-lang-php.el --- PHP language support        -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;;; Commentary:

;; Support for PHP language.

;;; Code:

(require 'lib-common)

;; FIXME: append, don't define a new var entirely
;; (defvar xref-ignored-files '("_ide_helper_models.php" "_ide_helper.php")
;;   "List of files to be ignored by `xref'.")

;; FIXME: relocate this... somewhere...
(defun xref-ignored-file-p (item)
  "Return t if `item' should be ignored."
  (seq-some
   (lambda (cand)
     (string-suffix-p cand (oref (xref-item-location item) file))) xref-ignored-files))

(use-package php-mode
  :init
  ;; Render multiline comments using `font-lock-comment-face'.
  (add-hook 'php-mode-hook #'ceamx--multiline-comment-face-h)
  :config
  (setopt php-mode-template-compatibility nil)

  ;; TODO: from doom
  ;; (set-docsets! 'php-mode "PHP" "PHPUnit" "Laravel" "CakePHP" "CodeIgniter" "Doctrine_ORM")
  ;; (set-repl-handler! 'php-mode #'+php/open-repl)
  ;; (set-lookup-handlers! 'php-mode :documentation #'php-search-documentation)
  ;; (set-ligatures! 'php-mode
  ;;   ;; Functional
  ;;   :lambda "function()" :lambda "fn"
  ;;   :def "function"
  ;;   ;; Types
  ;;   :null "null"
  ;;   :true "true" :false "false"
  ;;   :int "int" :float "float"
  ;;   :str "string"
  ;;   :bool "list"
  ;;   ;; Flow
  ;;   :not "!"
  ;;   :and "&&" :and "and"
  ;;   :or "||" :or "or"
  ;;   :for "for"
  ;;   :return "return"
  ;;   :yield "use")

  )

;; FIXME: i am going to assume this is all irrelevant in 2024 please
;; (use-feature! lsp-mode
;;   :init
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor")

;;   ;; FIXME: move these to lsp module?

;;   (def-advice! +lsp-ignored-locations-to-xref-items-a (items)
;;     :filter-return #'lsp--locations-to-xref-items
;;     "Remove ignored files from list of xref-items."
;;     (cl-remove-if #'xref-ignored-file-p items))

;;   (def-advice! +lsp-ui-peek--ignored-locations-a (items)
;;     :filter-return #'lsp-ui-peek--get-references
;;     "Remove ignored files from list of xref-items."
;;     (cl-remove-if #'xref-ignored-file-p items)))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

(after! web-mode
  ;; This should override the default file extension association.
  (pushnew! web-mode-engines-alist '(("blade"  . "\\.blade\\."))))

(provide 'init-lang-php)
;;; init-lang-php.el ends here
