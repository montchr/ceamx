;;; hl-sentence-end.el --- Minor mode to highlight sentence-end spacing errors  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
;;         Chris Montgomery <chmont@proton.me>
;; URL: https://emacs.stackexchange.com/a/48479
;; Keywords: faces, convenience

;; This work is licensed under CC BY-SA 4.0. To view a copy of this license,
;; visit <http://creativecommons.org/licenses/by-sa/4.0/>.

;;; Commentary:

;; <https://emacs.stackexchange.com/a/48479>

;;;; Usage

;; (global-hl-sentence-end-mode) ;; Just turn the global mode on.

;;; Code:

(defcustom hl-sentence-end-major-mode-list '(text-mode fundamental-mode)
  "Major modes with `hl-sentence-end-mode' turned on by default."
  :type '(repeat :tag "Major modes" symbol)
  :group 'hl-sentence-end)

(defvar hl-sentence-end-re "\\.\\( \\)[^[:space:]]"
  "Regular expression for marking missing double spaces as sentence separators.")

(defvar-local hl-sentence-end-font-lock-keywords nil
  "Additional `font-lock-keywors' entries for hdls-mode.")

(defface hl-sentence-end
  '((((class color) (background dark))
      :background "grey" :foreground "darkgray")
     (((class color) (background light))
       :background "yellow"  :foreground "lightgray")
     (t :inverse-video t))
  "Face used to mark missing sentence separating double spaces."
  :group 'hl-sentence-end)

(defun hl-sentence-end--clear (start end)
  "Clear hl-sentence-end fontification in region from START to END."
  (with-silent-modifications
    (cl-loop for int being the intervals property 'face from start to end
         if (eq (get-text-property (car int) 'face) 'hl-sentence-end)
         do (remove-text-properties (car int) (cdr int) '(face hdls)))))

(defun hl-sentence-end--jitlock-handler (start end)
  "Mark double spaces in region from START to END."
  (save-excursion
   (with-silent-modifications
     (hl-sentence-end--clear start end)
     (goto-char (max (- start 2) 1))
     (when (< end (point-max))
       (cl-incf end))
     (while (re-search-forward hl-sentence-end-re end t)
       (put-text-property (match-beginning 1) (match-end 1) 'face 'hl-sentence-end)))))

(defun hl-sentence-end-enable ()
  "Turn on `hl-sentence-end-mode' in the current buffer."
  (when (and (apply #'derived-mode-p hl-sentence-end-major-mode-list)
             sentence-end-double-space)
    (hl-sentence-end-mode)))

(define-globalized-minor-mode global-hl-sentence-end-mode hl-sentence-end-mode hl-sentence-end-enable)

(define-minor-mode hl-sentence-end-mode
  "Highlight missing double-spaces that should separate sentences."
  nil
  " ds"
  nil
  (if hl-sentence-end-mode
      (if font-lock-mode
      (progn
        (font-lock-add-keywords
         nil
         (setq hl-sentence-end-font-lock-keywords `((,hl-sentence-end-re 1 'hl-sentence-end))))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure)))
    (unless jit-lock-mode (jit-lock-mode t))
    (jit-lock-register #'hl-sentence-end--jitlock-handler)
    (jit-lock-fontify-now (point-min) (point-max)))
    (when font-lock-mode
      (font-lock-remove-keywords nil hl-sentence-end-font-lock-keywords)
      (save-restriction
    (widen)
    (font-lock-flush (point-min) (point-max))))
    (when jit-lock-mode
      (save-restriction
    (widen)
    (hl-sentence-end--clear (point-min) (point-max)))
      (jit-lock-unregister #'hl-sentence-end--jitlock-handler))))

(provide 'hl-sentence-end)
;;; hl-sentence-end.el ends here
