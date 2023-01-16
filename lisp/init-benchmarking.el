;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;
;; Copyright (c) 2006-2021, Steve Purcell
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;; Commentary:
;;; Code:

(defun cmx/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar cmx/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun cmx/require-times-wrapper (orig feature &rest args)
  "Note in `cmx/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (cmx/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'cmx/require-times
                       (list feature require-start-time time)
                       t))))))
(advice-add 'require :around 'cmx/require-times-wrapper)

(define-derived-mode cmx/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 cmx/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 cmx/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'cmx/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun cmx/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun cmx/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun cmx/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in cmx/require-times
           with order = 0
           do (cl-incf order)
           collect (list order
                         (vector
                          (format "%.3f" (cmx/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun cmx/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (cmx/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))

(defun cmx/show-init-time ()
  (message "init completed in %.2fms"
           (cmx/time-subtract-millis after-init-time before-init-time)))
(add-hook 'after-init-hook 'cmx/show-init-time)

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
