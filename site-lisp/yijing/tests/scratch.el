;; -*- mode: lisp-interaction; -*-

;; This is ‘lisp-interaction-mode’.  Use ‘C-j’ to evaluate and print results.

(require 'yijing)

(yijing-random-between 2 3)

(let ((trigram (yijing-trigram-info 4)))
  (map-elt trigram "ascii"))

(yijing-hexagram-info
 (yijing-hexagram-from-coins))

(require 'yijing)
(require 'yijing-journal)

(let* ((yijing-journal-default-consultations-file "~/Documents/data/i-ching/app/yijing-q.csv")
       ;; (data (yijing-journal--entries-from-file yijing-journal-default-entries-file))
       (raw-entries (yijing-journal--load-consultations))
       (entries (yijing-journal-consultations))
       (uuid "6c3be66c-41f6-4c9e-8dd2-c5bd546a5145")
       (sample-alist (map-elt raw-entries uuid))
       (sample-entry (yijing-journal-find-consultation uuid))
       )
  ;;raw-entries
  sample-alist
  entries
  ;;(yijing-journal-consultation-p sample-entry)
  sample-entry
  )

(yijing-journal--csv-label-row '("something" "else"))

(seq-mapn (## cons %1 %2)
          yijing-journal-app-columns '("AAA" "BBBB"))
