;;; templates/tempel/org-mode.eld

;;; Sources:
;; <https://github.com/Crandel/tempel-collection>
;; <https://github.com/minad/tempel/blob/5b09f612cfd805dba5e90bf06580583cab045499/README.org#template-file-format>

org-mode

(lel & "#+begin_src emacs-lisp :tangle lisp/init-" p ".el" n r n "#+end_src")
(lelc & "#+begin_src emacs-lisp :tangle lisp/config-" p ".el" n r n "#+end_src")
(lell & "#+begin_src emacs-lisp :tangle lisp/lib-" p ".el" n r n "#+end_src")

(src & "#+begin_src " p n r n "#+end_src" :post (org-edit-src-code))
(elisp & "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(elsp & "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))

(readmecollapse  "*** " (p "Heading") n "#+HTML: <details> <summary> " (p "sub-heading")  " </summary>" n (r> "link or any comments") n n "#+HTML: </details>" n)

(caption & "#+caption: ")
(drawer & ":" p ":" n r ":end:")
(begin & "#+begin_" (s name) n r n "#+end_" name)
(quote & "#+begin_quote" n> r> n "#+end_quote")
(sidenote & "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote & "#+begin_marginnote" n> r> n "#+end_marginnote")
(example & "#+begin_example" n> r> n "#+end_example")
(center & "#+begin_center" n> r> n "#+end_center")
(ascii & "#+begin_export ascii" n> r> n "#+end_export")
(html & "#+begin_export html" n> r> n "#+end_export")
(comment & "#+begin_comment" n> r> n "#+end_comment")
(verse & "#+begin_verse" n> r> n "#+end_verse")
(inlsrc "src_" p "{" q "}")
(title & "#+title: " p n "#+author: " p n "#+language: " p n n)

(litfile & "\
** =lisp/" (p "FEATURE-NAME" name) ".el=

#+name: " name "-file
#+begin_src emacs-lisp :comments no :tangle lisp/" name ".el
<<file-prop-line(feature=\"" name "\",desc=\"" (p "FEATURE DESCRIPTION" desc) "\")>>

;; Copyright (c) " (p "2022-") (format-time-string "%Y") "  " (i cdomat) "

;; Author: " (i cdomat) "
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

<<file-license>>

;;; Commentary:
;;; Code:

<<" name ">>

<<file-footer(feature=\"" name "\")>>
#+end_src

")

(elsrcref "#+begin_src emacs-lisp :noweb-ref " (p (or noweb-ref "NOWEB-REF")) n r n "#+end_src" n)

(litpathvar "#+begin_src emacs-lisp :noweb-ref config-feature-paths" n r n "#+end_src" n)
(litlib "#+begin_src emacs-lisp :noweb-ref lib-" (p "FEATURE-NAME") n r n "#+end_src" n)
;; TODO: use yasnippet -- this is not possible with tempel
;; (litlib2 (p "FEATURE-NAME" feature t)
;;          (let ((noweb-ref (concat "lib" feature)))
;;                                         (i elsrcref)))


;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
