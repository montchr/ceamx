;;; templates/org-mode.eld

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

(cdomat user-full-name " <" user-mail-address ">")

(litfile & "\
#+header-args: :tangle lisp/" (p "FEATURE-NAME" name) ".el
#+begin_src emacs-lisp :noweb yes :comments no
<<file-prop-line(feature=\"" (s name) "\",desc=\"" (p "FEATURE DESCRIPTION" desc) "\")>>

;; Copyright (c) " (format-time-string "%Y") "  " (i cdomat) "

;; Author: " (i cdomat) "
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

<<file-license>>

;;; Commentary:
;;; Code:

<<" (s name) ">>

<<file-footer(feature=\"" (s name) "\")>>
#+end_src


**** ~" (s name) "~: Code
:PROPERTIES:
:header-args: :noweb-ref " (s name) "
:END:

#+begin_src emacs-lisp


#+end_src

")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
