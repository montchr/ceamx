;;; templates/tempel/php-mode.eld

;; Adapted from:
;; <https://github.com/AndreaCrotti/yasnippet-snippets/tree/6fafad13bb4689600285d9e38c61958dd63c356d/snippets/php-mode>
;; The new templates share the same name as the original unless otherwise noted.

php-mode php-ts-mode

(cls & "class " (p (f-base (buffer-file-name))) n "{" r> n "}")
(define & "define('" (p "NAME") "', " p ");")
(enum & "enum " (p "Name") (p ": type") " {" n> "case: " (p "One") ";" n> "case: " (p "Two") ";" n r> n "}" %)
(fore & "foreach (" p " as $" (p "value") ") {" r> n "}")
;; orig: foreach_value
(forek & "foreach (" p " as $" (p "key") " => $" (p "value") ") {" r> n "}")
(fun & (p "public ") "function " (p "name") "(" (p "type") " $" (p "arg") ")" n "{" r> "}")
(iface & "interface " (p (f-base (buffer-file-name))) n "{" r> n "}")
(match & "match (" (p "$var") ") {" n> (p "$cond") " => " (p "$ret") "," n r> n "};")
(docparam & " * @param " (p "type") " $" (p "name") " " q n> "*" q)
(docreturn & " * @return " (p "type") " " (r "description"))
(strict & "declare(strict_types=1);" n)
(vd & "var_dump(" (r "$var") ");")
(vde & "var_dump(" (r "$var") ");exit(1);")
(ve & "var_export(" (r "$var") (p ", true") ");")

;; FIXME: indentation:
(switch & "switch (" p ") {" n> "case " p ":" n> p n> "break;" n > "default:" n> r n "}")
;; FIXME: indentation:
(set & "\
public function set" (p "NAME: " name t) (upcase-initials name) "(" (p "type") " $" name  ")
{
" > "$this->" name " = $" name ";
}
")

;; orig: "doc-comment-multiline"
;; FIXME: `indent-according-to-mode' does not line up with the first star
;; (phpdoc "/**" n> "* " (p "SUMMARY") n> "* " n> "* " q n> "*/" %)
;; FIXME: indents properly only at BOL
(phpdoc "\
/**
 * " (p "SUMMARY") "
 *
 * " p "
 */")
;; orig: doc-comment-oneline
(phpdoci & "/** @var " (p "TYPE") " " p " */" %)

(phpcsignore & "// phpcs:ignore " p)
(phpcsdisable (p "Sniffs (comma-separated): " sniffs t)
              (p "Reason: " reason) &
              "// phpcs:disable " sniffs " -- " reason n
              r
              "// phpcs:enable" n)


(ext "extends " (p "ParentClass"))
(impl "implements " p)

;; Local Variables:
;; outline-regexp: "[a-z]"
;; End:
