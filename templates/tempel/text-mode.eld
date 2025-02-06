text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))
(table (p (read-number "Rows: ") rows noinsert)
       (p (read-number "Cols: ") cols noinsert)
       "| " (p "  ") (* (1- cols) " | " (p "  ")) " |" n
       "|" (* cols "----|") n
       (* rows "| " (p "  ") (* (1- cols) " | " (p "  ")) " |" n))
