prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME: ")
(note (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE: ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO: ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG: ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK: ")
(xxx (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "XXX: ")
