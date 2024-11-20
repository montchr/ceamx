;;; init-outline.el --- Customizations for outline structures  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(after! (repeat outline)
  (define-keymap :keymap outline-navigation-repeat-map
    "C-x" #'foldout-exit-fold
    "x" #'foldout-exit-fold
    "C-z" #'foldout-zoom-subtree
    "z" #'foldout-zoom-subtree
    "C-a" #'outline-show-all
    "a" #'outline-show-all
    "C-c" #'outline-hide-entry
    "c" #'outline-hide-entry
    "C-d" #'outline-hide-subtree
    "C-e" #'outline-show-entry
    "e" #'outline-show-entry
    "TAB" #'outline-show-children
    "C-k" #'outline-show-branches
    "k" #'outline-show-branches
    "C-l" #'outline-hide-leaves
    "l" #'outline-hide-leaves
    "RET" #'outline-insert-heading
    "C-o" #'outline-hide-other
    "o" #'outline-hide-other
    "C-q" #'outline-hide-sublevels
    "q" #'outline-hide-sublevels
    "C-s" #'outline-show-subtree
    "s" #'outline-show-subtree
    "C-t" #'outline-hide-body
    "t" #'outline-hide-body
    "@" #'outline-mark-subtree)

  (ceamx-repeatify-keymap 'outline-navigation-repeat-map))
;; (after! (transient outline)
;;   (transient-define-prefix ceamx/outline-dispatch ()
;;     "Outline navigation transient menu."
;;     [["Navigate"
;;       ("u" "up" outline-up-heading)
;;       ("n" "next" outline-next-visible-heading)
;;       ("p" "prev" outline-previous-visible-heading)
;;       ("f" "forward" outline-forward-same-level)
;;       ("b" "backward" outline-backward-same-level)]]))

;; (after! (hydra outline)
;;   (defhydra ceamx/outline-hydra ( :color red)
;;     "
;; ^Navigate^            ^Subtree^        ^Metadata^
;; ^--------^----------  ^-------^-----  ^---------^--
;; _n_ext visible        _I_: drag up    _t_odo-state
;; _p_revious visible    _J_: promote    _d_eadline
;; _f_orward same level  _K_: drag down  _s_chedule
;; _b_ack same level     _L_: demote
;; _u_p level            _N_: narrow     _xp_: set property
;;                       _W_: widen
;; "))

(provide 'init-outline)
;;; init-outline.el ends here
