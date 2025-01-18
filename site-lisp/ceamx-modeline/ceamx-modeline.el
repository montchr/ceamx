;;; ceamx-modeline.el --- Ceamx Modeline             -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Chris Montgomery
;; Copyright (C) 2023-2024  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;;         Chris Montgomery <chmont@proton.me>
;; Keywords: local, faces, frames, convenience

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

;; Custom mode-line.

;;;; Sources:

;; <https://github.com/protesilaos/dotfiles/blob/9401dbaa308296dd5d9b863d5385d0666d91727e/emacs/.emacs.d/prot-lisp/prot-modeline.el>

;; (info "(elisp) Mode Line Format")


;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'subr-x))

(require 'ceamx-lib)

;;;; Variables

(defvar ceamx-modeline--original-format nil
  "The original modeline format prior to enabling `ceamx-modeline-mode'.")

;;;; Customizations

(defgroup ceamx-modeline nil
  "A minor-mode modeline from Ceamx."
  :group 'mode-line)

(defgroup ceamx-modeline-faces nil
  "Faces for the Ceamx modeline."
  :group 'ceamx-modeline)

(defcustom ceamx-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defcustom ceamx-modeline-format
  '("%e"
     ceamx-modeline-kbd-macro
     ceamx-modeline-narrow
     ceamx-modeline-buffer-status
     ceamx-modeline-window-dedicated-status
     "  "
     ceamx-modeline-buffer-identification
     "  "
     ceamx-modeline-major-mode
     ceamx-modeline-process
     "  "
     ceamx-modeline-vc-branch
     "  "
     ceamx-modeline-eglot
     "  "
     ;; ceamx-modeline-flymake
     "  "
     mode-line-format-right-align   ; Emacs 30
     ;; ceamx-modeline-notmuch-indicator
     "  "
     ceamx-modeline-misc-info)
  "Modeline format for `ceamx-modeline-mode'."
  :type '(string symbol))

;;;; Faces

(defface ceamx-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defface ceamx-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-red-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-green-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-yellow-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-blue-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-magenta-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-cyan-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-gray
  '((t :inherit shadow))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ceamx-modeline-faces)

(defface ceamx-modeline-indicator-gray-bg
  '((default :inherit (bold ceamx-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#808080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a0a0a0" :foreground "black")
    (t :inverse-video t))
  "Face for modeline indicatovrs with a background."
  :group 'ceamx-modeline-faces)


;;;; Indicators

(defvar-local ceamx-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'ceamx-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar-local ceamx-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'ceamx-modeline-indicator-cyan-bg)))
  "Mode line construct to report the narrowed state of the current buffer.")

;; TODO 2023-07-05: What else is there beside remote files?  If
;; nothing, this must be renamed accordingly.
(defvar-local ceamx-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    'face 'ceamx-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

(defvar-local ceamx-modeline-window-dedicated-status
    '(:eval
      (when (window-dedicated-p)
        (propertize " = "
                    'face 'ceamx-modeline-indicator-gray-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for dedicated window indicator.")

;;;;;; Buffer identification

(defvar-local ceamx-modeline-major-mode
    (list
     (propertize "%[" 'face 'ceamx-modeline-indicator-red)
     '(:eval
       (concat
        (ceamx-modeline-major-mode-indicator)
        " "
        (propertize
         (ceamx-modeline-string-abbreviate-but-last
          (ceamx-modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (ceamx-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'ceamx-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local ceamx-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

(defun ceamx-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `ceamx-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
      ((and (mode-line-window-selected-p)
         file
         (buffer-modified-p))
        '(italic mode-line-buffer-id))
      ((and file (buffer-modified-p))
        'italic)
      ((mode-line-window-selected-p)
        'mode-line-buffer-id))))

(defun ceamx-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `ceamx-modeline-string-cut-middle'."
  (when-let* ((name (buffer-name)))
    (ceamx-modeline-string-cut-middle name)))

(defun ceamx-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (ceamx-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun ceamx-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `ceamx-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local ceamx-modeline-buffer-identification
    '(:eval
      (propertize (ceamx-modeline-buffer-name)
                  'face (ceamx-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (ceamx-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;;;; Major mode

(defun ceamx-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun ceamx-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun ceamx-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `ceamx-modeline-major-mode'."
  (if-let* ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local ceamx-modeline-major-mode
    (list
     (propertize "%[" 'face 'ceamx-modeline-indicator-red)
     '(:eval
       (concat
        (ceamx-modeline-major-mode-indicator)
        " "
        (propertize
         (ceamx-modeline-string-abbreviate-but-last
          (ceamx-modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (ceamx-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'ceamx-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local ceamx-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;;;; Version control

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun ceamx-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let* ((rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file)
                          (substring rev 0 7))))
    (capitalize branch)))

(defvar-keymap ceamx-modeline-vc-map
  :doc "Keymap to display on VC indicator."
  "<mode-line> <down-mouse-1>" #'vc-diff
  "<mode-line> <down-mouse-3>" #'vc-root-diff)

(defun ceamx-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun ceamx-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (ceamx-modeline--vc-help-echo file)
               'local-map ceamx-modeline-vc-map)
   ;; " "
   ;; (ceamx-modeline-diffstat file)
   ))

(defun ceamx-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (ceamx-modeline-string-cut-end
   (ceamx-modeline--vc-text file branch face)))

(defvar ceamx-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun ceamx-modeline--vc-get-face (key)
  "Get face from KEY in `ceamx-modeline--vc-faces'."
  (alist-get key ceamx-modeline--vc-faces 'up-to-date))

(defun ceamx-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (ceamx-modeline--vc-get-face (vc-state file backend)))

(defvar-local ceamx-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (ceamx-modeline--vc-branch-name file backend))
                  (face (ceamx-modeline--vc-face file backend)))
        (ceamx-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;;;; TODO Flymake & Flycheck

;;;;;; Eglot

;; FIXME: handle this in `ceamx-modeline-mode-hook'
(with-eval-after-load 'eglot
  (setq mode-line-misc-info
    (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local ceamx-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;;;; Miscellaneous

(defvar-local ceamx-modeline-notmuch-indicator
    '(notmuch-indicator-mode
      (" "
       (:eval (when (mode-line-window-selected-p)
                notmuch-indicator--counters))))
  "The equivalent of `notmuch-indicator-mode-line-construct'.
Display the indicator only on the focused window's mode line.")

(defvar-local ceamx-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;;; Functions

;; (defun ceamx-modeline-format ()
;;   "Return the value of the `ceamx-modeline' format."
;;   `("%e"
;;      mode-line-front-space
;;      (:propertize (""
;;                     mode-line-mule-info
;;                     mode-line-client
;;                     mode-line-modified
;;                     mode-line-remote)
;;        display
;;        (min-width (5.0)))
;;      mode-line-frame-identification
;;      ;; mode-line-buffer-identification
;;      (:eval (breadcrumb-project-crumbs))
;;      "   "
;;      mode-line-position
;;      (vc-mode vc-mode)
;;      "  "
;;      mode-line-modes
;;      mode-line-misc-info
;;      mode-line-end-spaces))


;;;;; Helpers

(defun ceamx-modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`ceamx-modeline-string-truncate-length'."
  (if (ceamx-modeline--string-truncate-p str)
    (ceamx-string-truncate-right str ceamx-modeline-string-truncate-length)
    str))

(defun ceamx-modeline-string-cut-beginning (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`ceamx-modeline-string-truncate-length'."
  (if (ceamx-modeline--string-truncate-p str)
    (string-truncate-left str ceamx-modeline-string-truncate-length)
    str))

(defun ceamx-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`ceamx-modeline-string-truncate-length' both from its beginning
and end."
  (if (ceamx-modeline--string-truncate-p str)
    (ceamx-string-truncate-middle str ceamx-modeline-string-truncate-length)
    str))

(defun ceamx-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `ceamx-modeline-string-abbreviate-but-last'."
  (if (ceamx-modeline--string-truncate-p str)
    (mapconcat #'ceamx-string-first-char (split-string str "[_-]") "-")
    str))

(defun ceamx-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `ceamx-modeline-string-abbreviate'."
  (if (ceamx-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'ceamx-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

(defun ceamx-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (cond
    ((or (not (stringp str))
       (string-blank-p str))
      nil)
    ((and (ceamx-window-narrow-p)
       (> (length str) ceamx-modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))))

(defun ceamx-modeline--truncate-p ()
  "Return non-nil if truncation should happen.
This is a more general and less stringent variant of
`ceamx-modeline--string-truncate-p'."
  (and (ceamx-window-narrow-p)
       (not (one-window-p :no-minibuffer))))


;;;; Risky local variables

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '(ceamx-modeline-kbd-macro
                     ceamx-modeline-narrow
                     ceamx-modeline-buffer-status
                     ceamx-modeline-window-dedicated-status
                     ceamx-modeline-buffer-identification
                     ceamx-modeline-major-mode
                     ceamx-modeline-process
                     ceamx-modeline-vc-branch
                     ;; ceamx-modeline-flymake
                     ceamx-modeline-eglot
                     ;; ceamx-modeline-align-right
                     ;; ceamx-modeline-notmuch-indicator
                     ceamx-modeline-misc-info))
  (put construct 'risky-local-variable t))

;;;; Mode

;; FIXME: toggling does not actually change the format!  this is
;; because `mode-line-format' is set in user config.  but setting it
;; in `ceamx-modeline' resulted in a blank modeline...?

;;;###autoload
(define-minor-mode ceamx-modeline-mode
  "Ceamx custom modeline mode."
  :group 'ceamx-modeline
  :global t
  (if ceamx-modeline-mode
    (progn
      (setq ceamx-modeline--original-format (default-value 'mode-line-format))
      (setq mode-line-right-align-edge 'right-margin)
      ;; (setq-default mode-line-format ceamx-modeline-format)
      )
    (progn
      (setq mode-line-right-align-edge
        (default-value 'mode-line-right-align-edge))
      ;; (setq-default mode-line-format ceamx-modeline--original-format)
      )))

;;; Footer:

(provide 'ceamx-modeline)
;;; ceamx-modeline.el ends here
