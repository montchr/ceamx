;;; init-packages.el --- Package management setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

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

;; Initialize package.el and `use-package' (from bundled version).

;; For more info:
;; - Info node `(info "(elisp) Packaging Basics")'
;; - Info node `(info "(emacs) Package Installation")'


;;;; Managing package initialization

;; Configuration for the package installation process must happen in early-init
;; (if `package-enable-at-startup' is non-nil) or prior to package
;; initialization (e.g. with `package-initialize'). That means, for example,
;; configuring `package-archives' and the like.
;;
;; Packages are not automatically made available if `package-enable-at-startup'
;; is set to ‘nil’ in the early init file, so `package-initialize' will need to be
;; called later during init.
;;
;; `package-initialize' will call `package-activate-all', meaning that installed
;; packages will initialize autoloads, set up customizable options, and then be
;; byte-compiled. Then the package directory will be added to `load-path'. The
;; same process will occur when `package-enable-at-startup' is non-nil.
;;
;; TODO: confirm (this doesn't sound quite right, but it's a little bit of a
;; mess): When `package-enable-at-startup' is nil *and* `package-initialize' is
;; never called, then none of the packages will be available for use whatsoever,
;; unless this process is managed manually, e.g. by `use-package'.

;; TODO: look into `package-quickstart', which might provide some performance
;; improvements, but at the cost of more-finicky configuration.
;;
;; <https://github.com/redguardtoo/emacs.d/blob/ce199b63b431af400d3bca6c9fca3f1772c5272c/lisp/init-elpa.el>
;; is one example of using `package-quickstart', with some context. However, it
;; indicates that some settings will be ignored, like `package-user-dir', which
;; we don't want.


;;;; Managing selected packages

;; `package-selected-packages' must contain the list of packages selected for
;; installation when 1) `package-enable-at-startup' is non-nil or 2)
;; `package-initialize' is never called.
;;
;; `package-selected-packages' will be populated automatically when
;; `package-enable-at-startup' is non-nil or when calling `package-initialize'.
;; The value of `package-selected-packages' will then, by default, be written to
;; `custom-file', which is not ideal.
;;
;; The `custom-file' behavior is not currently changeable by any configuration
;; settings. This limitation has been reported as an Emacs bug at least twice:
;;
;; - <https://lists.gnu.org/archive/html/bug-gnu-emacs/2022-09/msg00116.html>
;; - <https://lists.gnu.org/archive/html/emacs-devel/2016-02/msg00958.html>


;;;; `use-package' `:ensure' keyword and `package' oddities

;; DISCLAIMER: I might be misreading these reports, as I haven't yet encountered
;; these issues (but perhaps only because of lack of experience using package.el).

;; Surprisingly, when calling the `use-package' macro with a non-nil value for
;; `:ensure' or when `use-package-always-ensure' is non-nil,
;; `package-selected-packages' is *NOT* managed, which means that the "ensured"
;; packages will never be installed unless `package-selected-packages' is
;; populated either by one of the previously-mentioned methods, or manually.
;;
;; While I havent confirmed the following, it is noted somewhat exhastively in a few places:
;;
;; - <https://old.reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/>
;; - <https://github.com/jwiegley/use-package/issues/870>
;; - <https://github.com/jwiegley/use-package/issues/414>
;; - <https://github.com/jwiegley/use-package/issues/397>
;;
;; In a sense, by ignoring `package-selected-packages' entirely, ensuring with
;; `use-package' ironically results in imperative package installation,
;; installing and activating packages regardless of whether any mention of such
;; a package is removed from the init files entirely, leaving seemingly
;; "deleted" packages lying around, even with `package-autoremove'.
;;
;; <https://github.com/jwiegley/use-package/issues/870> has the most up-to-date
;; info on the issue, and it is still open as of this writing. It seems that
;; over time, the state of issues with `package-selected-packages' and
;; `package-autoremove' has fluctuated from one fix to another (same goes for
;; Emacs versions).

;;; Links:

;; <https://old.reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/>

;;; Code:

(require 'cl-lib)

(require 'ceamx-paths)
(require 'lib-common)

(require 'package)

;;;; Handle `package-selected-packages' oddities:

;; From @purcell:
;;
;; "package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete."

;; via <https://github.com/purcell/emacs.d/blob/45dc1f21cce59d6f5d61364ff56943d42c8b8ba7/lisp/init-elpa.el#L69-L86>
(defvar ceamx-selected-packages nil
  "Track the packages installed by `ceamx-require-package'.
This aims to avoid potential issues with
`package-selected-packages'.")

(defadvice! ceamx-note-selected-package-a (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note that fact.
The package name is noted by adding it to
`ceamx-selected-packages'.

This function is used as an advice for `ceamx-require-package',
to which ARGS are passed. When the behavior of
`package-selected-packages' is handled in a sane way in some
future Emacs release, this advice can hopefully be removed."
  :around 'ceamx-require-package
  (let ((available (apply oldfun package args)))
    (prog1
      available
      (when available
        (add-to-list 'ceamx-selected-packages package)))))

;;; Merge selected package lists.
(def-hook! ceamx-merge-selected-package-lists-a () 'after-init-hook
  "Merge the `package-selected-packages' and
`ceamx-selected-packages' lists."
  (package--save-selected-packages
    (seq-uniq (append ceamx-selected-packages package-selected-packages))))


;;;; Prevent `seq' dependency hell:

;; Work around an issue in Emacs 29 where seq gets implicitly reinstalled via
;; the rg -> transient dependency chain, but fails to reload cleanly due to not
;; finding seq-25.el, breaking first-time start-up.
;;
;; Source: <https://github.com/purcell/emacs.d/blob/45dc1f21cce59d6f5d61364ff56943d42c8b8ba7/lisp/init-elpa.el#L89-L100>
;; See: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67025>
(when (version= (number-to-string emacs-major-version) "29")
  (defadvice! ceamx-reload-previously-loaded-with-updated-load-path-a (orig pkg-desc)
    "Update the ORIG load-path with PKG-DESC directories.
Intended as a workaround for `seq' dependency hell caused by
recent `magit' changes."
    :around 'package--reload-previously-loaded
    (let ((load-path (cons (package-desc-dir pkg-desc) load-path)))
      (funcall orig pkg-desc))))

;;;; Pre-initialization settings:

(setopt package-native-compile t)

;; Allow upgrading of builtin packages available in ELPA.
;;
;; This is *required* for `magit' currently, as it loads a version of `seq'
;; which is only available in Emacs 30.
(setopt package-install-upgrade-built-in t)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setopt package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0).
(setopt package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; Emacs 30+
(when (boundp 'package-vc-register-as-project)
  (setq package-vc-register-as-project nil))

;;
;;; Initialize packages:

;; When `package-enable-at-startup' is nil, `package-initialize' must be called
;; before `require'ing any packages installed using `package'. It should *not*
;; be called if `package-enable-at-startup' is non-nil, because it would cause a
;; second initialization (which, I think, will throw a warning).
(unless package-enable-at-startup
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'seq '(2 25))
  (package-install 'seq))

(eval-when-compile
  (require 'use-package))

(setopt use-package-always-ensure t)

;;;; Improve `use-package' debuggability if necessary:

(setopt use-package-expand-minimally nil)
(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

;;;; Initialize packages adding `use-package' keywords:

;; NOTE: `blackout' is still useful even without `use-package'
(use-package blackout
  :demand t)

(provide 'init-packages)
;;; init-packages.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
