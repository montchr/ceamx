;;; init-packages.el --- Package management setup    -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
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

;;; Workarounds

;;;; Handle `package-selected-packages' oddities

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

(def-advice! +package--note-selected-package-a (oldfun package &rest args)
  :around #'ceamx-require-package
  "Note whether OLDFUN reports PACKAGE was successfully installed.
The PACKAGE name is noted by adding it to
`ceamx-selected-packages'.

When the behavior of `package-selected-packages' is handled in a
sane way in some future Emacs release, this advice can hopefully
be removed."
  (let ((available (apply oldfun package args)))
    (prog1
      available
      (when available
        (add-to-list 'ceamx-selected-packages package)))))

(def-hook! +package--merge-selected-package-lists-h ()
  'after-init-hook
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
  (def-advice! ceamx-reload-previously-loaded-with-updated-load-path-a (orig pkg-desc)
    :around #'package--reload-previously-loaded
    "Update the ORIG load-path with PKG-DESC directories.
Intended as a workaround for `seq' dependency hell caused by
recent `magit' changes."
    (let ((load-path (cons (package-desc-dir pkg-desc) load-path)))
      (funcall orig pkg-desc))))

;;; Settings

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
  '(("melpa" . 3)
    ("gnu-elpa" . 2)
    ("nongnu" . 1)))

;; Emacs 30+
(when (boundp 'package-vc-register-as-project)
  (setq package-vc-register-as-project nil))

;;; Initialize

;; When `package-enable-at-startup' is nil, `package-initialize' must be called
;; before `require'ing any packages installed using `package'. It should *not*
;; be called if `package-enable-at-startup' is non-nil, because it would cause a
;; second initialization (which, I think, will throw a warning).
(unless package-enable-at-startup
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;;; Essential Dependencies

;;;; `auto-compile' :: <https://github.com/emacscollective/auto-compile>

;; > Starting with Emacs version 24.4, setting `load-prefer-newer` to `t`
;; > prevents outdated byte code files from being loaded. However this does not
;; > cause re-compilation of the source file, to actually do that
;; > `auto-compile-on-load-mode` is still required.

;; This issue will likely present itself every time a package is upgraded...

(unless (package-installed-p 'auto-compile)
  (package-install 'auto-compile))

(require 'auto-compile)
(auto-compile-on-load-mode)
;; NOTE: This will not affect user init files as long as `no-byte-compile' is
;; non-nil in `early-init' or `init'. It is a best practice to include this
;; file-local variable in all user init files (which `autoinsert' will take care
;; of by default).
(auto-compile-on-save-mode)

;;; `seq' [builtin]

;; Ensure we load the latest version of `seq' available in GNU ELPA, which is
;; required as a `magit' dependency in Emacs 29 and lower. We are better off
;; installing this updated version as early as possible, since will likely be
;; used by other packages/libraries before `magit' loads.

(unless (package-installed-p 'seq '(2 25))
  (package-install 'seq))

;;; `use-package' [builtin]

(eval-when-compile
  (require 'use-package))

(setopt use-package-always-ensure t)

;;;; Improve `use-package' debuggability if necessary

(setopt use-package-expand-minimally nil)
(when (bound-and-true-p init-file-debug)
  (require 'use-package)
  (setopt use-package-expand-minimally nil)
  (setopt use-package-verbose t)
  (setopt use-package-compute-statistics t))

;;; Essential storage path cleanup for features/packages

(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory ceamx-etc-dir)
  (setq no-littering-var-directory ceamx-var-dir))

(elpaca-wait)

;;; Use `quelpa' for managing Elisp packages from source

;; <https://github.com/quelpa/quelpa>

;; Some packages are not published to the various package repositories. Until
;; `package-vc-install' is usable for this purpose (in Emacs 29, it's terrible),
;; `quelpa' is the best option when using Emacs' builtin package manager.
;; Otherwise, `straight' and `elpaca' have their own approaches to this problem.

;; Note that quelpa uses its own commands, separate from the package.el
;; commands. See `quelpa-upgrade', `quelpa-upgrade-all', and `quelpa'. By
;; default, upgrades will be disabled entirely.
;; <https://github.com/quelpa/quelpa?tab=readme-ov-file#upgrading-packages>

;; NOTE: quelpa boolean settings seem to misuse the "-p" suffix, which is
;; conventionally intended for predicate functions only.

(use-package quelpa
  :demand t
  :preface
  ;; By default, Quelpa updates a local clone of the MELPA repo on every session
  ;; startup, which causes a very-noticeable delay. We have no interest in using
  ;; Quelpa for working with MELPA repos, so disable this entirely before load.
  ;; <https://github.com/quelpa/quelpa?tab=readme-ov-file#prevent-updating-of-melpa-repo-on-emacs-startup>
  (setq quelpa-checkout-melpa-p nil)
  ;; Misbehaving...
  (setq quelpa-dir (expand-file-name (ceamx-format-version-subdir "quelpa")
                     ceamx-local-dir)))

;; Add the use-package integration and `:quelpa' keyword.
(use-package quelpa-use-package
  :demand t
  :functions (quelpa-use-package-activate-advice)
  :config
  ;; Prevent catastrophic conflicts with `:ensure'.
  (quelpa-use-package-activate-advice))

;;; Initialize miscellaneous packages adding `use-package' keywords

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
