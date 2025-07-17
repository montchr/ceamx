;; -*- lexical-binding: t;  -*-

(require 'ceamx-lib)
(require 'ceamx-ui)

;; Configure cursor appearance


(package! cursory
  (require 'cursory)

  (def-hook! ceamx-init-theme-cursory-h ()
    'ceamx-after-init-hook
    "Enable `cursory-mode' and load the latest preset."
    (cursory-mode 1)
    (cursory-set-preset (or (cursory-restore-latest-preset) 'box)))

  (setopt cursory-latest-state-file
          (expand-file-name "cursory-latest-state.eld" ceamx-var-dir))

  (setopt cursory-presets
          '((box
             :blink-cursor-interval 0.8)
            (box-no-blink
             :blink-cursor-mode -1)
            (bar
             :cursor-type (bar . 2)
             :blink-cursor-interval 0.8)
            (bar-no-other-window
             :inherit bar
             :cursor-in-non-selected-windows nil)
            (bar-no-blink
             :cursor-type (bar . 2)
             :blink-cursor-mode -1)
            (t
             :cursor-type box
             :cursor-in-non-selected-windows hollow
             :blink-cursor-mode 1
             :blink-cursor-blinks 10
             :blink-cursor-interval 0.2
             :blink-cursor-delay 0.2))))

;; Customize the Customization buffers and menus


(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

(add-hook 'Custom-mode-hook #'custom-toggle-hide-all-widgets nil t)

;; =grid=: textual data table presentation

;; - Source :: [[https://github.com/ichernyshovvv/grid.el][ichernyshovvv/grid.el]]
;; - Retrieved :: [2024-06-07 Fri 11:45]

;; #+begin_quote
;; This library allows you to put text data into boxes and align them horizontally,
;; applying margin, padding, borders.
;; #+end_quote


(package! (grid :host github :repo "ichernyshovvv/grid.el"))

;; =hydra=

;; - Documentation :: <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>


(package! hydra
  (require 'hydra))
(package! pretty-hydra)

;; =symbol-overlay= :: highlight symbols with keymap-enabled overlays


(package! symbol-overlay)

;; =sideline= :: show information on the side
;; :PROPERTIES:
;; :ID:       556ce67a-0d8b-4ef3-8f01-d34e65faeed6
;; :END:


(package! sideline
  (require 'sideline)

  (setopt sideline-backends-left-skip-current-line t
          sideline-backends-right-skip-current-line t)
  (setopt sideline-order-left 'down
          sideline-order-right 'up)
  (setopt sideline-format-left "%s   "
          sideline-format-right "   %s")
  (setopt sideline-priority 100)
  (setopt sideline-display-backend-name t))

;; Consider all themes "safe"


(setopt custom-safe-themes t)

;; =standard-themes= :: Prot's themes like the default but more consistent
;; :PROPERTIES:
;; :ID:       09f2005b-1d4d-4d92-9f36-2327ca55757d
;; :END:


(package! standard-themes
  (require 'standard-themes)

  (ceamx-ui-define-preferred-themes
   'standard 'standard-dark 'standard-light)

  (setopt standard-themes-bold-constructs t
          standard-themes-italic-constructs t)
  (setopt standard-themes-disable-other-themes t)
  (setopt standard-themes-mixed-fonts t
          standard-themes-variable-pitch-ui t)
  (setopt standard-themes-prompts '(extrabold italic))
  (setopt standard-themes-to-toggle (ceamx-ui-theme-family-preferred-themes 'standard))
  (setopt standard-themes-to-rotate
          '(standard-dark
            standard-dark-tinted
            standard-light
            standard-light-tinted)))

;; =modus-themes= :: Prot’s accessible themes conforming to WCAG AAA
;; :PROPERTIES:
;; :ID:       3700042e-1b73-416f-aaa1-2cbd9f2101f0
;; :END:

;; - Website :: <https://protesilaos.com/modus-themes/>


(package! modus-themes
  (require 'modus-themes)

  (ceamx-ui-define-preferred-themes
   'modus 'modus-vivendi 'modus-operandi)

  (setopt modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui t)
  (setopt modus-themes-to-toggle
          (ceamx-ui-theme-family-preferred-themes 'modus))
  (setopt modus-themes-disable-other-themes nil)
  (setopt modus-themes-custom-auto-reload t)
  (setopt modus-themes-headings nil)
  (setopt modus-themes-prompts '(italic bold))
  (setopt modus-themes-completions
          '((matches . (extrabold))
            (selection . (semibold italic text-also))))
  (setopt modus-themes-org-blocks 'tinted-background))

;; =ef-themes= :: Prot’s colorful yet legible themes
;; :PROPERTIES:
;; :ID:       3f512af0-7bad-4447-a14d-08a5371e14c5
;; :END:

;; - Website :: <https://protesilaos.com/emacs/ef-themes>


(package! ef-themes
  (require 'ef-themes)

  (ceamx-ui-define-preferred-themes
   'ef 'ef-winter 'ef-frost)

  (setopt ef-themes-to-toggle (ceamx-ui-theme-family-preferred-themes 'ef))
  (setopt ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui t))

;; =doric-themes= :: Prot’s minimalist themes
;; :PROPERTIES:
;; :ID:       d39feac9-ef27-41b0-94f9-92ed49b24055
;; :END:


(package! doric-themes
  (require 'doric-themes)

  (ceamx-ui-define-preferred-themes 'doric 'doric-dark 'doric-light)

  (setopt doric-themes-to-toggle (ceamx-ui-theme-family-preferred-themes 'doric))
  (setopt doric-themes-to-rotate doric-themes-collection))

;; Configure the preferred themes
;; :PROPERTIES:
;; :ID:       a21cb155-3353-4e2c-9593-1680b43f4575
;; :END:


(use-feature! ceamx-ui
  :config
  (setopt ceamx-ui-theme-family 'modus))

;; Set approximate stomping coordinates for hyper-astronomic relativity calculations


(require 'cal-dst)

(setopt calendar-latitude 39.968)
(setopt calendar-longitude -75.133)

;; =circadian= :: theme phasing based on sunrise/sunset time
;; :PROPERTIES:
;; :ID:       badbc4d6-c784-4936-bd84-e1312a67273c
;; :END:


(package! circadian
  (require 'ceamx-ui)

  (if (eq 'solar ceamx-ui-theme-circadian-interval)
      (progn
        (add-hook 'ceamx-after-init-hook #'circadian-setup)

        (setopt circadian-themes `((:sunrise . ,ceamx-ui-theme-light)
                                   (:sunset . ,ceamx-ui-theme-dark))))
    (add-hook 'ceamx-after-init-hook
              (lambda ()
                (if (ceamx-ui-desktop-dark-theme-p)
                    (ceamx-ui/load-dark-theme)
                  (ceamx-ui/load-light-theme))))))

;; =avy= :: can do anything
;; :PROPERTIES:
;; :ID:       05cb2761-07fc-476b-8b78-0d08e0a89469
;; :END:

;; + Package :: <https://github.com/abo-abo/avy>
;; + Article :: <https://karthinks.com/software/avy-can-do-anything/>


(package! avy
  (defer! 2 (require 'avy))

  (define-keymap :keymap (current-global-map)
    "C-;" #'avy-goto-char-timer
    ;; "C-'" is reserved for special commands e.g. `avy-org-goto-heading-timer'

    "M-g w" #'avy-goto-word-1)

  (after! org
    (keymap-set org-mode-map "C-'" #'avy-org-goto-heading-timer)
    (keymap-set org-mode-map "C-\"" #'avy-org-refile-as-child)))

(after! avy
  (setopt avy-style 'at-full)
  (setopt avy-all-windows t)
  (setopt avy-case-fold-search t)

  ;; Prevent conflicts with themes.
  (setopt avy-background nil)

  ;; Anything lower feels unusable.
  (setopt avy-timeout-seconds 0.25))

;; Highlight current line in programming modes


(add-hook 'prog-mode-hook #'hl-line-mode)

(after! hl-line
  ;; Disable line highlight in unfocused windows.
  (setopt hl-line-sticky-flag nil))

;; =lin= :: improve line-highlighting for major-modes orientated around line selection


(package! lin
  (add-hook 'ceamx-after-init-hook #'lin-global-mode))

;; =pulsar= :: pulse current line after function invocations


(package! pulsar
  (add-hook 'ceamx-after-init-hook #'pulsar-global-mode)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line))

(after! pulsar
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10)
  (setopt pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (dolist (fn '(pulsar-pulse-line-red
                pulsar-recenter-top
                pulsar-reveal-entry))
    (add-hook 'next-error-hook #'fn)))

;; Window highlighting


(setopt highlight-nonselected-windows nil)

;; Hide frame decorations
;; :PROPERTIES:
;; :ID:       c532e9e7-8729-4442-8bd7-2c50717a9dc7
;; :END:


(unless (ceamx-host-macos-p)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(when (ceamx-host-macos-p)
  ;; `undecorated-round' is macOS-specific.
  (add-to-list 'default-frame-alist '(undecorated-round . t))

  ;; GUI menu bar is necessary otherwise Emacs will be treated as a
  ;; non-application OS window (e.g. no focus capture).
  ;; <https://github.com/doomemacs/doomemacs/blob/d657be1744a1481dc4646d0b62d5ee1d3e75d1d8/lisp/doom-start.el#L118-L128>
  (def-hook! ceamx-frame--maybe-restore-gui-menu-bar-h (&optional frame)
    '(after-make-frame-functions window-setup-hook)
    "TODO: Provide source for this approach (Doom?), and why it does what it does."
    (when-let (frame (or frame (selected-frame)))
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'menu-bar-lines 1))))

  ;; Stop C-z from minimizing windows.
  (keymap-global-unset "C-z" t))

;; =spacious-padding= :: a comfortable layout density


(package! spacious-padding
  (add-hook 'ceamx-after-init-hook #'spacious-padding-mode))

(after! spacious-padding
  (setopt spacious-padding-widths
          '( :internal-border-width 10
             :header-line-width 4
             :mode-line-width 6
             :tab-width 4
             :right-divider-width 10
             :scroll-bar-width 4
             :left-fringe-width 6
             :right-fringe-width 6))

  (setopt spacious-padding-subtle-mode-line t))

;; =olivetti= :: "distraction-free" editing

;; - Package :: <https://github.com/rnkn/olivetti>


(package! olivetti
  (keymap-set ctl-x-x-map "o" #'olivetti-mode))

(after! olivetti
  (setopt olivetti-body-width 0.7
          olivetti-minimum-body-width 40
          olivetti-recall-visual-line-mode-entry-state t)
  (setopt olivetti-style 'fancy)

  (def-hook! +olivetti-mode-on--disable-conflicting-features-h ()
    '(olivetti-mode-on-hook)
    "Disable features that conflict with `olivetti-mode' appearance."
    (when (bound-and-true-p diff-hl-mode)
      (diff-hl-mode -1))))

;; =logos= :: a simple focus mode with page breaks or outlines :present:
;; :PROPERTIES:
;; :ID:       9f620970-a54b-46bf-bbc4-ad3712646506
;; :END:


(package! logos
  (define-keymap :keymap (current-global-map)
    "C-x n N" #'logos-narrow-dwim

    "C-x ]" #'logos-forward-page-dwim
    "C-x [" #'logos-backward-page-dwim)

    "M-]" #'logos-forward-page-dwim
    "M-[" #'logos-backward-page-dwim)

(after! logos
  (setopt logos-outlines-are-pages t)
  (setopt logos-outline-regexp-alist
          `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
            (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
            (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
            (conf-toml-mode . "^\\[")))

  ;; These apply buffer-locally when `logos-focus-mode' is enabled.
  (setq-default logos-hide-cursor t
                logos-hide-mode-line t
                logos-hide-header-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch t
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)

  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

  (def-hook! ceamx-essentials-logos-recenter-top-h ()
    '(logos-page-motion-hook)
    "Place point at the top when changing pages in non-`prog-mode' modes."
    (unless (derived-mode-p 'prog-mode)
      ;; NOTE: '0' value will recenter at the absolute top.
      (recenter 1))))

;; =focus= :: a lexical focus mode

;; + Package :: <https://github.com/larstvei/Focus>

;; Focusing by sentence, paragraph, or code block.


;; TODO: keybindings to C-c T z as prefix
(package! focus
  (define-keymap :keymap ceamx-toggle-prefix
    "Z" #'focus-mode))

;; =moc= :: "Master of Ceremonies" presentation utilities :present:

;; + Package :: <https://github.com/positron-solutions/moc/>


(package! (moc :host github :repo "positron-solutions/moc"))

;; Allow restoring deleted frames


(undelete-frame-mode 1)

;; Customize menu bar appearance :menubar:

;; Disable the menu bar by default:


(menu-bar-mode -1)

;; Customize tab bar appearance :tabs:

;; Enable the tab bar:


(tab-bar-mode 1)

(setopt tab-bar-auto-width t
        tab-bar-auto-width-max '((120) 20))

;; Text rendering and scaling


(setq x-underline-at-descent-line nil)

(setq-default text-scale-remap-header-line t)

(package! fontaine
  (when (display-graphic-p)
    (require 'fontaine)

    (setq fontaine-latest-state-file
          (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))

    ;; For some reason I do not yet understand, according to some
    ;; hearsay, font sizes best scale in multiples of 3-point
    ;; increments.
    (setq fontaine-presets
            `((tiny
               :bold-weight medium
               :default-height ,(pcase (system-name)
                                  (_ 78))
               :default-weight ,(pcase (system-name)
                                  (_ 'semilight)))

              (small
               :bold-weight semibold
               :default-height ,(pcase (system-name)
                                  (_ 90))
               :default-weight ,(pcase (system-name)
                                  (_ 'regular)))

              (regular
               :bold-weight semibold)

              (medium
               :default-height ,(pcase (system-name)
                                  ("boschic" 124)
                                  (_ 120)))

              (large
               :default-height ,(pcase (system-name)
                                  (_ 144)))

              (xlarge
               :default-height ,(pcase (system-name)
                                  (_ 156)))

              (big-mclarge-huge
               :default-weight semilight
               :default-height ,(pcase (system-name)
                                  (_ 180))
               :bold-weight extrabold)

              (t
               :default-family "Aporetic Sans Mono"
               ;; :default-family "Berkeley Mono"
               :default-weight regular
               :default-height ,(pcase (system-name)
                                  ("tuuvok" 102)
                                  ;; (_ 105)
                                  (_ 100))

               ;; :fixed-pitch-family "Aporetic Sans Mono"
               ;; :fixed-pitch-family "Berkeley Mono"
               :fixed-pitch-family nil
               :fixed-pitch-weight nil
               :fixed-pitch-height 1.0

               :fixed-pitch-serif-family nil
               :fixed-pitch-serif-weight nil
               :fixed-pitch-serif-height 1.0

               :variable-pitch-family "Aporetic Serif"
               ;; :variable-pitch-family nil
               :variable-pitch-weight nil
               :variable-pitch-height 1.0

               :mode-line-active-family nil
               :mode-line-active-weight nil
               :mode-line-active-height 0.8

               :mode-line-inactive-family nil
               :mode-line-inactive-weight nil
               :mode-line-inactive-height 0.8

               :header-line-family nil
               :header-line-weight nil
               :header-line-height 0.8

               :line-number-family nil
               :line-number-weight nil
               :line-number-height 0.8

               :tab-bar-family nil
               :tab-bar-weight nil
               :tab-bar-height 0.8

               :bold-family nil
               :bold-weight bold

               :italic-family nil
               :italic-weight nil
               :italic-slant italic

               :line-spacing nil)))

    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

    (fontaine-mode 1)))

;; ~ligature.el~ :: improved ligature support
;; :PROPERTIES:
;; :ID:       faf642cc-811e-4b58-bbaa-51e9e11b1dff
;; :END:

;; + Package :: <https://github.com/mickeynp/ligature.el>

;; A better implementation of ligature support than the builtin
;; ~prettify-symbols-mode~.

;; <https://old.reddit.com/r/emacs/comments/keji66/what_is_bad_about_prettifysymbolsmode/>


(package! ligature
  (when (display-graphic-p)
    (after! fontaine
      (global-ligature-mode 1))))

(after! ligature
  ;; Enable all Iosevka ligatures in programming modes.
  ;; <https://github.com/mickeynp/ligature.el/wiki#iosevka>
  (ligature-set-ligatures
   'prog-mode
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
     "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">="
     "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>"
     "::" ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>"
     "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

;; =show-font= :: a tool to preview fonts

;; + Package :: <https://protesilaos.com/emacs/show-font>


(when (display-graphic-p)
  (package! show-font)

  (after! show-font
    (setopt show-font-pangram 'ceamx)
    (setopt show-font-character-sample
            "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
")))

;; Provide common dependency: ~nerd-icons~ :package:
;; :PROPERTIES:
;; :ID:       0eb3c103-ba4b-40ee-9341-e5cab30e075f
;; :END:


(package! nerd-icons
  (require 'nerd-icons)

  (keymap-set ceamx-insert-prefix-map "I" '("icon" . nerd-icons-insert))

  (define-keymap :keymap (current-global-map)
    "C-x 8 i" (cons "icons" (define-prefix-command 'ceamx-insert-icons-prefix 'ceamx-insert-icons-prefix-map))
    "C-x 8 i i" #'nerd-icons-insert))

(after! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Provide common dependency: ~svg-lib~ :package:


(package! svg-lib)

;; =page-break-lines= :: improve appearance of form feed characters :package:

;; - docs :: <https://github.com/purcell/page-break-lines/blob/master/README.md>


(package! page-break-lines
  (add-hook 'ceamx-after-init-hook #'global-page-break-lines-mode))

(provide 'ceamx-init-ui)
;;; ceamx-init-ui.el ends here
