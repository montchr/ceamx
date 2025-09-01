;; -*- lexical-binding: t;  -*-

(require 'ceamx-paths)
(require 'ceamx-lib)

;; Configure message encyption


(setq! mm-encrypt-option nil
       mm-sign-option nil)

(setq! mml-secure-openpgp-encrypt-to-self t
       mml-secure-openpgp-sign-with-sender t
       mml-secure-smime-encrypt-to-self t
       mml-secure-smime-sign-with-sender t)

;; Configure message composition (=message.el=)


(after! message
  (add-hook 'message-setup-hook #'message-sort-headers))

(setq! mail-user-agent 'message-user-agent
       message-mail-user-agent t)
(setq! mail-header-separator "-- text follows this line --")
(setq! message-elide-ellipsis "\n> [... %l lines elided]\n")
(setq! compose-mail-user-agent-warnings nil)
(setq! message-signature "Chris Montgomery"
       mail-signature message-signature)
(setq! message-citation-line-function #'message-insert-formatted-citation-line)
(setq! message-citation-line-format (concat "> From: %f\n"
                                            "> Date: %a, %e %b %Y %T %z\n"
                                            ">")
       message-ignored-cited-headers "")
(setq! message-confirm-send t)
(setq! message-kill-buffer-on-exit t)
;; Always reply-all.
(setq! message-wide-reply-confirm-recipients nil)

;; Configure Dired integration for email attachments (~gnus-dired-mode~)


(after! dired
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode))

;; =notmuch= :: just an email system :package:requires_external:


(package! notmuch
  (setq! notmuch-show-logo nil
         notmuch-column-control 1.0
         notmuch-hello-auto-refresh t
         notmuch-hello-recent-searches-max 20
         notmuch-hello-thousands-separator ""
         ;; notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
         notmuch-show-all-tags-list t)

;;;;; Searches

  (setq! notmuch-search-oldest-first nil)
  (setq! notmuch-show-empty-saved-searches t)
  (setq! notmuch-saved-searches
         '(( :name "inbox"
             :query "tag:inbox"
             :sort-order newest-first
             :key "i")
           ( :name "unread"
             :query "tag:unread and tag:inbox"
             :sort-order newest-first
             :key "u")))

;;;;; Composing Messages

  (setq! notmuch-mua-compose-in 'current-window)
  (setq! notmuch-mua-hidden-headers nil)
  (setq! notmuch-address-command 'internal)
  (setq! notmuch-address-use-company nil)
  (setq! notmuch-always-prompt-for-sender t)
  (setq! notmuch-mua-cite-function #'message-cite-original-without-signature)
  (setq! notmuch-mua-reply-insert-header-p-function #'notmuch-show-reply-insert-header-p-never)
  (setq! notmuch-mua-user-agent-function nil)
  (setq! notmuch-maildir-use-notmuch-insert t)
  (setq! notmuch-crypto-process-mime t
         notmuch-crypto-get-keys-asynchronously t)

;;;;; Reading Messages

  (setq! notmuch-show-relative-dates t)
  (setq! notmuch-show-all-multipart/alternative-parts nil)
  (setq! notmuch-show-indent-messages-width 1
         notmuch-show-indent-multipart nil)
  (setq! notmuch-show-part-button-default-action #'notmuch-show-view-part) ; orig. `notmuch-show-save-part'
  (setq! notmuch-wash-wrap-lines-length 120)
  (setq! notmuch-unthreaded-show-out nil)
  (setq! notmuch-message-headers '("To" "Cc" "Subject" "Date")
         notmuch-message-headers-visible t)

  ;; Disable buttonisation of long quotes.
  (let ((count most-positive-fixnum))
    (setq! notmuch-wash-citation-lines-prefix count
           notmuch-wash-citation-lines-suffix count)))

;;;;; Keybindings

(after! notmuch
  (define-keymap :keymap notmuch-search-mode-map
    "/" #'notmuch-search-filter
    "r" #'notmuch-search-reply-to-thread
    "R" #'notmuch-search-reply-to-thread-sender)

  (define-keymap :keymap notmuch-show-mode-map
    "r" #'notmuch-search-reply-to-thread
    "R" #'notmuch-search-reply-to-thread-sender))

;; =consult-notmuch= :: Consult interface for Notmuch :consult:package:


(package! consult-notmuch)

;; =notmuch-indicator= :: Display message count in the mode line :modeline:


(package! notmuch-indicator
  (setq! notmuch-indicator-args
         '(( :terms "tag:unread and tag:inbox"
             :label "[U] ")))
  (setq! notmuch-indicator-refresh-count (* 60 3))
  (setq! notmuch-indicator-hide-empty-counters t)
  (setq! notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))

  (notmuch-indicator-mode 1))

(provide 'ceamx-init-email)
