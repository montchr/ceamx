;;; config-prog.el --- User options for programming modes  -*- lexical-binding: t;  -*-

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

(defcustom ceamx-lsp-client 'eglot
  "The preferred LSP client."
  :group 'ceamx
  :type '(choice :tag "LSP client" :value eglot
          (const :tag "Eglot [builtin]" eglot)
          (const :tag "LSP-Mode" lsp-mode)))

(defvar ceamx-lsp-mode-cache-dir (file-name-as-directory (concat ceamx-var-dir "lsp")))
(setopt ceamx-lsp-client 'eglot)
(defvar ceamx-eglot-server-configurations-alist '()
  "Alist of language server initialization options as accepted in `eglot-server-programs'.")
;; TODO: defcustom
(defvar ceamx-lsp-server-nix-lang "nix-nil")

(defvar ceamx-lsp-nix-nixd-default-config
  `(:nixpkgs (:expr "import (builtins.getFlake \"/etc/nix/inputs/nixpkgs\") { } ")
    :formatting (:command ["nixfmt"])
    :options (:nixos (:expr ,(format "import (builtins.getFlake \"%s\").%s.\"%s\".options"
                              "/etc/nixos"
                              "nixosConfigurations"
                              (system-name)))
              :home-manager (:expr ,(format "import (builtins.getFlake \"%s\").%s.\"%s@%s\".options"
                                     "/etc/nixos"
                                     "homeConfigurations"
                                     (user-login-name)
                                     (system-name))))))
(defconst ceamx-lang-php-extension-regexp "\\.\\(php\\|phtml\\)\\'"
  "Pattern matching files with PHP syntax.")
;; Available options listed in schema: https://github.com/bmewburn/intelephense-docs/blob/master/installation.md#configuration-options
(defvar ceamx-php-iph-default-stubs
  ["apache" "bcmath" "bz2" "calendar"
   "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant"
   "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase"
   "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli"
   "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql"
   "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode"
   "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets"
   "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg"
   "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc"
   "xmlwriter" "Zend OPcache" "zip" "zlib"])

(defvar ceamx-php-iph-extra-stubs [])

(defvar ceamx-php-iph-stubs
  (seq-concatenate 'vector ceamx-php-iph-default-stubs ceamx-php-iph-extra-stubs))

(provide 'config-prog)
;;; config-prog.el ends here

(defvar xref-ignored-files '()
  "List of files to be ignored by `xref'.")
