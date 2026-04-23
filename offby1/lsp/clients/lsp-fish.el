;;; lsp-fish.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, fish, shell-script

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

;; LSP Clients for the Fish Programming Language

;;; Code:

(require 'lsp-mode)

;;; Fish
(defgroup lsp-fish nil
  "Settings for the Fish Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/ndonfris/fish-lsp")
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fish-allowed-shells '(fish)
  "List of allowed `sh-shell` values that LSP will be enabled for."
  :type '(list symbol)
  :group 'lsp-fish
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-fish-explainshell-endpoint nil
  "The endpoint to use explainshell.com to answer `onHover' queries.
See instructions at https://marketplace.visualstudio.com/items?itemName=mads-hartmann.bash-ide-vscode"
  :type 'string
  :risky t
  :group 'lsp-fish
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fish-highlight-parsing-errors nil
  "Consider parsing errors in scripts as `problems'."
  :type 'boolean
  :group 'lsp-fish
  :package-version '(lsp-mode . "6.2"))

(defcustom lsp-fish-glob-pattern nil
  "Glob pattern used to find shell script files to parse."
  :type 'string
  :group 'lsp-fish
  :package-version '(lsp-mode . "6.3"))

(defun lsp-fish--fish-lsp-server-command ()
  "Startup command for Fish language server."
  (list (lsp-package-path 'fish-language-server) "start"))

(lsp-dependency 'fish-language-server
                '(:system "fish-lsp")
                '(:npm :package "fish-lsp"
                  :path "fish-lsp"))

(defvar sh-shell)

(defun lsp-fish-check-sh-shell (&rest _)
  "Check whether `sh-shell' is supported.

This prevents the Fish server from being turned on for unsupported dialects, e.g. `zsh`."
  (and (local-variable-p 'sh-shell)
       (memq sh-shell lsp-fish-allowed-shells)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-fish--fish-lsp-server-command)
  :major-modes '(sh-mode)
  :priority -1
  :activation-fn #'lsp-fish-check-sh-shell
  :environment-fn (lambda ()
                    '(("EXPLAINSHELL_ENDPOINT" . lsp-fish-explainshell-endpoint)
                      ("HIGHLIGHT_PARSING_ERRORS" . lsp-fish-highlight-parsing-errors)
                      ("GLOB_PATTERN" . lsp-fish-glob-pattern)))
  :server-id 'fish-lsp
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'fish-lsp callback error-callback))))

(lsp-consistency-check lsp-fish)

(provide 'lsp-fish)
;;; lsp-fish.el ends here
