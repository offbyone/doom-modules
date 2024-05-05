;;; caddy-mode.el --- Major mode for editing Caddy configuration files

;; Define the caddy-mode
(define-derived-mode caddy-mode fundamental-mode "Caddy"
  "Major mode for editing Caddy configuration files."
  (setq font-lock-defaults '(caddy-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'caddy-indent-line))

;; Define the syntax highlighting keywords
(defconst caddy-font-lock-keywords
  '(("\\(^[a-zA-Z0-9_\\.-]+\\)" 1 font-lock-function-name-face)
    ("\\(handle_errors\\|reverse_proxy\\|root\\|rewrite\\|file_server\\)" . font-lock-builtin-face)
    ("\\(@[a-zA-Z0-9_]+\\)" . font-lock-variable-name-face)
    ("\\({\\|}\\)" . font-lock-constant-face)
    ("\\(expression\\)" . font-lock-keyword-face)
    ("\\(==\\|>=\\|<\\|&&\\)" . font-lock-operator-face)
    ("\\({[^}]+}\\)" . font-lock-variable-name-face)
    ("\\([0-9]+\\)" . font-lock-number-face)
    ("\\(localhost:[0-9]+\\)" . font-lock-constant-face)))

;; Define the auto-mode-alist entry
;;;###autoload
(add-to-list 'auto-mode-alist '("\\(?:Caddyfile\\|\.caddy\\)\\'" . caddy-mode))

;; Indentation function
(defun caddy-indent-line ()
  "Indent current line as Caddy configuration."
  (interactive)
  (let ((indent-level 4))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (indent-line-to 0)
        (let ((prev-indent (save-excursion
                             (forward-line -1)
                             (current-indentation))))
          (cond
           ((looking-at "^\\s-*}") (indent-line-to (max 0 (- prev-indent indent-level))))
           ((looking-at "^\\s-*\\(handle\\|@[a-zA-Z0-9_]+\\)\\s-*{") (indent-line-to (+ prev-indent indent-level)))
           (t (indent-line-to prev-indent))))))))

(provide 'caddy-mode)
;;; caddy-mode.el ends here
