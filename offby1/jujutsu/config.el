;; (use-package! jujutsu)

(use-package! project
  :config
  (add-to-list 'project-vc-extra-root-markers ".jj"))

(use-package! vc-jj)
(use-package! jujutsushi)

(after! git-commit
  (setq git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\|jjdescription\\)\\'"))

;; Fix log-edit-done for jujutsu description files
(defun +jujutsu-log-edit-done ()
  "Finish editing jujutsu description and exit emacsclient."
  (interactive)
  (save-buffer)
  (server-edit))

(add-hook! 'log-edit-mode-hook
  (when (and (buffer-file-name)
             (string-match-p "jjdescription" (buffer-file-name)))
    (setq-local log-edit-callback '+jujutsu-log-edit-done)))
