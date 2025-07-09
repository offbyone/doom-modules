;; (use-package! jujutsu)

(use-package! project
  :config
  (add-to-list 'project-vc-extra-root-markers ".jj"))

(use-package! vc-jj)
(use-package! jujutsushi)

;; (after! git-commit
;;   (setq git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\|jjdescription\\)\\'"))
