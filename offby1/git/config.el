(use-package! git-auto-commit-mode
  :config
  (setq gac-commit-additional-flag "--no-verify"))

(use-package! magit-delta
  :when (modulep! +magit-delta)
  :hook (magit-mode . magit-delta-mode)
  :after magit
  :config
  (setq magit-delta-default-dark-theme "zenburn"))
