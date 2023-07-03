(use-package! git-auto-commit-mode
  :config
  (setq gac-commit-additional-flag "--no-verify"))

(use-package! magit-delta
  :when (modulep! +magit-delta)
  :hook (magit-mode . magit-delta-mode)
  :after magit
  :config
  (setq magit-delta-default-dark-theme "zenburn"))

(use-package! consult-gh
  :when (modulep! +consult)
  :after consult
  :config

  ;;add your main GitHub account (replace "armindarvish" with your user or org)
  (add-to-list 'consult-gh-default-orgs-list "offbyone")

  ;;use "gh org list" to get a list of all your organizations and adds them to default list
  (setq consult-gh-default-orgs-list (append consult-gh-default-orgs-list (remove "" (split-string (consult-gh--command-to-string "org" "list") "\n"))))

  ;; set the default folder for cloning repositories, By default Consult-GH will confirm this before cloning
  (setq consult-gh-default-clone-directory "~/projects"))
