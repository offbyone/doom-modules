(use-package! smithy-mode)
(use-package! amz-common)
(use-package! amz-brazil-config)
(use-package! ion-mode)
(use-package! amz-workspace
  :config (setq amz-workspace-default-root-directory "~/workspace"))

(add-to-list 'org-agenda-files "~/Amazon WorkDocs Drive/My Documents/Notes/org/")

(use-package! projectile
  :config
  (mapc (lambda (v) (add-to-list 'projectile-project-search-path `(,v . 1)))
  (append
    (file-expand-wildcards "~/workspace/[A-Za-z]*/*/src")
    (file-expand-wildcards "~/workspace/[A-Za-z]*/src"))))

(map! :leader
      :prefix ("v" . "versioning")
      "A" #'amz-browse-source-at-point)

;; a general amazon map
(map! :leader
      :prefix-map ("z". "amazon")
      (:prefix ("b" . "browse")
       :desc "Browse source at point"  "s" #'amz-browse-source-at-point
       :desc "Browse git ref"          "g" #'amz-browse-git-ref-at-point
       :desc "Browse git ref at point" "r" #'amz-browse-git-ref
       :desc "Browse package at point" "p" #'amz-browse-package-at-point)
      (:prefix ("w" . "workspace")
       :desc "Create workspace"        "c" #'amz-workspace-create-workspace
       :desc "Delete workspace"        "d" #'amz-workspace-delete-workspace
       :desc "Add package"             "a" #'amz-workspace-add-package
       :desc "Use version set"         "u" #'amz-workspace-use-version-set
       :desc "Sync workspace"          "s" #'amz-workspace-sync-workspace))

(use-package! smartparens
  :after ion-mode
  :config
  (sp-with-modes 'ion-mode
    (sp-local-pair "{" nil
                   :post-handlers '(
                                    ("||\n[i]" "RET")))))

(use-package! amz-brazil-config
  :after lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(brazil-config-mode . "brazil-config"))
  (lsp-register-client
    (make-lsp-client
      :priority -1
      :new-connection (lsp-stdio-connection "barium")
      :activation-fn (lsp-activate-on "brazil-config")
      :server-id 'barium))
  (add-hook 'brazil-config-mode-hook #'lsp 'append))

(add-hook 'perl-mode-hook 'amzn-deperl-config-h)

;; Brief -- a BRazil Interface for Emacs in ... something "F"
(use-package! amz-brief
  :defer t
  :config
  (setq amz-brief-default-host "chrisros-al2.aka.pdx.corp.amazon.com"
        amz-brief-remote-workplace-dir "~/workspace")

  ;; (amz-brief-add-commands  "~/workplace/AdsDiagnosticsService" "AdsDiagnosticsServiceCDK"
  ;;                      '(("bb deploy:assets adsdiagnosticsservice-dev-DataReportingStack-venkobas")
  ;;                        ("bb cdk deploy adsdiagnosticsservice-dev-DataReportingStack-venkobas")))
  :bind (:map amz-brief-mode-map
         ("C-<tab>" . amz-brief)))

(use-package! xwiki-mode)
