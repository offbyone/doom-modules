(after! lsp-mode
  (-doto lsp-command-map
    (lsp-define-conditional-key "Ti" lsp-inlay-hints-mode "toggle inlay hints" (lsp-feature? "textDocument/inlayHint"))))

(use-package! lsp
  :when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
  :hook (prog-mode . lsp-headerline-breadcrumb-mode))

(use-package! eglot-hierarchy
  :when (modulep! :tools lsp +eglot))
