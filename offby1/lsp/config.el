(after! lsp-mode
  (-doto lsp-command-map
    (lsp-define-conditional-key "Ti" lsp-inlay-hints-mode "toggle inlay hints" (lsp-feature? "textDocument/inlayHint")))
