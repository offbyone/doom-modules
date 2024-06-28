(when (modulep! :tools lsp +eglot)
  (package! eglot-hierarchy
    :recipe (:type git :repo "dolmens/eglot-hierarchy" :host github)))
