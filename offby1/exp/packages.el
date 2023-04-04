(when (modulep! +chatgpt)
  (package! chatgpt-shell :recipe (:type git :host github :repo "xenodium/chatgpt-shell"))
  (package! markdown-mode)
  (package! request))
