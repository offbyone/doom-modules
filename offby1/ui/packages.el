(when (modulep! +nyan)
  (package! nyan-mode))

(when (modulep! +org-margin)
  (package! org-margin :recipe (:type git :host github :repo "rougier/org-margin")))

(when (modulep! +popper)
  (package! popper  :recipe (:type git :host github :repo "karthink/popper")))

(when (modulep! +rainbow)
  (package! rainbow-delimiters))
