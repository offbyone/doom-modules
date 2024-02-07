(when (modulep! +nyan)
  (package! nyan-mode))

(when (modulep! +org-margin)
  (package! org-margin :recipe (:type git :host github :repo "rougier/org-margin")))
