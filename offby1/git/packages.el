(package! git-auto-commit-mode)

(when (modulep! +magit-delta)
  (package! magit-delta))

(when (modulep! +consult)
  (package! consult-gh
    :recipe (:type git :host github :repo "armindarvish/consult-gh")))
