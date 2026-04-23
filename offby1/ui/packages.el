(when (modulep! +nyan)
  (package! nyan-mode))

(when (modulep! +org-margin)
  (package! org-margin :recipe (:type git :host github :repo "rougier/org-margin")))

(when (modulep! +popper)
  (package! popper  :recipe (:type git :host github :repo "karthink/popper")))

(when (modulep! +rainbow)
  (package! rainbow-delimiters))

(package! string-inflection)

(package! tokyo-night :recipe (:type git :host github :repo "bbatsov/tokyo-night-emacs"))

(package! warm-burnout
:recipe (:host github :repo "offbyone/warm-burnout" :branch "emacs-burnout" :files ("emacs/*.el")))
