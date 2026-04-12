(when (modulep! +cc)
  (add-hook! '(c-ts-mode-hook c-mode-hook c++-mode-hook) :append #'hide-ifdef-mode))

(after! hideif
  :config
  (setq hide-ifdef-initially t))
