(use-package! protobuf-mode)

(use-package! d2-mode
  :mode ("\\.d2\\'" . d2-mode))

(use-package! justl
  :config
  (map! :n "c" 'justl-exec-recipe))

(use-package! caddyfile-mode)
