(use-package! chatgpt-shell
  :if (modulep! +chatgpt)
  :config
  (setq dall-e-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  (setq chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  (setq chatgpt-shell-anthropic-key (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))

  (add-to-list 'doom-detect-indentation-excluded-modes 'chatgpt-shell-mode))

(use-package! ob-chatgpt-shell
  :if (modulep! +ob-chatgpt)
  :config
  (ob-chatgpt-shell-setup))
