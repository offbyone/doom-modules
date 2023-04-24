(use-package! chatgpt-shell
  :if (modulep! +chatgpt)
  :config
  (setq dall-e-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  (setq chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))))

(after! (ob chatgpt-shell)
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup))
