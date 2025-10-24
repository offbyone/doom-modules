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

(use-package! gptel
  :if (modulep! :tools llm)
  :config
  (setq gptel-model 'claude-3.7-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot")))

(use-package! elisp-dev-mcp
  :if (modulep! +mcp))

(after! (elisp-dev-mcp straight)
  (setq elisp-dev-mcp-additional-allowed-dirs
        (list (straight--build-dir)
              (straight--repos-dir))))
