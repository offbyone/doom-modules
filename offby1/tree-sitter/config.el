;;; tree-sitter config for me
(defvar offby1/treesit-grammar-sources
  '((css "https://github.com/tree-sitter/tree-sitter-css")
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package! treesit
  :preface
  (defun offby1/setup-install-grammars ()
    "Install tree-sitter grammars if they\'re absent"
    (interactive)
    (dolist (grammar offby1/treesit-grammar-sources)
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping '((toml-mode . toml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (offby1/setup-install-grammars))

;; (python-mode . python-ts-mode)
;; (ruby-mode . ruby-ts-mode)
;; (css-mode . css-ts-mode)
;; (typescript-mode . js-ts-mode)
;; (js-mode . js-ts-mode)
;; (css-mode . css-ts-mode)
;; (yaml-mode . yaml-ts-mode)

(after! (treesit lsp)
  (add-to-list 'lsp-language-id-configuration '(python-ts-mode . "python"))
  (add-to-list 'lsp-language-id-configuration '(ruby-ts-mode . "ruby"))
  (add-to-list 'lsp-language-id-configuration '(js-ts-mode . "typescript"))
  (add-to-list 'lsp-language-id-configuration '(css-ts-mode . "css"))
  (add-to-list 'lsp-language-id-configuration '(js-ts-mode . "javascript"))
  (add-to-list 'lsp-language-id-configuration '(yaml-tsmode . "yaml"))
  (add-to-list 'lsp-language-id-configuration '(toml-ts-mode . "toml")))
