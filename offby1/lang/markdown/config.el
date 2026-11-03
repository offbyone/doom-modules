;;; config.el -*- lexical-binding: t; -*-

(defun offby1/markdown-ts--enclosing-fenced-block (&optional pos)
  "Return the fenced code block node enclosing POS or point."
  (when-let* ((node (treesit-node-at (or pos (point)) 'markdown)))
    (treesit-parent-until node "\\`fenced_code_block\\'" t)))

(defun offby1/markdown-ts--enclosing-fenced-block-range (&optional pos)
  "Return match-data-style bounds of the fenced block enclosing POS or point."
  (when-let* ((block (offby1/markdown-ts--enclosing-fenced-block pos)))
    (list (treesit-node-start block)
          (treesit-node-end block))))

(defun offby1/markdown-ts--fenced-block-language (&optional pos)
  "Return the language of the fenced code block enclosing POS or point."
  (when-let* ((block (offby1/markdown-ts--enclosing-fenced-block pos))
              (language (treesit-search-subtree block "\\`language\\'")))
    (treesit-node-text language t)))

(defun offby1/markdown-ts-edit-code-block ()
  "Edit a Markdown code block in an indirect buffer under tree-sitter."
  (interactive)
  (cl-letf (((symbol-function 'markdown-get-enclosing-fenced-block-construct)
             #'offby1/markdown-ts--enclosing-fenced-block-range)
            ((symbol-function 'markdown-code-block-lang)
             #'offby1/markdown-ts--fenced-block-language))
    (call-interactively #'markdown-edit-code-block)))

(map! :after markdown-ts-mode
      :map markdown-ts-mode-map
      :localleader
      "'" #'offby1/markdown-ts-edit-code-block)
