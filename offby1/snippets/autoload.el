;;; ui/tabs/autoload.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor snippets)

;;;###autoload
(defun +offby1/snippets-initialize ()
  (let ((module-snippets-path (doom-module-expand-path '(:offby1 . snippets) "snippets")))
    (add-to-list 'yas-snippet-dirs module-snippets-path)
    (yas-load-directory module-snippets-path t)))

;;;###autoload
(eval-after-load 'yasnippet
  (lambda () (+offby1/snippets-initialize)))
