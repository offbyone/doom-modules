;; All Amazon packages from EmacsAmazonLibs take the same shape, so here's an
;; easy macro for them.

(defmacro amazon-package! (name)
  `(package! ,name
     :recipe (:type git
              :build t
              :protocol ssh
              :host nil
              :post-build (copy-file "lisp/brazil-path-cache-artifacts" (straight--build-dir ,(symbol-name name)))
              :repo "ssh://git.amazon.com:2222/pkg/EmacsAmazonLibs")))

(amazon-package! smithy-mode)
(amazon-package! amz-common)
(amazon-package! amz-brazil-config)
(amazon-package! ion-mode)
(amazon-package! amz-workspace)

(package! amz-brief :recipe
  (:type git :host nil
  :repo "ssh://git.amazon.com:2222/pkg/Brief"))

(package! xwiki-mode
  :recipe (:host github :repo "ackerleytng/xwiki-mode"))
