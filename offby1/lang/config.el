(use-package! protobuf-mode)

(use-package! d2-mode
  :mode ("\\.d2\\'" . d2-mode))

(use-package! justl
  :config
  (map! :n "c" 'justl-exec-recipe))

(use-package! caddyfile-mode)

;; huJSON support
(use-package! json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hujson\\'" . json-mode)))

(if (modulep! +go-template)
    (load! "go/go-template-mode.el" nil t)
  (use-package! go-template-mode
    :mode ("\\.tmpl\\'" . go-template-mode)))

(cond (IS-MAC (use-package! osx-plist)))

(use-package! scad-mode
  :when (modulep! +openscad)
  :mode ("\\.scad\\'" . scad-mode)
  :config
  (setq scad-command
        (cond
         (IS-MAC
          (let ((app-binary "/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD"))
            (if (file-exists-p app-binary)
                app-binary
              "openscad")))
         (t "openscad"))))

(use-package! ob-scad
  :when (modulep! +openscad)
  :commands org-babel-execute:scad
  :after org)

(use-package! apheleia
  :config
  ;; Define both formatters
  (add-to-list 'apheleia-formatters '(tofu "tofu" "fmt" "-"))

  ;; Don't set a default in mode-alist since we'll use buffer-local selection
  (setf (alist-get 'terraform-mode apheleia-mode-alist) nil)

  (defun offby1/select-terraform-formatter ()
    "Select tofu or terraform formatter based on executable availability.
Prefers tofu if available, falls back to terraform."
    (setq-local apheleia-formatter
                (cond
                 ((executable-find "tofu") 'tofu)
                 ((executable-find "terraform") 'terraform)
                 (t nil))))

  :hook
  (terraform-mode . offby1/select-terraform-formatter))
