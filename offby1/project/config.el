(use-package! conner
  :when (modulep! +conner))

;; define a function that runs conner-run-project-command when called, but when called with the universal argument, runs projectile-compile-project instead
(defun offby1/run-project-command (&optional prefix)
  "Run a conner command in the project root. With a prefix argument, run a shell command in the project root."
  (interactive "P")
  (if prefix
      (call-interactively #'projectile-compile-project)
    (call-interactively #'conner-run-project-command)))

(after! (conner projectile)
  ;; rebind ! to conner in projectile-command-map
  (define-key projectile-command-map "!" #'offby1/run-project-command))
