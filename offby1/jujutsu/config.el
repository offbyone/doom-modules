;; (use-package! jujutsu)

(use-package! project
  :config
  (add-to-list 'project-vc-extra-root-markers ".jj"))

(use-package! vc-jj)
;; this fails on load due to `(void-variable jujutsushi-dispatch)` in jujutsushi.el
;; (use-package! jujutsushi)

(after! git-commit
  (setq git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\|jjdescription\\)\\'"))

;; Fix log-edit-done for jujutsu description files
(defun +jujutsu-log-edit-done ()
  "Finish editing jujutsu description and exit emacsclient."
  (interactive)
  (save-buffer)
  (server-edit))

(add-hook! 'log-edit-mode-hook
  (when (and (buffer-file-name)
             (string-match-p "jjdescription" (buffer-file-name)))
    (setq-local log-edit-callback '+jujutsu-log-edit-done)))

;; Helper function to detect VCS backend
(defun offby1/current-vcs-backend ()
  "Return 'JJ for jujutsu, 'Git for git, or nil."
  (when (buffer-file-name)
    (vc-backend (buffer-file-name))))

;; Smart VCS Adapters - work with both jujutsu and git

(defun offby1/smart-vcs-status ()
  "Open jj-log for jujutsu repositories, magit-status for git repositories."
  (interactive)
  (let ((backend (offby1/current-vcs-backend)))
    (pcase backend
      ('JJ (call-interactively #'jj-log))
      ('Git (call-interactively #'magit-status))
      (_ (call-interactively #'magit-status)))))

(defun offby1/smart-vcs-log-buffer-file ()
  "Show log for current file, adapting to VCS backend."
  (interactive)
  (let ((backend (offby1/current-vcs-backend)))
    (pcase backend
      ('JJ (if (fboundp 'vc-print-log)
               (call-interactively #'vc-print-log)
             (call-interactively #'jj-log)))
      ('Git (call-interactively #'magit-log-buffer-file))
      (_ (when (fboundp 'vc-print-log)
           (call-interactively #'vc-print-log))))))

(defun offby1/smart-vcs-fetch ()
  "Fetch from remote, adapting to VCS backend."
  (interactive)
  (let ((backend (offby1/current-vcs-backend)))
    (pcase backend
      ('JJ (call-interactively #'jj-git-fetch))
      ('Git (call-interactively #'magit-fetch))
      (_ (call-interactively #'magit-fetch)))))

;; Keybindings for smart VCS adapters
(map! :map doom-leader-versioning-map
      "g" #'offby1/smart-vcs-status
      "L" #'offby1/smart-vcs-log-buffer-file
      "F" #'offby1/smart-vcs-fetch)

;; jj-specific operations under C-c v j prefix
(map! :map doom-leader-versioning-map
      :prefix ("j" . "jujutsu")
      "s" #'jj-squash-transient
      "b" #'jj-bookmark-transient
      "n" #'jj-new-transient
      "r" #'jj-rebase-transient
      "l" #'jj-bookmark-list)
