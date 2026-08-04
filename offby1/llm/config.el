;;; Configure agent-shell
(use-package agent-shell
  :config
  (setq agent-shell-preferred-agent-config (agent-shell-pi-make-agent-config)))

(use-package mcp-server
  :when (modulep! +mcp)
  ;; Defer loading: without a defer keyword use-package would `require' the
  ;; package now (during module config), which pulls in the org-roam tool files
  ;; before `config.el' has set `org-directory'. `:commands' autoloads the entry
  ;; point so the package only loads when the timer actually starts it.
  :commands (mcp-server-start-unix)
  :init
  ;; Start the server after the whole config (incl. `org-directory') is loaded,
  ;; and off the critical path so the UI comes up first.
  (add-hook! 'doom-after-init-hook
    (run-with-idle-timer 1 nil #'mcp-server-start-unix))
  :config
  ;; The Unix-socket server (and each connected client) is a normal Emacs
  ;; process, so it triggers the "active processes exist" prompt on exit.
  ;; Clear their query-on-exit flags so Emacs shuts them down unconditionally.
  (advice-add 'mcp-server-transport-unix--start :after
              (lambda (&rest _)
                (when (processp mcp-server-transport-unix--server-process)
                  (set-process-query-on-exit-flag
                   mcp-server-transport-unix--server-process nil))))
  (advice-add 'mcp-server-transport-unix--handle-new-connection :filter-return
              (lambda (client-id)
                (let ((process (car (mcp-server-transport-unix--get-client client-id))))
                  (when (processp process)
                    (set-process-query-on-exit-flag process nil)))
                client-id)))
