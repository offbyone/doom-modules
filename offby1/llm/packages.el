;;; packages.el ---
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Rose

;; Author: Chris Rose <offline@offby1.net>
;; Keywords: llm
(package! shell-maker)
(package! acp)
(package! agent-shell)

(if (modulep! +mcp)
    (package! mcp-server
      :recipe (:type git :host github :repo "rhblind/emacs-mcp-server"
               :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))))
