;;; config.el --- Configuration for GitHub           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Rose

;; Author: Chris Rose <offbyone@github.com>
;; Keywords:

(defvar +offby1/project-disable-formatting nil
  "Project level setting to disable formatting")

(defvar +offby1/project-disable-flycheck nil
  "Project level setting to disable checking")

(put '+offby1/project-disable-formatting 'safe-local-variable 'booleanp)

(defadvice! +offby1/projectile-configure-formatting-fabfh (fn)
  :around #'format-all-buffer--from-hook
  (when (or (not (boundp '+offby1/project-disable-formatting)) (not +offby1/project-disable-formatting))
    (funcall fn)))

(defadvice! +offby1/projectile-configure-formatting-f/b (fn)
  :around #'+format/buffer
  (when (or (not (boundp '+offby1/project-disable-formatting)) (not +offby1/project-disable-formatting))
    (funcall fn)))

(use-package! copilot
  :when (modulep! +copilot)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
