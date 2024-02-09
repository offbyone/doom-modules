;;; config.el --- Configuration for GitHub           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Rose

;; Author: Chris Rose <offbyone@github.com>
;; Keywords:

(defvar +offby1/project-disable-formatting nil
  "Project level setting to disable formatting")

(defvar +offby1/project-disable-flycheck nil
  "Project level setting to disable checking")

(put '+offby1/project-disable-formatting 'safe-local-variable 'booleanp)

(defun +offby1/formatting-enabled-for-project-p ()
  (or (not (boundp '+offby1/project-disable-formatting))
      (not +offby1/project-disable-formatting)))

(defadvice! +offby1/projectile-configure-formatting-fabfh (fn &rest args)
  :around #'format-all-buffer--from-hook
  (if (+offby1/formatting-enabled-for-project-p) (apply fn args)))

(defadvice! +offby1/projectile-configure-formatting-f/b (fn &rest args)
  :around #'+format/buffer
  (if (+offby1/formatting-enabled-for-project-p) (apply fn args)))

(use-package! copilot
  :when (modulep! +copilot)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  ;; this warning is nearly always unhelpful, so let's remove it.
  (setq copilot-indent-offset-warning-disable t)
  ;; having copilot popping up constantly is annoying, so let's make it wait a bit.
  (setq copilot-idle-delay 0.1))
