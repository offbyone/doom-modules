;; -*- no-byte-compile: t; -*-
;;; tools/chezmoi/packages.el

(package! chezmoi
  :recipe (:host github
           :repo "offbyone/chezmoi.el"
           :branch "main"
           :files ("*.el"
                   "extensions/*.el")))
