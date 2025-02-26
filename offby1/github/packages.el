(when (modulep! +copilot)
  (package! copilot
    :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
  (package! copilot-chat
    :recipe (:host github :repo "chep/copilot-chat.el")))
