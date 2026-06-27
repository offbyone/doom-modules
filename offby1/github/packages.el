(when (modulep! +copilot)
  (package! copilot
    :recipe (:host github 
             :repo "copilot-emacs/copilot.el" :files ("*.el"))))

(when (modulep! +copilot-chat)
  (package! copilot-chat
    :recipe (:host github :repo "chep/copilot-chat.el")))
