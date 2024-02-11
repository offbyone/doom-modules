;; -*- no-byte-compile: t; -*-
;;; ui/deft/packages.el

(package! notdeft
  :recipe (:type git :host github :repo "hasu/notdeft"
           :files ("*.el" "xapian")))
