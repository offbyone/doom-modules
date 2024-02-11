;;; ui/deft/config.el -*- lexical-binding: t; -*-

(use-package! notdeft
  :config
  (setq notdeft-xapian-program (concat notdeft-xapian-home "notdeft-xapian"))
  (setq notdeft-xapian-program-compile-command-format "g++ -o %s %s -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`")
  (add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent))

