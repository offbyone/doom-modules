;;; ui/deft/config.el -*- lexical-binding: t; -*-

(use-package! notdeft
  :commands (notdeft)
  :config
  (setq notdeft-extension "org")
  (setq notdeft-xapian-program (concat (if (boundp 'notdeft-xapian-home)
                                           (concat notdeft-xapian-home "notdeft-xapian")
                                         ;; we haven't loaded this, so we make our own
                                         (expand-file-name "xapian/" (file-name-directory (file-truename (locate-library "notdeft")))))
                                       "notdeft-xapian"))
  (setq notdeft-xapian-program-compile-command-format "g++ -o %s %s -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`")
  (add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent))

(add-hook! 'doom-after-init-hook
  (map! :leader
        (:prefix-map ("n" . "notes")
         :desc "Notdeft" "d" #'notdeft)))
