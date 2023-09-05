;;; packages.el --- Packages used by my ruby config  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Rose

;; Author: Chris Rose <offline@offby1.net>
(when (modulep! +coverage)
  (package! coverage-mode :recipe (:type git :host github :repo "purcell/coverage-mode")))
