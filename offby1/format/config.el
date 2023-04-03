;;; config.el --- Configure some core behaviour for format-all  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Rose

;; Author: Chris Rose <offbyone@github.com>
;; Keywords: languages,

(after! (format-all inheritenv)
  (inheritenv-add-advice 'format-all--buffer-thunk))
