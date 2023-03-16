;;;### autoload
(defun amzn-deperl-config-h ()
  (when (or (string= (file-name-nondirectory (buffer-file-name)) "Config")
            (string= (file-name-extension (buffer-file-name)) "cfg"))
              ;; Delete perl header and save
              (brazil-config-mode)
              (when (> (delete-matching-lines "# -\\*-perl-\\*-") 0)
                (save-excursion
                  (goto-char (point-min))
                  (delete-blank-lines))
                (save-buffer))))

