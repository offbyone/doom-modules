;;; config.el --- configure ruby for doom for me     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Rose

;; Author: Chris Rose <offbyone@github.com>
;; Keywords: languages

;;; Reconfigure format-all with rubocop

;;; The following two functions are copied wholesale from the more modern
;;; format-all-the-code library, which has evolved far past the one in the
;;; doom distro
(defun +offby1/format-all--ruby-gem-bundled-p (gem-name)
  "Internal helper function to check for a Ruby gem.
Returns t if GEM-NAME is listed in the current project's
Gemfile.lock, nil otherwise."
  (let* ((lockfile "Gemfile.lock")
         (dir (locate-dominating-file (buffer-file-name) lockfile)))
    (and dir
         (with-temp-buffer
           (insert-file-contents (expand-file-name lockfile dir))
           (re-search-forward (format "^    %s " (regexp-quote gem-name))
                              nil t))
         t)))

(defun +offby1/remove-matching-lines (regex-list string)
  (with-temp-buffer
    (insert string)
    (flush-lines "^\\s-*$" (point-min) (point-max))
    (flush-lines (regexp-opt regex-list) (point-min) (point-max))
    (unless (= (point-min) (point-max))
      (buffer-string))))

(defun +offby1/format-all--buffer-thunk-with-cleanup (fn &rest args)
  "wrap format-all-buffer-hard with line filters, and return stderr as nil if it ends up empty"
  (cl-destructuring-bind (output errput) (apply fn args)
    (let ((errout (+offby1/remove-matching-lines '("====================" "C: [Corrected]") errput)))
      (message (format "errout: %s" errout))
      (message (format "errput: %s" errput))
      (list output errout))))


(defun +offby1/format-all--buffer-hard-ruby
    (gem-name ok-statuses error-regexp root-files executable &rest args)
  "Internal helper function to implement ruby based formatters.
GEM-NAME is the name of a Ruby gem required to run EXECUTABLE.
For OK-STATUSES, ERROR-REGEXP, ROOT-FILES, EXECUTABLE and ARGS,
see `format-all--buffer-hard'."
  (let* ((command (file-name-nondirectory executable))
         (error-regexp
          (regexp-opt
           (append
            (if error-regexp (list error-regexp))
            (list
             "Bundler::GemNotFound"
             (concat "bundler: failed to load command: "
                     (regexp-quote command))
             (concat (regexp-opt (list "bundle" (regexp-quote command)))
                     ": command not found")))))
         (command-args
          (append
           (if (+offby1/format-all--ruby-gem-bundled-p gem-name)
               (list "bundle" "exec" command)
             (list executable))
           (format-all--flatten-once args))))
    (format-all--buffer-hard
     ok-statuses error-regexp root-files
     (car command-args)
     (cdr command-args))))


(after! format-all
  (remhash 'ruby-mode format-all--mode-table)
  (remhash 'enh-ruby-mode format-all--mode-table)

                                        ; rubocop _insists_ on dumping "====================" into the stderr. I don't know why.
                                        ; anyway, this lets me clean that shit up when it's the only thing.
  (advice-add #'format-all--buffer-thunk :around #'+offby1/format-all--buffer-thunk-with-cleanup)

  (define-format-all-formatter rubocop
                               (:executable "rubocop")
                               (:install "gem install rubocop:'>=1.4.0'")
                               (:modes ruby-mode enh-ruby-mode)
                               (:format
                                (+offby1/format-all--buffer-hard-ruby
                                 "rubocop" '(0 1) nil nil
                                 executable
                                 "--autocorrect"
                                 "--format" "emacs"
                                 "--stderr"
                                 "--stdin" (or (buffer-file-name) (buffer-name))))))

(defun offby1/project-gemfile (gems &optional gemfile)
  "Check if specific GEMS are present in the Gemfile. IfGEMFILE is not provided, locate the Gemfile in the project root."
  (let ((gemfile (or gemfile (expand-file-name "Gemfile" (doom-project-root))))
        (found-gems nil))
    (when (file-exists-p gemfile)
      (with-temp-buffer
        (insert-file-contents gemfile)
        (dolist (gem (if (listp gems) gems (list gems)))
          (goto-char (point-min))  ; Reset point to beginning of bufferfor each gem
          (when (re-search-forward (format "gem ['\"]%s['\"]" gem) nil t)
            (push gem found-gems))))
      (nreverse found-gems))))

(defun offby1/lsp-sorbet-enabled-for-project (file-name major-mode)
  (and (funcall (lsp-activate-on "ruby") file-name major-mode)
       (offby1/project-gemfile '("sorbet-runtime" "sorbet-typed" "sorbet-sig" "sorbet-static" "sorbet" "sorbet-static-and-runtime"))))

(defun offby1//lsp--configure ()
  (eval '(setf (lsp--client-activation-fn (ht-get lsp-clients 'sorbet-ls))
               #'offby1/lsp-sorbet-enabled-for-project)))

(after! (lsp-sorbet lsp-mode) (offby1//lsp--configure))
