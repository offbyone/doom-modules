(defun dig-my-grave ()
  "Three consecutive graves (e.g. “`”) at the start of the line prompts for
   inserting content.  See `dig-my-grave/templates-alist/org-mode'."
  (interactive)
  (if (or (and (> (point) 3)
               (string= (buffer-substring-no-properties
                         (- (point) 3) (point)) "\n``"))
          ;; Account for starting on the first line
          (and (= (point) 3)
               (string= (buffer-substring-no-properties
                         (- (point) 2) (point)) "``")))
      ;; We have just hit our third back-tick at the beginning of the line.
      (progn
        (delete-char -2)
        ;; I use the alist-get pattern a lot...perhaps a function?
        (let ((value (alist-get (completing-read "Special Content: "
                                                 dig-my-grave/templates-alist/org-mode nil t)
                                dig-my-grave/templates-alist/org-mode nil nil #'string=)))
          (cond
           ;; Let's assume that we're dealing with registered org blocks.
           ((stringp value)
            (insert value) (forward-line -1) (org-edit-special))
           ;; Trust the function
           ((commandp value) (call-interactively value))
           ((functionp value) (funcall value))
           ((ad-lambda-p) (funcall value))
           ;; Time for a pull request
           (t (error "Unprocessable value %s for #'dig-my-grave" value)))))
    (setq last-command-event ?`)
    (call-interactively #'org-self-insert-command)))

(define-minor-mode dig-my-grave-mode
  "Markdown code templates for other modes with blocks and code (org!)"
  :lighter "`"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "`") #'dig-my-grave) map))

(defvar dig-my-grave/templates-alist/org-mode
  '(("Bash" . "#+begin_src bash :results scalar replace :exports both :tangle yes\n#+end_src")
    ("Details and Summary" . "#+begin_details\n#+begin_summary\n\n#+end_summary\n#+end_details")
    ("Emacs Lisp" . "#+begin_src emacs-lisp\n#+end_src")
    ("Package" . "#+begin_src emacs-lisp :tangle packages.el\n#+end_src")
    ("Org Structure" . org-insert-structure-template)
    ("Plant UML" . "#+begin_src plantuml\n@startuml\n!theme amiga\n\n@enduml\n#+end_src")
    ("Ruby" . "#+begin_src ruby\n#+end_src"))
  "A list of `cons' cells with `car' as the label and `cdr' as
   the value that we'll insert.  Used as the collection for the
   `dig-my-grave' `completing-read'.")

(defun offby1/org-link-url (link)
  "Extract the URL from an org-mode link."
  (if (string-match org-link-bracket-re link)
      (match-string 1 link)
    link))

(defun offby1/org-link-title (link)
  "Extract the title from an org-mode link."
  (if (string-match org-link-bracket-re link)
      (or (match-string 2 link) (match-string 1 link))
    link))

(after! org
  ;; Enable dig-my-grave for easy source blocks
  (add-hook 'org-mode-hook (lambda () (dig-my-grave-mode 1)))

  ;; set up work log org templates. Override any existing ones.
  (setq org-capture-templates
        (cl-remove-if (lambda (template)
                        (member (car template) '("c" "u" "l")))
                      org-capture-templates))

  (cl-pushnew `("l" "Work log entry"
                entry (file+datetree ,(function offby1/get-work-log-target))
                "* %?"
                :empty-lines 0)
              org-capture-templates
              :key #'car :test #'equal)
  (cl-pushnew `("c" "On-Call Work log entry"
                entry (file+datetree ,(function offby1/get-work-log-target))
                "* Triage %?"
                :empty-lines 0)
              org-capture-templates
              :key #'car :test #'equal)

  (cl-pushnew '("u" "URL capture from Chrome" entry
                (file+olp+datetree "~/Dropbox/Notes/org/urls.org" "Inbox")
                "* %(offby1/org-link-title (org-mac-link-chrome-get-frontmost-url))    :chrome:url:
%U
%(offby1/org-link-url (org-mac-link-chrome-get-frontmost-url))

")
              org-capture-templates
              :key #'car :test #'equal))
