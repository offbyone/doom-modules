;;; offby1/exp/autoload/chatgpt.el -*- lexical-binding: t; -*-
;;;###if (modulep! +chatgpt)

;;; chatgpt bindings

;;; Modeled on Xe Iaso's chatgpt bindings, with doom conventions
;;; (meaning, no customize, +naming, etc.)


;;; Code:
(setf lexical-binding t)
(eval-when-compile '(require cl))

(require 'request)

(defvar +chatgpt-base-prompt
  '("You are an assistant that helps me with programming. You will return answers and code that helps me program things.")
  "A list of initial prompts to send to ChatGPT to prime the chat")

(defun +chatgpt--create-answer-buffer (suffix)
  "Create a new scratch buffer with the name SUFFIX and switch to it."
  (let ((buffer (get-buffer-create (format "*+chatgpt-%s*" suffix))))
    (switch-to-buffer buffer)
    (markdown-mode)
    buffer))

(defun +chatgpt--chomp (str)
  "Chomp leading and trailing whitespace from STR"
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
    str)

(defun +chatgpt--read-file (fname)
  "Reads FNAME and returns its contents as a string with whitespace trimmed"
  (with-temp-buffer
    (insert-file-contents fname)
    (+chatgpt--chomp (buffer-string))))

(defun +chatgpt--prepare-messages (question)
  (append
   (mapcar (lambda (p) `(("role" . "system") ("content" . ,p))) +chatgpt-base-prompt)
   `((("role" . "user") ("content" . ,question)))))

(defun +chatgpt--get-api-token (&optional username)
  "Retrieve or set a secret in the macOS Keychain for the given service and username.

By default this will use the server api.openai.com and <username>-token as the username"
  (let* ((server "api.openai.com")
         (auth-username (or username (format "%s-token" (user-login-name))))
         (auth-info (nth 0 (auth-source-search :max 1
                                                :host server
                                                :user auth-username))))
    (when auth-info
      (let ((saved-secret (plist-get auth-info :secret)))
        (if (functionp saved-secret)
            (funcall saved-secret)
          saved-secret)))))


(defun +chatgpt--make-request (question mode)
  "Ask ChatGPT a QUESTION with the current MODE as context"
  (+chatgpt--create-answer-buffer mode)
  (insert question)
  (point-max)
  (let* ((req `(("model" . "gpt-3.5-turbo")
                ("messages" . ,(+chatgpt--prepare-messages question))))
         (auth-key (+chatgpt--get-api-token))
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(format "Bearer %s" auth-key)))))
    ;; (insert (json-encode req))
    (request
      "https://api.openai.com/v1/chat/completions"
      :type "POST"
      :data (json-encode req)
      :headers headers
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((choice (aref (alist-get 'choices data) 0))
                         (message (alist-get 'message choice))
                         (content (alist-get 'content message)))
                    (insert (+chatgpt--chomp content))))))
    ))

;;;###autoload
(defun +ask-chatgpt (question)
  "Ask ChatGPT a QUESTION and get the message put into the current buffer"
  (interactive "squestion> ")
  (+chatgpt--make-request (format "%s\n\n" question) "detail"))

;;;###autoload
(defun +ask-chatgpt-with-mode (question)
  "Ask ChatGPT a QUESTION with the current mode as context.

  Putt the response in the current buffer"
  (interactive "squestion>")
  (let* ((editor-mode (string-join (split-string (symbol-name major-mode) "-") " "))
         (prompt (format "%s\nUser is in %s. Only include the code.\n\n" question editor-mode)))
    (+chatgpt--make-request prompt "quick")))

;;;###autoload
(defun +chatgpt-explain-region (beginning end)
  "Ask ChatGPT to explain this region of code from BEGINNING to END."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties beginning end))
         (mode-sp (split-string (symbol-name major-mode) "-"))
         (editor-mode (string-join mode-sp " "))
         (prompt
          (format
           "Explain this code. User is in %s.\n\n```%s\n%s```\n\n"
           editor-mode (car mode-sp) code)))
    (+chatgpt--make-request prompt "explain")))

(provide '+chatgpt)
;;; +chatgpt.el ends here
