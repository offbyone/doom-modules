;;; lang/ruby/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun offby1//lsp--configure ()
  (setf (lsp--client-activation-fn (ht-get lsp-clients 'sorbet-ls))
        #'offby1/lsp-sorbet-enabled-for-project))
