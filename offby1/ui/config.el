;; Overrides for my own help map
(define-key! help-map
  "i"      #'consult-info)


;;; Experimenting with some Embark-isms
;;; Based heavily on [[https://karthinks.com/software/fifteen-ways-to-use-embark/][Fifteen ways to use Embark | Karthinks]]

(eval-when-compile
  (defmacro offby1/embark-ace-action (fn)
    `(defun ,(intern (concat "offby1/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(use-package! embark
  :defer t
  :init
  (map! (:map embark-file-map
         :desc "Find file into window"  "o" (offby1/embark-ace-action find-file)))
  (map! (:map embark-buffer-map
         :desc "Switch to buffer in window"  "o" (offby1/embark-ace-action switch-to-buffer))))

(use-package! nyan-mode
  :if (modulep! +nyan)
  :init (nyan-mode 1))

(use-package! org-margin
  :if (modulep! +org-margin)
  :init (org-margin-mode 1))

;; Spare me the zoomies
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
