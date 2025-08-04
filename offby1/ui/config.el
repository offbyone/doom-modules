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

(use-package! popper
  :if (modulep! +popper)
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Spare me the zoomies
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(after! embark
  (setq embark-cycle-key "C-;"))

(when (modulep! +rainbow)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode) ; for a specific major mode

  ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)       ; for a group of major modes

  ;; ;; Or enable it in *most* places:
  ;; (add-hook! '(prog-mode-hook
  ;;              text-mode-hook
  ;;              conf-mode-hook)
  ;;            #'rainbow-delimiters-mode)
  )

(use-package! string-inflection
  :defer t)

(map! :leader
      (:prefix "t"
               (:prefix ("s" . "string inflection")
                :desc "Camelize"   "c" #'string-inflection-lower-camel-case
                :desc "Capitalize" "C" #'string-inflection-camel-case
                :desc "Downcase"   "d" #'string-inflection-downcase
                :desc "Upcase"     "u" #'string-inflection-upcase
                :desc "Snake case" "s" #'string-inflection-snake-case)))

(map! :map prog-mode-map
      :localleader
      (:prefix ("s" . "string inflection")
       :desc "Camelize"   "c" #'string-inflection-lower-camel-case
       :desc "Capitalize" "C" #'string-inflection-camel-case
       :desc "Downcase"   "d" #'string-inflection-downcase
       :desc "Upcase"     "u" #'string-inflection-upcase
       :desc "Snake case" "s" #'string-inflection-snake-case))
