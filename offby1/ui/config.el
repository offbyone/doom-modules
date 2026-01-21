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

(defun offby1/current-timestamp ()
  "Return the current timestamp in the format: 'Modified: 2025-07-29T08:15:57.000000'"
  (concat (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))
          ".000000"))

(defun offby1/insert-current-timestamp ()
  "Insert the current timestamp in the desired format."
  (interactive)
  (insert (offby1/current-timestamp)))

(map! :leader "t t" #'offby1/insert-current-timestamp)

;; Equalize window sizes after splitting or closing windows
(use-package! emacs
  :config
  (defadvice split-window-right (after balance-out-windows activate)
    (balance-windows))
  (defadvice split-window-below (after balance-out-windows activate)
    (balance-windows))
  (defadvice find-file-other-window (after balance-out-windows activate)
    (balance-windows))
  (defadvice delete-window (after balance-out-windows activate)
    (balance-windows)))

;; I'd like to have just the buffer name, then if applicable the project folder.
;; For example when I open my config file it the window will be titled =config.org ●
;; doom= then as soon as I make a change it will become =config.org ◉ doom=.
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; The elide-head mode doesn't support the Apache code blob that is present in
;; Wellington. Rather than always deal with that half-page of crap, I configure
;; it here to hide it.
(use-package! elide-head
  :custom
  (elide-head-headers-to-hide
   '(("is free software[:;] you can redistribute it" . "\\(Boston, MA 0211\\(1-1307\\|0-1301\\), USA\\|If not, see <http://www\\.gnu\\.org/licenses/>\\)\\.")
     ("The Regents of the University of California\\.  All rights reserved\\." . "SUCH DAMAGE\\.")
     ("Permission is hereby granted, free of charge" . "authorization from the X Consortium\\.")
     ("Licensed under the Apache License" . "limitations under the License\\."))))

(defvar offby1/persp-dir-mappings
  '(("~/.doom.d/" . "config")
    ("~/.local/share/chezmoi/" . "config")
    ("~/Dropbox/org/notes/" . "notes")
    ("~/.config/" . "config")
    ("~/Library/CloudStorage/Dropbox/notes/" . "notes")
    ("~/Library/CloudStorage/Dropbox (Personal)/notes/" . "notes"))
  "Alist mapping project directories to perspective names.
Each entry is (DIRECTORY . PERSPECTIVE-NAME) where DIRECTORY is a path
prefix (with ~ expansion) and PERSPECTIVE-NAME is the target perspective.")

(defun offby1/file-matches-persp-dir-p (filepath dir-pattern)
  "Check if FILEPATH starts with DIR-PATTERN after expanding ~."
  (when (and filepath (stringp filepath))
    (let ((expanded-pattern (expand-file-name dir-pattern))
          (expanded-file (expand-file-name filepath))
          (case-fold-search (eq system-type 'darwin)))
      (if case-fold-search
          (string-prefix-p (downcase expanded-pattern) (downcase expanded-file))
        (string-prefix-p expanded-pattern filepath)))))

(defun offby1/switch-to-persp-based-on-file ()
  "Switch to a perspective based on the current buffer's file path."
  (when (and persp-mode
             (not *persp-pretend-switched-off*)
             buffer-file-name)
    (when-let* ((match (cl-find-if
                        (lambda (mapping)
                          (offby1/file-matches-persp-dir-p buffer-file-name (car mapping)))
                        offby1/persp-dir-mappings))
                (persp-name (cdr match)))
      ;; Create the perspective if it doesn't exist, then switch to it
      (let ((persp (persp-add-new persp-name)))
        (when persp
          (persp-switch persp-name)

          ;; ensure the buffer is added to the perspective
          (persp-add-buffer (current-buffer) persp nil nil))))))

(add-hook 'find-file-hook #'offby1/switch-to-persp-based-on-file)
