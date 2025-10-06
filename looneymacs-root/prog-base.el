;;; prog-base --- All the base stuff for programming

;;; Commentary:
;; - Main settings for programming



;;; Code:
;;===========================
;;= Base of the programming =
;;=        settings         =
;;===========================
;; - Automatic indent with <RET>; Indent / complete with <TAB>
;; - Use subword mode in programming modes
;; - Hippie expand instead dabbrev
;; - Align pretty
;; - GDB
;; - Projectile
;; - Markdown-mode
;; - Magit
;; - Paren
;; - Rainbow-delimiters
;; - Whitespace mode
;; - Yasnippet
;; - Company
;; - Flyspell
;; - Flycheck
;; - Import build-project.el
;; - Programming mode tags
;; - Call configurations for programming utilities


;; - Automatic indent with <RET>
(global-set-key (kbd "RET") 'newline-and-indent)
;; - Indent or complete with <TAB>
(setq-default indent-tabs-mode nil)

;; - Use subword mode in programming modes
(add-hook 'prog-mode-hook 'subword-mode)

;; - Use hippie expand instead dabbrev
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(global-set-key (kbd "M-/") #'hippie-expand)

;; - Align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)


;; - GDB to use many windows mode
(setq gdb-many-windows t
      gdb-show-main t)



;;==============
;;= Projectile =
;;==============
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))
;;==============



;; =================
;; = Markdown-mode =
;; =================
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))
;; =================



;;=========
;;= Magit =
;;=========
(use-package magit
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-refresh-status-buffer nil))
;;=========



;;=========
;;= Paren =
;;=========
(use-package paren
  :config
  (show-paren-mode +1))
;;=========



;;=======================
;;= Raindbow-delimiters =
;;=======================
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;;=======================


;; ===================
;; = Whitespace mode =
;; ===================
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
;; ===================



;;=============
;;= Yasnippet =
;;=============
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))
;;=============



;;===========
;;= Company =
;;===========
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.0)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (add-hook 'after-init-hook 'global-company-mode))
;;===========



;;============
;;= Flyspell =
;;============
(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))
;;============



;;============
;;= Flycheck =
;;============
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-indication-mode 'nil)
  (add-hook 'after-init-hook #'global-flycheck-mode))
;;============



;; =========================
;; = Programming mode tags =
;; =========================
(make-face 'font-lock-todo-face)
(make-face 'font-lock-done-face)
(make-face 'font-lock-warn-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-prog-face)
(modify-face 'font-lock-todo-face "Dark Orange" nil nil t t t nil nil)
(modify-face 'font-lock-done-face "Dark Green"  nil nil t t t nil nil)
(modify-face 'font-lock-note-face "Dark Gray"   nil nil t t t nil nil)
(modify-face 'font-lock-warn-face "Salmon"      nil nil t t t nil nil)
(modify-face 'font-lock-prog-face "Goldenrod"   nil nil t t t nil nil)


(defun highlight-tags()
  "Highlight the specified comment tags."
  (font-lock-add-keywords
   nil
   '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
     ("\\<\\(PROG\\)" 1 'font-lock-prog-face t)
     ("\\<\\(DONE\\)" 1 'font-lock-done-face t)
     ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
     ("\\<\\(WARN\\)" 1 'font-lock-warn-face t))))

(add-hook 'prog-mode-hook 'highlight-tags)
;; =========================



;;===========================
;;= Import build-project.el =
;;===========================
(require 'build-project)
(require 'name-convention-cycle)


;;=======================================
;;= Call configurations for programming =
;;=              utilities              =
;;=======================================
(require 'eglot-conf)
(require 'cpp-conf)
(require 'cs-conf)
(require 'rust-conf)

;;======================================

(provide 'prog-base)
;;; prog-base.el ends here
