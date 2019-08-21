;;; prog_base --- All the base stuff for programming

;;; Commentary:
;; - Main settings for programming



;;; Code:
;;===========================
;;= Base of the programming =
;;=        settings         =
;;===========================
;; - Automatic indent with <RET>; Indent / complete with <TAB>
;; - Show number of line in programming modes
;; - Hippie expand instead dabbrev
;; - Align pretty
;; - GDB
;; - Compile a makefile with <F5>
;; - Projectile
;; - Paren
;; - Rainbow-delimiters
;; - Yasnippet
;; - Company
;; - Flyspell
;; - Flycheck
;; - Call configurations for programming utilities



;; - Automatic indent with <RET>
(global-set-key (kbd "RET") 'newline-and-indent)
;; - Indent or complete with <TAB>
(setq tab-always-indent 'complete)

;; - Show number of line in programming modes
(add-hook 'prog-mode-hook 'linum-mode)


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
(global-set-key (kbd "s-/") #'hippie-expand)


;; - Align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)


;; - GDB to use many windows mode
(setq gdb-many-windows t
      gdb-show-main t)


;; - Compile a makefile with <F5>
;; These settings are taken from prelude Emacs
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;; Setup compilation-mode used by `compile' command
(require 'compile)
(setq compilation-ask-about-save nil          ; Save before compiling
      compilation-always-kill t               ; Kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; Taken from prelude-c.el:48
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq=default indent-tabs-mode nil))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))



;;==============
;;= Projectile =
;;==============
(use-package projectile
  :ensure t

  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))
;;==============



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
  :ensure t)
;;=======================



;;=============
;;= Yasnippet =
;;=============
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
;;=============



;;===========
;;= Company =
;;===========
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))
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
  (add-hook 'after-init-hook #'global-flycheck-mode))
;;============



;;=======================================
;;= Call configurations for programming =
;;=              utilities              =
;;=======================================
(require 'LSP_conf)
(require 'cAndCpp_conf)
(require 'python_conf)
(require 'rust_conf)
(require 'c-sharp_conf)

;;======================================

(provide 'prog_base)
;;; prog_base.el ends here
