;;===========================
;;= Base of the programming =
;;=        settings         =
;;===========================
;; - Automatic indent with <RET>
;; - Yasnippet
;; - GDB
;; - Compile a makefile with <F5>
;; - Flycheck
;; - Lsp-mode



;; - Automatic indent with <RET>
(global-set-key (kbd "RET") 'newline-and-indent)


;; - Yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;; - GDB to use many windows mode
(setq gdb-many-windows t
      gdb-show-main t)


;; - Compile a makefile with <F5>
;; These settings are taken from prelude emacs
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
  (setq indent-tabs-mode t))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))


;; - Enable flycheck globally
(require ' flycheck)
(global-flycheck-mode)



;;==============================
;;= Add and configure lsp-mode =
;;==============================
(add-to-list 'load-path "/home/ivo/.emacs.d/elpa/lsp-mode")
(require 'lsp-mode)

(lsp-define-stdio-client
 lsp-prog-major-mode
 "language-id"
 (lambda () default-directory)
 '("/my/lsp/server" "and" "args"))

(add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

(require 'lsp-imenu)
(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;;==============================



;;======================================
;;= Calls configurations for different =
;;=             languages              =
;;======================================


;;======================================

(provide 'prog_base)
