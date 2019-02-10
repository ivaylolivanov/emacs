;;; LSP_conf --- Configuration of language service protocol

;;; Commentary:
;; - All the configurations of LSP are stored here.
;; The goal is to be turned on or off easily. Also to
;; to be easier for further modifications



;;; Code:
;;======================
;;= LSP configurations =
;;======================
;; - Lsp-mode; Hook modes to lsp
;; - Company-lsp
;; - Lsp-ui
;; - Emacs-cquery



;;=======
;;= Lsp =
;;=======
(use-package lsp-mode

  :ensure t
  :init (setq lsp-auto-guess-root t)
  :hook
  ((c-mode c++-mode objc-mode python-mode-hook) . lsp)
  :config
  (require 'lsp-clients)
  ;; (add-hook 'python-mode-hook 'lsp)
  )
;;=======



;;===============
;;= Company-lsp =
;;===============
(use-package company-lsp
  :ensure t
  :config
  (require 'company-lsp)
  (push 'company-lsp company-backends)
  (add-hook 'after-init-hook 'global-company-mode))
;;===============



;;==========
;;= Lsp-ui =
;;==========

(use-package lsp-ui
  :ensure t
  :config
  (require 'lsp-ui))

;;==========



;;================
;;= Emacs-cquery =
;;================

(use-package cquery
  :ensure t
  :config
  (setq cquery-executable "/usr/bin/cquery")

  (with-eval-after-load 'projectile
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".cquery")
                projectile-project-root-files-top-down-recurring)))

  (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "misspeak"))
  (setq cquery-sem-highlight-method 'font-lock)

  )

;;================



(provide 'LSP_conf)
;;; LSP_conf.el ends here
