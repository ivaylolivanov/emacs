;;; LSP_conf --- Configuration of language service protocol

;;; Commentary:
;; - All the configurations of LSP are stored here.
;; The goal is to be turned on or off easily. Also to
;; to be easier for further modifications



;;; Code:
;;======================
;;= LSP configurations =
;;======================
;; - Lsp-mode
;; - Company-lsp
;; - Lsp-ui



;;=======
;;= Lsp =
;;=======
(use-package lsp-mode

  :ensure t
  :config

  ;; lsp-imenu everywhere we have LSP
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;; - Example to add language:
  ;; (lsp-define-stdio-client
  ;;  lsp-prog-major-mode
  ;;  "language-id"

  ;; Configure lsp for python (must be in project)
  ;; Lsp-python-enable defined
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))

  ;; Activate when python-mode is activated
  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-python-enable)))



  (defun lsp-set-cfg ()
    (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
      (lsp--set-configuration lsp-cfg)))

  (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

  (defun my-set-projectile-root ()
    (when lsp--cur-workspace
      (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
  (add-hook 'lsp-before-open-hook #'my-set-projectile-root))



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

(provide 'LSP_conf)
;;; LSP_conf.el ends here