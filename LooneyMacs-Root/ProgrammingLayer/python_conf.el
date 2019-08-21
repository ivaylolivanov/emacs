;;; package --- Python configurations


;;; Commentary:
;; - This file will house the python configurations
;; - Elpy
;; - Py-autopep8



;;; Code:
;; - Enable Elpy
(use-package elpy
  :ensure t
  :init
  (add-hook 'python-mode-hook #'elpy-enable)
  :config
  ;; Flycheck with elpy
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)))



;; - Enable py-autopep8
(use-package py-autopep8
  :ensure t
  :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(provide 'python_conf)
;;; python_conf.el ends here
