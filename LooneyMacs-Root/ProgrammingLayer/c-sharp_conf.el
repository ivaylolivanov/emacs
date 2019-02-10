;;; c-sharp_conf --- This file houses configurations for c#

;;; Commentary:
;; - Omnisharp configurations

;;; Code:
;; - Configure omnisharp
(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
  (add-hook 'csharp-mode-hook #'company-mode)
  (setq omnisharp-server-executable-path "/home/sleepyme/Downloads/AppImages/omnisharp-roslyn/run"))

(provide 'c-sharp_conf)
;;; c-sharp_conf.el ends here
