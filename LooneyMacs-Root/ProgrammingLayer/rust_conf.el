;;; rust_conf --- This file houses the rust configurations

;;; Commentary:
;; - Enable code formatting on save
;; - Activate racer when rust mode starts

;;; Code:



(use-package rust-mode
  :ensure t
  :config
  ;; Flycheck-rust
  (use-package flycheck-rust
    :ensure t
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
    (add-hook 'rust-mode-hook #'flycheck-rust-setup))

  ;; - Configure rust-mode to auto format the code on save
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  ;; Activate racer when rust mode starts
  (add-hook 'racer-mode-hook #'company-mode))

(provide 'rust_conf)
;;; rust_conf.el ends here
