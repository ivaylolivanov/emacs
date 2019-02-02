;;; rust_conf --- This file houses the rust configurations

;;; Commentary:
;; - Enable code formatting on save
;; - Activate racer when rust mode starts

;;; Code:
;; - Configure rust-mode to auto format the code on save
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; - Flycheck-rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'rust-mode-hook #'flycheck-rust-setup)

;; - Activate racer when rust mode starts
(require 'rust-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)


(provide 'rust_conf)
;;; rust_conf.el ends here
