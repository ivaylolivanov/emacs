;;; package --- Summary
;; My rust config

;;; rust-conf --- This file houses the rust configurations

;;; Commentary:
;; - Rust-mode configuration

;;; Code:
;;============================
;;= Rust-mode configurations =
;;============================

(use-package rust-mode
  :ensure t

  :config
  (setq rust-format-on-save t)
  (setq company-tooltip-align-annotations t)

  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

;;============================

(provide 'rust-conf)
;;; rust-conf.el ends here
