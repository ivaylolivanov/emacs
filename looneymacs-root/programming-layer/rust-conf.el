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
  :hook
  ;; - Configure rust-mode to auto format the code on save
  (setq rust-format-on-save t)
  (setq company-tooltip-align-annotations t))

;;============================

(provide 'rust-conf)
;;; rust-conf.el ends here
