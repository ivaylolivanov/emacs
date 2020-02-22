;;; js_conf --- This file houses configurations for c#

;;; Commentary:
;; - js2-mode configurations

;;; Code:
;; - Configure js2-mode
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
)

(provide 'js_conf)
;;; c-sharp_conf.el ends here
