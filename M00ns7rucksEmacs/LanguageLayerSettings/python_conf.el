;;; package --- Python configurations


;;; Commentary:
;; - This file will house the python configurations
;; - Elpy
;; - Use flycheck with Elpy
;; - Py-autopep8



;;; Code:
;; - Enable Elpy
(elpy-enable)


;; - Flycheck with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; - Enable py-autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


(provide 'python_conf)
;;; python_conf.el ends here
