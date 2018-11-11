;;; lisp_conf.el --- Contains LISP configurations

;;; Commentary:
;; Main LISP configurations
;; - SLIME



;;; Code:
;; - SLIME
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr//bin/sbcl")
(setq slime-contribs '(slime-fancy))


(provide 'lisp_conf)
;;; lisp_conf.el ends here
