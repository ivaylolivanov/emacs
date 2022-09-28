;;; LSP_conf --- Configuration of language service protocol

;;; Commentary:
;; - All the configurations of LSP are stored here.
;; The goal is to be turned on or off easily. Also to
;; to be easier for further modifications



;;; Code:
;;======================
;;= LSP configurations =
;;======================
;; - Lsp-mode; Hook modes to lsp
;; - Lsp-ui



;;=======
;;= Lsp =
;;=======
(use-package lsp-mode
  :ensure t
  :init (setq lsp-auto-guess-root t)
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode-hook . lsp)
	 (js2-mode-hook . lsp)
	 (csharp-mode . lsp-deferred))
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-auto-guess-root t))
;;=======


;;==========
;;= Lsp-ui =
;;==========

(use-package lsp-ui
  :ensure t
  :config
  (require 'lsp-ui)
  (setq lsp-ui-doc-enable nil))

;;==========

(provide 'LSP_conf)
;;; LSP_conf.el ends here
