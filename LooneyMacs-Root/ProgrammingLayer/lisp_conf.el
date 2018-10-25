;;; lisp_conf --- Lisp programming configurations


;;; Commentary:
;; - Most is taken from prelude-emacs
;; - Crux package: start of switch to buffer
;; - Lisp-mode
;;   - Visi-ielm function
;; - Ielm
;; - Elisp-slime-nav
;; - Paredit



;;; Code:

;; - Taken from crux package
;; Invoke FUNCTION if there is no buffer with BUFFER-NAME
(defun crux-start-or-switch-to (function buffer-name)
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))



;;=============
;;= Lisp-mode =
;;=============

(use-package lisp-mode
  :ensure nil
  :config
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;;============



;;========
;;= Ielm =
;;========

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

;;========



;;===================
;;= Elisp-slime-nav =
;;===================

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

;;===================



;;===========
;;= Paredit =
;;===========

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

;;===========



(provide 'lisp_conf)
;;; lisp_conf.el ends here
