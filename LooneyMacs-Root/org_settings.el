;;; org_settings --- Houses all org configurations

;;; Commentary:
;;=============
;;= Org setup =
;;=============
;; - Activate org
;; - Keybindings
;; - Add time stamp when To-Do is closed
;; - Specify agenda directories
;; - Enable visual-line-mode within org-mode



;;; Code:
;; - Activate org
(require 'org)

;; - Keybindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; - Add time stamp when To-Do is closed
(setq org-log-done t)

;; - Specify agenda directories
(setq org-agenda-files (list '"/home/sleepyme/Documents/GTD/"))

;; - Enable visual-line-mode within org-mode
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable 'org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

(provide 'org_settings)
;;; org_settings.el ends here
