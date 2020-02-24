;;; org_settings --- Houses all org configurations

;;; Commentary:
;;=============
;;= Org setup =
;;=============
;; - Activate org
;; - Add time stamp when To-Do is closed
;; - Specify agenda directories
;; - Enable visual-line-mode within org-mode
;; - Keybindings



;;; Code:
;; - Activate org
(require 'org)

;; - Add time stamp when To-Do is closed
(setq org-log-done t)

;; - Specify agenda directories
(setq org-agenda-files (list org-agenda-dir))

;; - Enable visual-line-mode within org-mode
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable 'org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; - Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(provide 'org_settings)
;;; org_settings.el ends here
