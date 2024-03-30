;;; org-settings --- Houses all org configurations

;;; Commentary:
;;=============
;;= Org setup =
;;=============
;; - Activate org
;; - Add time stamp when To-Do is closed
;; - Specify agenda directories
;; - Enable visual-line-mode within org-mode
;; - Configure org capture
;; - Org roam mode
;; - Keybindings



;;; Code:
;; - Activate org
(require 'org)

;; - Add time stamp when To-Do is closed
(setq org-log-done t)

;; - Specify agenda directories
(setq org-agenda-files (list org-roam-storage-dir))

;; - Enable visual-line-mode within org-mode
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable 'org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; - Org roam mode
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :after org
  :custom
  (org-roam-directory (file-truename org-roam-storage-dir))
  (org-roam-completion-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (org-roam-setup))

;; - Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'org-settings)
;;; org-settings.el ends here
