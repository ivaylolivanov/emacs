;;; org_settings --- Houses all org configurations

;;; Commentary:
;;=============
;;= Org setup =
;;=============
;; - Activate org
;; - Keybindings
;; - Add time stamp when To-Do is closed
;; - Specify agenda directories



;;; Code:
;; - Activate org
(require 'org)

;; - Keybindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; - Add time stamp when To-Do is closed
(setq org-log-done t)

;; - Specify agenda directories
(setq org-agenda-files (list '"~/Documents/Notes/"))

(provide 'org_settings)
;;; org_settings.el ends here
